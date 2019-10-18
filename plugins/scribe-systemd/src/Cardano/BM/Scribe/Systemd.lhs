\subsection{Cardano.BM.Scribe.Systemd}
\label{code:Cardano.BM.Scribe.Systemd}

%if style == newcode

\begin{code}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

#if defined(linux_HOST_OS)
#define LINUX
#endif

module Cardano.BM.Scribe.Systemd
    (
#if defined(LINUX)
      plugin
#endif
    ) where

#ifdef LINUX
import           Control.Monad (when)
import           Data.Aeson (ToJSON, FromJSON, encode)
import qualified Data.ByteString.Lazy as BL (toStrict)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Builder (toLazyText)
import           Language.Haskell.TH.Syntax (Loc(Loc, loc_filename, loc_start))
import           Systemd.Journal (JournalFields, codeFile, codeLine, message,
                     mkJournalField, priority, sendJournalFields,
                     syslogFacility, syslogIdentifier)
import qualified Systemd.Journal as J
import           System.Posix.Syslog (Facility)
import qualified Katip as K
import qualified Katip.Core as KC
import           Katip.Format.Time (formatAsIso8601)
import           Katip.Scribes.Handle (brackets)

import           Cardano.BM.Configuration
import           Cardano.BM.Backend.Log (sev2klog)
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Output (ScribeId)
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.Trace
import           Cardano.BM.Plugin (Plugin (..))
#endif

\end{code}
%endif

This plugin provides a scribe to \emph{katip} to output logged items
to systemd's journal on \emph{Linux}.

\subsubsection{Plugin definition}
\begin{code}
#ifdef LINUX
plugin :: (IsEffectuator s a, ToJSON a, FromJSON a)
       => Configuration -> Trace IO a -> s a -> IO (Plugin a)
plugin config trace sb =
    ScribePlugin
               <$> mkJournalScribe
               <*> pure "JournalSK"
#endif

\end{code}

\subsubsection{Scribe definition}
\begin{code}
#ifdef LINUX
mkJournalScribe :: IO K.Scribe
mkJournalScribe = return $ journalScribe Nothing (sev2klog Debug) K.V3

-- taken from https://github.com/haskell-service/katip-libsystemd-journal
journalScribe :: Maybe Facility
              -> K.Severity
              -> K.Verbosity
              -> K.Scribe
journalScribe facility severity verbosity = K.Scribe liPush scribeFinalizer (pure . const True)
  where
    liPush :: K.LogItem a => K.Item a -> IO ()
    liPush i = do
        permit <- K.permitItem severity i
        when permit $
            sendJournalFields $ itemToJournalFields facility verbosity i

    scribeFinalizer :: IO ()
    scribeFinalizer = pure ()
#endif

\end{code}

\subsubsection{Conversion utilities}
Converts a |Katip Item| into a libsystemd-journal |JournalFields| map.

\begin{code}
#ifdef LINUX
itemToJournalFields :: K.LogItem a
                    => Maybe Facility
                    -> K.Verbosity
                    -> K.Item a
                    -> JournalFields
itemToJournalFields facility verbosity item = mconcat [ defaultFields item
                                                      , maybe HM.empty facilityFields facility
                                                      , maybe HM.empty locFields (K._itemLoc item)
                                                      ]
  where
    defaultFields kItem =
        mconcat [ message (TL.toStrict $ toLazyText $ KC.unLogStr (KC._itemMessage kItem))
                , priority (mapSeverity (KC._itemSeverity kItem))
                , syslogIdentifier (unNS (KC._itemApp kItem))
                , HM.fromList [ (environment, T.encodeUtf8 $ KC.getEnvironment (KC._itemEnv kItem))
                              , (namespace, T.encodeUtf8 $ unNS (KC._itemNamespace kItem))
                              , (payload, BL.toStrict $ encode $ KC.payloadObject verbosity (KC._itemPayload kItem))
                              , (thread, T.encodeUtf8 $ KC.getThreadIdText (KC._itemThread kItem))
                              , (time, T.encodeUtf8 $ formatAsIso8601 (KC._itemTime kItem))
                              ]
                ]
    facilityFields = syslogFacility
    locFields Loc{..} = mconcat [ codeFile loc_filename
                                , codeLine (fst loc_start)
                                ]

    environment = mkJournalField "environment"
    namespace = mkJournalField "namespace"
    payload = mkJournalField "payload"
    thread = mkJournalField "thread"
    time = mkJournalField "time"

    unNS ns = case K.unNamespace ns of
        []  -> T.empty
        [p] -> p
        parts -> T.intercalate "." parts

    mapSeverity s = case s of
        K.DebugS     -> J.Debug
        K.InfoS      -> J.Info
        K.NoticeS    -> J.Notice
        K.WarningS   -> J.Warning
        K.ErrorS     -> J.Error
        K.CriticalS  -> J.Critical
        K.AlertS     -> J.Alert
        K.EmergencyS -> J.Emergency
#endif

\end{code}
