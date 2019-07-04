
\subsection{Cardano.BM.Data.LogItem}
\label{code:Cardano.BM.Data.LogItem}

%if style == newcode
\begin{code}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.BM.Data.LogItem
  ( LogObject (..)
  , loType
  , loType2Name
  , LOMeta (..), mkLOMeta
  , LOContent (..)
  , CommandValue (..)
  , LoggerName
  , MonitorAction (..)
  , PrivacyAnnotation (..)
  , PrivacyAndSeverityAnnotated (..)
  , utc2ns
  , mapLogObject
  , mapLOContent
  )
  where

import           Control.Concurrent (myThreadId)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson (FromJSON, ToJSON, object, toJSON, (.=))
import           Data.Text (Text, pack)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Data.Time.Clock (UTCTime (..), getCurrentTime)
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import           Cardano.BM.Data.Aggregated (Aggregated (..), Measurable (..))
import           Cardano.BM.Data.BackendKind
import           Cardano.BM.Data.Counter
import           Cardano.BM.Data.Severity

\end{code}
%endif

\subsubsection{LoggerName}\label{code:LoggerName}\index{LoggerName}
A |LoggerName| has currently type |Text|.
\begin{code}
type LoggerName = Text

\end{code}

\subsubsection{Logging of outcomes with |LogObject|}
\label{code:LogObject}\index{LogObject}
\label{code:LOMeta}\index{LOMeta}
\label{code:LOContent}\index{LOContent}

\begin{code}
data LogObject a = LogObject
                     { loName    :: LoggerName
                     , loMeta    :: !LOMeta
                     , loContent :: (LOContent a)
                     }
                     deriving (Generic, Show, ToJSON, FromJSON)

\end{code}

\label{code:mkLOMeta}\index{mkLOMeta}
Meta data for a |LogObject|.
Text was selected over ThreadId in order to be able to use the logging system
under SimM of ouroboros-network because ThreadId from Control.Concurrent lacks a Read
instance.
\begin{code}
data LOMeta = LOMeta {
                  tstamp   :: {-# UNPACK #-} !UTCTime
                , tid      :: {-# UNPACK #-} !Text
                , severity :: !Severity
                , privacy  :: !PrivacyAnnotation
                }
                deriving (Show, Generic, FromJSON)

instance ToJSON LOMeta where
    toJSON (LOMeta _tstamp _tid _sev _priv) =
        object [ "tstamp"   .= _tstamp
               , "tid"      .= show _tid
               , "severity" .= show _sev
               , "privacy"  .= show _priv
               ]

mkLOMeta :: MonadIO m => Severity -> PrivacyAnnotation -> m LOMeta
mkLOMeta sev priv =
    LOMeta <$> (liftIO getCurrentTime)
           <*> (pack . show <$> (liftIO myThreadId))
           <*> pure sev
           <*> pure priv

\end{code}

Convert a timestamp to ns since epoch:\label{code:utc2ns}\index{utc2ns}
\begin{code}
utc2ns :: UTCTime -> Word64
utc2ns utctime = fromInteger $ round $ 1000 * 1000 * 1000 * (utcTimeToPOSIXSeconds utctime)

\end{code}

\begin{code}
data MonitorAction = MonitorAlert Text
                   | MonitorAlterGlobalSeverity Severity
                   | MonitorAlterSeverity LoggerName Severity
                   deriving (Generic, Show, ToJSON, FromJSON)
\end{code}

\label{code:LogMessage}\index{LogMessage}
\label{code:LogError}\index{LogError}
\label{code:LogValue}\index{LogValue}
\label{code:ObserveOpen}\index{ObserveOpen}
\label{code:ObserveDiff}\index{ObserveDiff}
\label{code:ObserveClose}\index{ObserveClose}
\label{code:AggregatedMessage}\index{AggregatedMessage}
\label{code:MonitoringEffect}\index{MonitoringEffect}
\label{code:Command}\index{Command}
\label{code:KillPill}\index{KillPill}
Payload of a |LogObject|:
\begin{code}
data LOContent a = LogMessage a
                 | LogError Text
                 | LogValue Text Measurable
                 | ObserveOpen CounterState
                 | ObserveDiff CounterState
                 | ObserveClose CounterState
                 | AggregatedMessage [(Text, Aggregated)]
                 | MonitoringEffect MonitorAction
                 | Command CommandValue
                 | KillPill
                   deriving (Generic, Show, ToJSON, FromJSON)

loType :: LogObject a -> Text
loType (LogObject _ _ content) = loType2Name content

\end{code}

Name of a message content type
\begin{code}
loType2Name :: LOContent a -> Text
loType2Name = \case
    LogMessage _        -> "LogMessage"
    LogError _          -> "LogError"
    LogValue _ _        -> "LogValue"
    ObserveOpen _       -> "ObserveOpen"
    ObserveDiff _       -> "ObserveDiff"
    ObserveClose _      -> "ObserveClose"
    AggregatedMessage _ -> "AggregatedMessage"
    MonitoringEffect _  -> "MonitoringEffect"
    Command _           -> "Command"
    KillPill            -> "KillPill"

\end{code}

\label{code:CommandValue}\index{CommandValue}
Backends can enter commands to the trace. Commands will end up in the
|Switchboard|, which will interpret them and take action.
\begin{code}
data CommandValue = DumpBufferedTo BackendKind
                    deriving (Generic, Show, ToJSON, FromJSON)

\end{code}

\subsubsection{Privacy annotation}
\label{code:PrivacyAnnotation}\index{PrivacyAnnotation}
\label{code:Confidential}\index{PrivacyAnnotation!Confidential}
\label{code:Public}\index{PrivacyAnnotation!Public}
\begin{code}
data PrivacyAnnotation =
      Confidential -- confidential information - handle with care
    | Public       -- indifferent - can be public.
    deriving (Show, Generic, ToJSON, FromJSON)

\end{code}

Data structure for annotating the severity and privacy of an object.
\begin{code}
data PrivacyAndSeverityAnnotated a
            = PSA { psaSeverity :: !Severity
                  , psaPrivacy  :: !PrivacyAnnotation
                  , psaPayload  :: a
                  }
              deriving (Show)

\end{code}

\subsubsection{Mapping Log Objects}
\label{code:mapLogObject}\index{mapLogObject}
\label{code:mapLOContent}\index{mapLOContent}

This provides a helper function to transform log items. It would often
be used with |contramap|.

\begin{code}
mapLogObject :: (a -> b) -> LogObject a -> LogObject b
mapLogObject f (LogObject nm me loc) = LogObject nm me (mapLOContent f loc)

instance Functor LogObject where
  fmap = mapLogObject

mapLOContent :: (a -> b) -> LOContent a -> LOContent b
mapLOContent f = \case
    LogMessage msg       -> LogMessage (f msg)
    LogError a           -> LogError a
    LogValue a n         -> LogValue a n
    ObserveOpen st       -> ObserveOpen st
    ObserveDiff st       -> ObserveDiff st
    ObserveClose st      -> ObserveClose st
    AggregatedMessage ag -> AggregatedMessage ag
    MonitoringEffect act -> MonitoringEffect act
    Command v            -> Command v
    KillPill             -> KillPill

\end{code}
