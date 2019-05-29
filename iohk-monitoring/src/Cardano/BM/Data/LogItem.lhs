
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
  , LOMeta (..), mkLOMeta
  , LOContent (..)
  , CommandValue (..)
  , LoggerName
  , MonitorAction (..)
  , PrivacyAnnotation (..)
  , PrivacyAndSeverityAnnotated (..)
  )
  where

import           Control.Concurrent (myThreadId)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson (FromJSON, ToJSON, object, toJSON, (.=))
import           Data.Text (Text, pack)
import           Data.Time.Clock (UTCTime, getCurrentTime)
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
                     deriving (Generic, Show, ToJSON)

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
                deriving (Show)

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

\begin{code}
data MonitorAction = MonitorAlert Text
                   | MonitorAlterGlobalSeverity Severity
                   | MonitorAlterSeverity LoggerName Severity
                   deriving (Generic, Show, ToJSON)
\end{code}

\label{code:LogMessage}\index{LogMessage}
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
                 | LogValue Text Measurable
                 | ObserveOpen CounterState
                 | ObserveDiff CounterState
                 | ObserveClose CounterState
                 | AggregatedMessage [(Text, Aggregated)]
                 | MonitoringEffect MonitorAction
                 | Command CommandValue
                 | KillPill
                   deriving (Generic, Show, ToJSON)

loType :: LogObject a -> Text
loType = \case
    LogObject _ _ (LogMessage _)        -> "LogMessage"
    LogObject _ _ (LogValue _ _)        -> "LogValue"
    LogObject _ _ (ObserveOpen _)       -> "ObserveOpen"
    LogObject _ _ (ObserveDiff _)       -> "ObserveDiff"
    LogObject _ _ (ObserveClose _)      -> "ObserveClose"
    LogObject _ _ (AggregatedMessage _) -> "AggregatedMessage"
    LogObject _ _ (MonitoringEffect _)  -> "MonitoringEffect"
    LogObject _ _ (Command _)           -> "Command"
    LogObject _ _ KillPill              -> "KillPill"

\end{code}

\label{code:CommandValue}\index{CommandValue}
Backends can enter commands to the trace. Commands will end up in the
|Switchboard|, which will interpret them and take action.
\begin{code}
data CommandValue = DumpBufferedTo BackendKind
                    deriving (Generic, Show, ToJSON)

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
