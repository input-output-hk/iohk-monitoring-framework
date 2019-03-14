
\subsection{Cardano.BM.Data.LogItem}
\label{code:Cardano.BM.Data.LogItem}

%if style == newcode
\begin{code}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.BM.Data.LogItem
  ( NamedLogItem
  , LogNamed (..)
  , LogObject (..)
  , LOMeta (..), mkLOMeta
  , LOContent (..)
  , LoggerName
  , PrivacyAnnotation (..)
  , PrivacyAndSeverityAnnotated (..)
  )
  where

import           Control.Concurrent (myThreadId)
import           Data.Aeson (FromJSON, ToJSON, object, toJSON, (.=))
import           Data.Text (Text, pack)
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           GHC.Generics (Generic)

import           Cardano.BM.Data.Aggregated (Aggregated (..), Measurable (..))
import           Cardano.BM.Data.Counter
import           Cardano.BM.Data.Severity

\end{code}
%endif

\subsubsection{LoggerName}\label{code:LoggerName}\index{LoggerName}
A |LoggerName| has currently type |Text|.
\begin{code}
type LoggerName = Text

\end{code}

\subsubsection{NamedLogItem}\label{code:NamedLogItem}\index{NamedLogItem}
\begin{code}
type NamedLogItem a = LogNamed (LogObject a)

\end{code}

\subsubsection{LogNamed}\label{code:LogNamed}\index{LogNamed}
A |LogNamed| contains of a context name and some log item.
\begin{code}

data LogNamed item = LogNamed
    { lnName :: LoggerName
    , lnItem :: item
    } deriving (Show)

deriving instance Generic item => Generic (LogNamed item)
deriving instance (ToJSON item, Generic item) => ToJSON (LogNamed item)

\end{code}

\subsubsection{Logging of outcomes with |LogObject|}
\label{code:LogObject}\index{LogObject}
\label{code:LOMeta}\index{LOMeta}
\label{code:LOContent}\index{LOContent}

\begin{code}
data LogObject a = LogObject LOMeta (LOContent a)
                   deriving (Generic, Show, ToJSON)

\end{code}

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
        object ["tstamp" .= _tstamp, "tid" .= show _tid , "severity" .= show _sev, "privacy" .= show _priv]

mkLOMeta :: Severity -> PrivacyAnnotation -> IO LOMeta
mkLOMeta sev priv =
    LOMeta <$> getCurrentTime
           <*> (pack . show <$> myThreadId)
           <*> pure sev
           <*> pure priv

\end{code}

\label{code:LogMessage}\index{LogMessage}
\label{code:LogValue}\index{LogValue}
\label{code:ObserveOpen}\index{ObserveOpen}
\label{code:ObserveDiff}\index{ObserveDiff}
\label{code:ObserveClose}\index{ObserveClose}
\label{code:AggregatedMessage}\index{AggregatedMessage}
Payload of a |LogObject|:
\begin{code}
data LOContent a = LogMessage a
                 | LogValue Text Measurable
                 | ObserveOpen CounterState
                 | ObserveDiff CounterState
                 | ObserveClose CounterState
                 | AggregatedMessage [(Text, Aggregated)]
                 | MonitoringEffect (LogObject a)
                 | KillPill
                   deriving (Generic, Show, ToJSON)

\end{code}

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
            = PSA
                { psaSeverity :: !Severity
                , psaPrivacy  :: !PrivacyAnnotation
                , psaPayload  :: a
                }
              deriving (Show)

\end{code}
