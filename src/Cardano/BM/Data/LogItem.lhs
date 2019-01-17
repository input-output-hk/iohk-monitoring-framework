
\subsection{Cardano.BM.Data.LogItem}

%if style == newcode
\begin{code}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.BM.Data.LogItem
  ( NamedLogItem
  , LogItem (..)
  , LogNamed (..)
  , LogObject (..)
  , LoggerName
  , LogSelection (..)
  )
  where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Text (Text)

import           GHC.Generics (Generic)

import           Cardano.BM.Data.Aggregated (Aggregated (..), Measurable (..))
import           Cardano.BM.Data.Counter
import           Cardano.BM.Data.Severity

\end{code}
%endif

\subsubsection{LoggerName}\label{code:LoggerName}\index{LoggerName}
\begin{code}
type LoggerName = Text
\end{code}

\subsubsection{NamedLogItem}\label{code:NamedLogItem}\index{NamedLogItem}
\begin{code}
type NamedLogItem = LogNamed LogObject

\end{code}

\subsubsection{LogItem}\label{code:LogItem}\index{LogItem}
\label{code:liSelection}\index{LogItem!liSelection}
\label{code:liSeverity}\index{LogItem!liSeverity}
\label{code:liPayload}\index{LogItem!liPayload}
\todo[inline]{TODO |liPayload :: ToObject|}
\begin{code}
data LogItem = LogItem
    { liSelection :: LogSelection
    , liSeverity  :: Severity
    , liPayload   :: Text   -- TODO should become ToObject
    } deriving (Show, Generic, ToJSON)

\end{code}

\label{code:LogSelection}\index{LogSelection}
\label{code:Public}\index{LogSelection!Public}
\label{code:PublicUnsafe}\index{LogSelection!PublicUnsafe}
\label{code:Private}\index{LogSelection!Private}
\label{code:Both}\index{LogSelection!Both}
\begin{code}
data LogSelection =
      Public       -- only to public logs.
    | PublicUnsafe -- only to public logs, not console.
    | Private      -- only to private logs.
    | Both         -- to public and private logs.
    deriving (Show, Generic, ToJSON, FromJSON)

\end{code}


\subsubsection{LogObject}
\label{code:LogObject}\index{LogObject}
\begin{code}

data LogObject = LogMessage LogItem
               | LogValue Text Measurable
               | ObserveOpen CounterState
               | ObserveDiff CounterState
               | ObserveClose CounterState
               | AggregatedMessage [(Text, Aggregated)]
               | KillPill
                 deriving (Generic, Show, ToJSON)

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
