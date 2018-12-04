
\subsection{Cardano.BM.Data.Backend}

%if style == newcode
\begin{code}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Cardano.BM.Data.Backend
  ( Backend (..)
  , BackendKind (..)
  , HasPass (..)
  )
  where

import           Data.Aeson (FromJSON)
import           GHC.Generics (Generic)

import           Cardano.BM.Data.LogItem

\end{code}
%endif

\subsubsection{BackendKind}\label{code:BackendKind}
This identifies the backends that can be attached to the \nameref{code:Switchboard}.
\begin{code}
data BackendKind = AggregationBK
                 | EKGViewBK
                 | KatipBK
                 | DevNullBK
                 deriving (Generic, Eq, Show, FromJSON)

\end{code}

\subsubsection{Accepts a \nameref{code:NamedLogItem}}\label{code:HasPass}
\begin{code}
class HasPass t where
    pass :: t -> NamedLogItem -> IO ()

\end{code}

\subsubsection{Backend}\label{code:Backend}
A backend is referenced through the function |pass'| which accepts
a \nameref{code:NamedLogItem}.

\begin{code}
data Backend = MkBackend { pass' :: NamedLogItem -> IO () }

\end{code}
