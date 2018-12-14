
\subsection{Cardano.BM.Data.Backend}

%if style == newcode
\begin{code}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE RankNTypes     #-}

module Cardano.BM.Data.Backend
  ( Backend (..)
  , BackendKind (..)
  , HasPass (..)
  )
  where

import           Data.Aeson (FromJSON, ToJSON)
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
                 deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON)

\end{code}

\subsubsection{Accepts a \nameref{code:NamedLogItem}}\label{code:HasPass}
\begin{code}
class HasPass t where
    pass :: t -> NamedLogItem -> IO ()

class IsBackend t where
    typeof :: BackendKind
    setup :: Configuration -> IO ()
    terminate :: t -> IO ()
    pass :: t -> NamedLogItem -> IO ()
    passfrom :: forall s . (HasPass s) => t -> NamedLogItem -> s -> IO ()
    default passfrom t nli _ = pass t nli

instance (IsBackend t) => HasPass t where
    pass = pass

\end{code}

\subsubsection{Backend}\label{code:Backend}
A backend is referenced through the function |bPass| which accepts
a \nameref{code:NamedLogItem} and a terminating function |bTerminate|
which is responsible for closing the specific backend.

\begin{code}
data Backend = MkBackend
    { bPass      :: NamedLogItem -> IO ()
    , bTerminate :: IO ()
    }

\end{code}
