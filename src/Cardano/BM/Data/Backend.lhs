
\subsection{Cardano.BM.Data.Backend}

%if style == newcode
\begin{code}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE DefaultSignatures #-}

module Cardano.BM.Data.Backend
  ( Backend (..)
  , BackendKind (..)
  , IsBackend (..)
  , IsEffectuator (..)
  )
  where

import           Cardano.BM.Data.BackendKind
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Configuration.Model (Configuration)

\end{code}
%endif

\subsubsection{Accepts a \nameref{code:NamedLogItem}}\label{code:HasPass}
\begin{code}
class IsEffectuator t where
    effectuate  :: t -> NamedLogItem -> IO ()

\end{code}

\subsubsection{Declaration of a |Backend|}\label{code:IsBackend}
\begin{code}
class (IsEffectuator t) => IsBackend t where
    typeof :: t -> BackendKind
    realize     :: Configuration -> IO t
    realizefrom :: forall s . (IsEffectuator s) => Configuration -> s -> IO t
    default realizefrom :: forall s . (IsEffectuator s) => Configuration -> s -> IO t
    realizefrom c _ = realize c
    unrealize   :: t -> IO ()
    effectuatefrom :: forall s . (IsEffectuator s) => t -> NamedLogItem -> s -> IO ()
    default effectuatefrom :: forall s . (IsEffectuator s) => t -> NamedLogItem -> s -> IO ()
    effectuatefrom t nli _ = effectuate t nli

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
