
\subsection{Cardano.BM.Data.Backend}
\label{code:Cardano.BM.Data.Backend}

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
import           Cardano.BM.Data.Trace
import           Cardano.BM.Configuration.Model (Configuration)

\end{code}
%endif

\subsubsection{Accepts a \nameref{code:NamedLogItem}}\label{code:IsEffectuator}\index{IsEffectuator}
Instances of this type class accept a |NamedLogItem| and deal with it.
\begin{code}
class IsEffectuator t where
    effectuate     :: t -> NamedLogItem -> IO ()
    effectuatefrom :: forall s . (IsEffectuator s) => t -> NamedLogItem -> s -> IO ()
    default effectuatefrom :: forall s . (IsEffectuator s) => t -> NamedLogItem -> s -> IO ()
    effectuatefrom t nli _ = effectuate t nli
    handleOverflow :: t -> IO ()

\end{code}

\subsubsection{Declaration of a |Backend|}\label{code:IsBackend}\index{IsBackend}
A backend is life-cycle managed, thus can be |realize|d and |unrealize|d.
\begin{code}
class (IsEffectuator t) => IsBackend t where
    typeof      :: t -> BackendKind
    realize     :: Configuration -> IO t
    realizefrom :: forall s . (IsEffectuator s) => Trace IO -> s -> IO t
    default realizefrom :: forall s . (IsEffectuator s) => Trace IO -> s -> IO t
    realizefrom (ctx,_) _ = realize (configuration ctx)
    unrealize   :: t -> IO ()

\end{code}

\subsubsection{Backend}\label{code:Backend}\index{Backend}
This data structure for a backend defines its behaviour
as an |IsEffectuator| when processing an incoming message,
and as an |IsBackend| for unrealizing the backend.
\begin{code}
data Backend = MkBackend
    { bEffectuate :: NamedLogItem -> IO ()
    , bUnrealize  :: IO ()
    }

\end{code}
