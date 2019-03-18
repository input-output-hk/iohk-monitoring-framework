
\subsection{Cardano.BM.Data.Backend}
\label{code:Cardano.BM.Data.Backend}

%if style == newcode
\begin{code}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

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
class IsEffectuator t a where
    effectuate     :: t a -> NamedLogItem a -> IO ()
    effectuatefrom :: forall s . (IsEffectuator s a) => t a -> NamedLogItem a -> s a -> IO ()
    default effectuatefrom :: forall s . (IsEffectuator s a) => t a -> NamedLogItem a -> s a -> IO ()
    effectuatefrom t nli _ = effectuate t nli
    handleOverflow :: t a -> IO ()

\end{code}

\subsubsection{Declaration of a |Backend|}\label{code:IsBackend}\index{IsBackend}
A backend is life-cycle managed, thus can be |realize|d and |unrealize|d.
\begin{code}
class IsEffectuator t a => IsBackend t a where
    typeof      :: t a -> BackendKind
    realize     :: Configuration -> IO (t a)
    realizefrom :: forall s . (IsEffectuator s a) => Trace IO a -> s a -> IO (t a)
    default realizefrom :: forall s . (IsEffectuator s a) => Trace IO a -> s a -> IO (t a)
    realizefrom (ctx,_) _ = realize (configuration ctx)
    unrealize   :: t a -> IO ()

\end{code}

\subsubsection{Backend}\label{code:Backend}\index{Backend}
This data structure for a backend defines its behaviour
as an |IsEffectuator| when processing an incoming message,
and as an |IsBackend| for unrealizing the backend.
\begin{code}
data Backend a = MkBackend
    { bEffectuate :: NamedLogItem a -> IO ()
    , bUnrealize  :: IO ()
    }

\end{code}
