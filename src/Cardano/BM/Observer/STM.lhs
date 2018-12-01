
\subsection{Cardano.BM.Observer.STM}

%if False
\begin{code}

module Cardano.BM.Observer.STM
    (
      bracketObserveIO
    , bracketObserveLogIO
    ) where

import qualified Control.Monad.STM as STM

import           Data.Text

import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.SubTrace
import           Cardano.BM.Observer.Monadic (observeClose, observeOpen)
import           Cardano.BM.Trace (Trace, subTrace)
\end{code}
%endif

\begin{code}

stmWithLog :: STM.STM (t, [LogObject]) -> STM.STM (t, [LogObject])
stmWithLog action = action

\end{code}

\subsubsection{Observe |STM| action in a named context}\label{code:bracketObserveIO}
\begin{code}

bracketObserveIO :: Trace IO -> Text -> STM.STM t -> IO t
bracketObserveIO logTrace0 name action = do
    (subTrace, logTrace) <- subTrace name logTrace0
    bracketObserveIO' subTrace logTrace action

bracketObserveIO' :: SubTrace -> Trace IO -> STM.STM t -> IO t
bracketObserveIO' NoTrace _ action =
    STM.atomically action
bracketObserveIO' subTrace logTrace action = do
    countersid <- observeOpen subTrace logTrace
    -- run action, returns result only
    t <- STM.atomically action
    observeClose subTrace logTrace countersid []
    pure t

\end{code}

\subsubsection{Observe |STM| action in a named context and output captured log items}\label{code:bracketObserveLogIO}
\begin{code}

bracketObserveLogIO :: Trace IO -> Text -> STM.STM (t,[LogObject]) -> IO t
bracketObserveLogIO logTrace0 name action = do
    (subTrace, logTrace) <- subTrace name logTrace0
    bracketObserveLogIO' subTrace logTrace action

bracketObserveLogIO' :: SubTrace -> Trace IO -> STM.STM (t,[LogObject]) -> IO t
bracketObserveLogIO' NoTrace _ action = do
    (t, _) <- STM.atomically $ stmWithLog action
    pure t
bracketObserveLogIO' subTrace logTrace action = do
    countersid <- observeOpen subTrace logTrace
    -- run action, return result and log items
    (t, as) <- STM.atomically $ stmWithLog action
    observeClose subTrace logTrace countersid as
    pure t

\end{code}
