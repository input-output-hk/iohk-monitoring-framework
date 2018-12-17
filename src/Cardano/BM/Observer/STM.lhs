
\subsection{Cardano.BM.Observer.STM}

%if style == newcode
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
import           Cardano.BM.Trace (Trace, subTrace, typeofTrace)

\end{code}
%endif

\begin{code}
stmWithLog :: STM.STM (t, [LogObject]) -> STM.STM (t, [LogObject])
stmWithLog action = action

\end{code}

\subsubsection{Observe |STM| action in a named context}\label{code:bracketObserveIO}
With given name, create a |SubTrace| according to |Configuration|
and run the passed |STM| action on it.
\begin{code}
bracketObserveIO :: Trace IO -> Text -> STM.STM t -> IO t
bracketObserveIO logTrace0 name action = do
    logTrace <- subTrace name logTrace0
    let subtrace = typeofTrace logTrace
    bracketObserveIO' subtrace logTrace action
  where
    bracketObserveIO' :: SubTrace -> Trace IO -> STM.STM t -> IO t
    bracketObserveIO' NoTrace _ act =
        STM.atomically act
    bracketObserveIO' subtrace logTrace act = do
        countersid <- observeOpen subtrace logTrace
        -- run action, returns result only
        t <- STM.atomically act
        observeClose subtrace logTrace countersid []
        pure t

\end{code}

\subsubsection{Observe |STM| action in a named context and output captured log items}\label{code:bracketObserveLogIO}
The |STM| action might output messages, which after "success" will be forwarded to the logging trace.
Otherwise, this function behaves the same as \nameref{code:bracketObserveIO}.
\begin{code}
bracketObserveLogIO :: Trace IO -> Text -> STM.STM (t,[LogObject]) -> IO t
bracketObserveLogIO logTrace0 name action = do
    logTrace <- subTrace name logTrace0
    let subtrace = typeofTrace logTrace
    bracketObserveLogIO' subtrace logTrace action
  where
    bracketObserveLogIO' :: SubTrace -> Trace IO -> STM.STM (t,[LogObject]) -> IO t
    bracketObserveLogIO' NoTrace _ act = do
        (t, _) <- STM.atomically $ stmWithLog act
        pure t
    bracketObserveLogIO' subtrace logTrace act = do
        countersid <- observeOpen subtrace logTrace
        -- run action, return result and log items
        (t, as) <- STM.atomically $ stmWithLog act
        observeClose subtrace logTrace countersid as
        pure t

\end{code}
