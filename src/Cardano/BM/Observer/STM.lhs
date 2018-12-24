
\subsection{Cardano.BM.Observer.STM}

%if style == newcode
\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.BM.Observer.STM
    (
      bracketObserveIO
    , bracketObserveLogIO
    ) where

import           Control.Monad.Catch (SomeException, catch, throwM)
import qualified Control.Monad.STM as STM

import           Data.Text

import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.SubTrace
import           Cardano.BM.Observer.Monadic (observeClose, observeOpen)
import           Cardano.BM.Trace (Trace, logError, logNotice, subTrace, typeofTrace)

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
        mCountersid <- observeOpen subtrace logTrace

        -- run action; if an exception is caught will be logged and rethrown.
        t <- (STM.atomically act) `catch` (\(e :: SomeException) -> (logError logTrace (pack (show e)) >> throwM e))

        case mCountersid of
            Left openException ->
                -- since observeOpen faced an exception there is no reason to call observeClose
                -- however the result of the action is returned
                logNotice logTrace ("ObserveOpen: " <> pack (show openException))
            Right countersid -> do
                    res <- observeClose subtrace logTrace countersid []
                    case res of
                        Left ex -> logNotice logTrace ("ObserveClose: " <> pack (show ex))
                        _ -> pure ()
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
        mCountersid <- observeOpen subtrace logTrace

        -- run action, return result and log items; if an exception is
        -- caught will be logged and rethrown.
        (t, as) <- (STM.atomically $ stmWithLog act) `catch`
                    (\(e :: SomeException) -> (logError logTrace (pack (show e)) >> throwM e))

        case mCountersid of
            Left openException ->
                -- since observeOpen faced an exception there is no reason to call observeClose
                -- however the result of the action is returned
                logNotice logTrace ("ObserveOpen: " <> pack (show openException))
            Right countersid -> do
                    res <- observeClose subtrace logTrace countersid as
                    case res of
                        Left ex -> logNotice logTrace ("ObserveClose: " <> pack (show ex))
                        _ -> pure ()
        pure t

\end{code}
