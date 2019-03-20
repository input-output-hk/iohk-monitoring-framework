
\subsection{Cardano.BM.Observer.STM}
\label{code:Cardano.BM.Observer.STM}

%if style == newcode
\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.BM.Observer.STM
    (
      bracketObserveIO
    , bracketObserveLogIO
    ) where

import           Control.Exception.Safe (SomeException, catch, throwM)
import qualified Control.Monad.STM as STM

import           Data.Maybe (fromMaybe)
import           Data.Text
import qualified Data.Text.IO as TIO
import           System.IO (stderr)

import qualified Cardano.BM.Configuration as Config
import           Cardano.BM.Data.LogItem (LOContent, LOMeta)
import           Cardano.BM.Data.SubTrace
import           Cardano.BM.Data.Severity (Severity)
import           Cardano.BM.Observer.Monadic (observeClose, observeOpen)
import           Cardano.BM.Trace (Trace)

\end{code}
%endif

\begin{code}
stmWithLog :: STM.STM (t, [(LOMeta, LOContent a)]) -> STM.STM (t, [(LOMeta, LOContent a)])
stmWithLog action = action

\end{code}

\subsubsection{Observe |STM| action in a named context}\label{code:bracketObserveIO}
With given name, create a |SubTrace| according to |Configuration|
and run the passed |STM| action on it.
\begin{code}
bracketObserveIO :: Config.Configuration -> Trace IO a -> Severity -> Text -> STM.STM t -> IO t
bracketObserveIO config trace severity name action = do
    subTrace <- fromMaybe Neutral <$> Config.findSubTrace config name
    bracketObserveIO' subTrace severity trace action
  where
    bracketObserveIO' :: SubTrace -> Severity -> Trace IO a -> STM.STM t -> IO t
    bracketObserveIO' NoTrace _ _ act =
        STM.atomically act
    bracketObserveIO' subtrace sev logTrace act = do
        mCountersid <- observeOpen subtrace sev logTrace

        -- run action; if an exception is caught, then it will be logged and rethrown.
        t <- (STM.atomically act) `catch` (\(e :: SomeException) -> (TIO.hPutStrLn stderr (pack (show e)) >> throwM e))

        case mCountersid of
            Left openException ->
                -- since observeOpen faced an exception there is no reason to call observeClose
                -- however the result of the action is returned
                TIO.hPutStrLn stderr ("ObserveOpen: " <> pack (show openException))
            Right countersid -> do
                    res <- observeClose subtrace sev logTrace countersid []
                    case res of
                        Left ex -> TIO.hPutStrLn stderr ("ObserveClose: " <> pack (show ex))
                        _ -> pure ()
        pure t

\end{code}

\subsubsection{Observe |STM| action in a named context and output captured log items}\label{code:bracketObserveLogIO}
The |STM| action might output messages, which after "success" will be forwarded to the logging trace.
Otherwise, this function behaves the same as |bracketObserveIO|.
\begin{code}
bracketObserveLogIO :: Config.Configuration -> Trace IO a -> Severity -> Text -> STM.STM (t,[(LOMeta, LOContent a)]) -> IO t
bracketObserveLogIO config trace severity name action = do
    subTrace <- fromMaybe Neutral <$> Config.findSubTrace config name
    bracketObserveLogIO' subTrace severity trace action
  where
    bracketObserveLogIO' :: SubTrace -> Severity -> Trace IO a -> STM.STM (t,[(LOMeta, LOContent a)]) -> IO t
    bracketObserveLogIO' NoTrace _ _ act = do
        (t, _) <- STM.atomically $ stmWithLog act
        pure t
    bracketObserveLogIO' subtrace sev logTrace act = do
        mCountersid <- observeOpen subtrace sev logTrace

        -- run action, return result and log items; if an exception is
        -- caught, then it will be logged and rethrown.
        (t, as) <- (STM.atomically $ stmWithLog act) `catch`
                    (\(e :: SomeException) -> (TIO.hPutStrLn stderr (pack (show e)) >> throwM e))

        case mCountersid of
            Left openException ->
                -- since observeOpen faced an exception there is no reason to call observeClose
                -- however the result of the action is returned
                TIO.hPutStrLn stderr ("ObserveOpen: " <> pack (show openException))
            Right countersid -> do
                    res <- observeClose subtrace sev logTrace countersid as
                    case res of
                        Left ex -> TIO.hPutStrLn stderr ("ObserveClose: " <> pack (show ex))
                        _ -> pure ()
        pure t

\end{code}
