
\subsection{Cardano.BM.Observer.Monadic}

%if style == newcode
\begin{code}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.BM.Observer.Monadic
    (
      bracketObserveIO
    , bracketObserveM
      -- * observing functions
    , observeOpen
    , observeClose
    ) where

import           Control.Monad (forM_)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Catch (MonadCatch, SomeException, catch, throwM)

import           Data.Text
import           Data.Unique (newUnique)

import           Cardano.BM.Data.Counter (CounterState (..), diffCounters)
import           Cardano.BM.Data.LogItem (LogObject (..), LOContent (..), mkLOMeta)
import           Cardano.BM.Data.SubTrace (SubTrace (NoTrace))
import           Cardano.BM.Counters (readCounters)
import           Cardano.BM.Trace (Trace, logError, logNotice, subTrace, traceNamedObject,
                     typeofTrace)
\end{code}
%endif

\subsubsection{Monadic.bracketObserverIO}
Observes an |IO| action and adds a name to the logger
name of the passed in |Trace|. An empty |Text| leaves
the logger name untouched.
\newline
\par\noindent
Microbenchmarking steps:
\newline
\par
1. Create a |trace| which will have been configured
   to observe things besides logging.

\begin{spec}
        import qualified Cardano.BM.Configuration.Model as CM
        . . .
        c <- config
        trace@(ctx, _) <- setupTrace (Right c) "demo-playground"
            where
                config :: IO CM.Configuration
                config = do
                    c <- CM.empty
                    CM.setMinSeverity c Debug
                    CM.setSetupBackends c [KatipBK, AggregationBK]
                    CM.setDefaultBackends c [KatipBK, AggregationBK]
                    CM.setSetupScribes c [ ScribeDefinition {
                                              scName = "stdout"
                                            , scKind = StdoutSK
                                            , scRotation = Nothing
                                            }
                                    ]
                    CM.setDefaultScribes c ["StdoutSK::stdout"]

                    return c
\end{spec}

2. |c| is the |Configuration| of |trace|. In order to
   enable the collection and processing of measurements
   (min, max, mean, std-dev) |AggregationBK| is needed.

\begin{spec}
        CM.setDefaultBackends c [KatipBK, AggregationBK]
\end{spec}
in a configuration file (YAML) means

\begin{spec}
        defaultBackends:
          - KatipBK
          - AggregationBK
\end{spec}

3. Set the measurements that you want to take by changing
   the configuration of the |trace| using |setSubTrace|,
   in order to declare the namespace where we want to
   enable the particular measurements and the list with
   the kind of measurements.

\begin{spec}
        CM.setSubTrace
            (configuration ctx)
            "demo-playground.submit-tx"
            (Just $ ObservableTrace observablesSet)
          where
            observablesSet = [MonotonicClock, MemoryStats]
\end{spec}

4. Find an action to measure. e.g.:

\begin{spec}
        runProtocolWithPipe x hdl proto `catch` (\ProtocolStopped -> return ())
\end{spec}

    and use |bracketObserveIO|. e.g.:


\begin{spec}
        bracketObserveIO trace "submit-tx" $
            runProtocolWithPipe x hdl proto `catch` (\ProtocolStopped -> return ())
\end{spec}

-------------------

\begin{code}
bracketObserveIO :: Trace IO -> Text -> IO t -> IO t
bracketObserveIO logTrace0 name action = do
    logTrace <- subTrace name logTrace0
    bracketObserveIO' (typeofTrace logTrace) logTrace action
  where
    bracketObserveIO' :: SubTrace -> Trace IO -> IO t -> IO t
    bracketObserveIO' NoTrace _ act = act
    bracketObserveIO' subtrace logTrace act = do
        mCountersid <- observeOpen subtrace logTrace

        -- run action; if an exception is caught will be logged and rethrown.
        t <- act `catch` (\(e :: SomeException) -> (logError logTrace (pack (show e)) >> throwM e))

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

\subsubsection{Monadic.bracketObserverM}
Observes a |MonadIO m => m| action and adds a name to the logger
name of the passed in |Trace|. An empty |Text| leaves
the logger name untouched.
\begin{code}
bracketObserveM :: (MonadCatch m, MonadIO m) => Trace IO -> Text -> m t -> m t
bracketObserveM logTrace0 name action = do
    logTrace <- liftIO $ subTrace name logTrace0
    bracketObserveM' (typeofTrace logTrace) logTrace action
  where
    bracketObserveM' :: (MonadCatch m, MonadIO m) => SubTrace -> Trace IO -> m t -> m t
    bracketObserveM' NoTrace _ act = act
    bracketObserveM' subtrace logTrace act = do
        mCountersid <- liftIO $ observeOpen subtrace logTrace

        -- run action; if an exception is caught will be logged and rethrown.
        t <- act `catch`
            (\(e :: SomeException) -> (liftIO (logError logTrace (pack (show e)) >> throwM e)))

        case mCountersid of
            Left openException ->
                -- since observeOpen faced an exception there is no reason to call observeClose
                -- however the result of the action is returned
                liftIO $ logNotice logTrace ("ObserveOpen: " <> pack (show openException))
            Right countersid -> do
                    res <- liftIO $ observeClose subtrace logTrace countersid []
                    case res of
                        Left ex -> liftIO (logNotice logTrace ("ObserveClose: " <> pack (show ex)))
                        _ -> pure ()
        pure t


\end{code}

\subsubsection{observerOpen}\label{observeOpen}
\begin{code}
observeOpen :: SubTrace -> Trace IO -> IO (Either SomeException CounterState)
observeOpen subtrace logTrace = (do
    identifier <- newUnique

    -- take measurement
    counters <- readCounters subtrace
    let state = CounterState identifier counters
    -- send opening message to Trace
    traceNamedObject logTrace =<<
        LogObject <$> mkLOMeta <*> pure (ObserveOpen state)
    return (Right state)) `catch` (return . Left)

\end{code}

\subsubsection{observeClose}\label{observeClose}
\begin{code}
observeClose :: SubTrace -> Trace IO -> CounterState -> [LogObject] -> IO (Either SomeException ())
observeClose subtrace logTrace initState logObjects = (do
    let identifier = csIdentifier initState
        initialCounters = csCounters initState

    -- take measurement
    counters <- readCounters subtrace
    -- send closing message to Trace
    traceNamedObject logTrace =<<
        LogObject <$> mkLOMeta <*> pure (ObserveClose (CounterState identifier counters))
    -- send diff message to Trace
    traceNamedObject logTrace =<<
        LogObject <$> mkLOMeta <*> pure (ObserveDiff (CounterState identifier (diffCounters initialCounters counters)))
    -- trace the messages gathered from inside the action
    forM_ logObjects $ traceNamedObject logTrace
    return (Right ())) `catch` (return . Left)

\end{code}
