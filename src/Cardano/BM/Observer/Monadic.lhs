
\subsection{Cardano.BM.Observer.Monadic}
\label{code:Cardano.BM.Observer.Monadic}

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

import           Control.Exception.Safe (MonadCatch, SomeException, catch, throwM)
import           Control.Monad (forM_)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Maybe (fromMaybe)
import           Data.Text
import qualified Data.Text.IO as TIO
import           Data.Unique (newUnique)
import           System.IO (stderr)

import           Cardano.BM.Data.Counter (CounterState (..), diffCounters)
import           Cardano.BM.Data.LogItem (LogObject (..), LOContent (..),
                     PrivacyAnnotation(Confidential), mkLOMeta)
import           Cardano.BM.Data.Severity (Severity)
import qualified Cardano.BM.Configuration as Config
import           Cardano.BM.Counters (readCounters)
import           Cardano.BM.Data.SubTrace (SubTrace (Neutral, NoTrace))
import           Cardano.BM.Data.Trace (TraceContext (..))
import           Cardano.BM.Trace (Trace, traceNamedObject)
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
bracketObserveIO :: Trace IO a -> Severity -> Text -> IO t -> IO t
bracketObserveIO trace@(ctx, _) severity name action = do
    subTrace <- fromMaybe Neutral <$> Config.findSubTrace (configuration ctx) name
    bracketObserveIO' subTrace severity trace action
  where
    bracketObserveIO' :: SubTrace -> Severity -> Trace IO a -> IO t -> IO t
    bracketObserveIO' NoTrace _ _ act = act
    bracketObserveIO' subtrace sev logTrace act = do
        mCountersid <- observeOpen subtrace sev logTrace

        -- run action; if an exception is caught it will be logged and rethrown.
        t <- act `catch` (\(e :: SomeException) -> (TIO.hPutStrLn stderr (pack (show e)) >> throwM e))

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

\subsubsection{Monadic.bracketObserverM}
Observes a |MonadIO m => m| action and adds a name to the logger
name of the passed in |Trace|. An empty |Text| leaves
the logger name untouched.
\begin{code}
bracketObserveM :: (MonadCatch m, MonadIO m) => Trace IO a -> Severity -> Text -> m t -> m t
bracketObserveM trace@(ctx, _) severity name action = do
    subTrace <- liftIO $ fromMaybe Neutral <$> Config.findSubTrace (configuration ctx) name
    bracketObserveM' subTrace severity trace action
  where
    bracketObserveM' :: (MonadCatch m, MonadIO m) => SubTrace -> Severity -> Trace IO a -> m t -> m t
    bracketObserveM' NoTrace _ _ act = act
    bracketObserveM' subtrace sev logTrace act = do
        mCountersid <- liftIO $ observeOpen subtrace sev logTrace

        -- run action; if an exception is caught it will be logged and rethrown.
        t <- act `catch` (\(e :: SomeException) -> liftIO (TIO.hPutStrLn stderr (pack (show e)) >> throwM e))

        case mCountersid of
            Left openException ->
                -- since observeOpen faced an exception there is no reason to call observeClose
                -- however the result of the action is returned
                liftIO $ TIO.hPutStrLn stderr ("ObserveOpen: " <> pack (show openException))
            Right countersid -> do
                    res <- liftIO $ observeClose subtrace sev logTrace countersid []
                    case res of
                        Left ex -> liftIO (TIO.hPutStrLn stderr ("ObserveClose: " <> pack (show ex)))
                        _ -> pure ()
        pure t

\end{code}

\subsubsection{observerOpen}\label{observeOpen}
\begin{code}
observeOpen :: SubTrace -> Severity -> Trace IO a -> IO (Either SomeException CounterState)
observeOpen subtrace severity logTrace = (do
    identifier <- newUnique

    -- take measurement
    counters <- readCounters subtrace
    let state = CounterState identifier counters
    if counters == []
    then return ()
    else do
        -- send opening message to Trace
        traceNamedObject logTrace =<<
            LogObject <$> (mkLOMeta severity Confidential) <*> pure (ObserveOpen state)
    return (Right state)) `catch` (return . Left)

\end{code}

\subsubsection{observeClose}\label{observeClose}
\begin{code}
observeClose
    :: SubTrace
    -> Severity
    -> Trace IO a
    -> CounterState
    -> [LogObject a]
    -> IO (Either SomeException ())
observeClose subtrace sev logTrace initState logObjects = (do
    let identifier = csIdentifier initState
        initialCounters = csCounters initState

    -- take measurement
    counters <- readCounters subtrace
    if counters == []
    then return ()
    else do
        mle <- mkLOMeta sev Confidential
        -- send closing message to Trace
        traceNamedObject logTrace $
            LogObject mle (ObserveClose (CounterState identifier counters))
        -- send diff message to Trace
        traceNamedObject logTrace $
            LogObject mle (ObserveDiff (CounterState identifier (diffCounters initialCounters counters)))
    -- trace the messages gathered from inside the action
    forM_ logObjects $ traceNamedObject logTrace
    return (Right ())) `catch` (return . Left)

\end{code}
