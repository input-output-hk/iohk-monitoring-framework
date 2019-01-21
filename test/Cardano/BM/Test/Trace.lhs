
\subsection{Trace}

%if style == newcode
\begin{code}
{-# LANGUAGE LambdaCase #-}

module Cardano.BM.Test.Trace (
    TraceConfiguration (..)
  , tests
  ) where

import           Prelude hiding (lookup)

import qualified Control.Concurrent.STM.TVar as STM
import qualified Control.Monad.STM as STM

import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (newMVar)
import           Control.Monad (forM, forM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.Map (fromListWith, lookup)
import           Data.Text (Text, append, pack)
import qualified Data.Text as T
import           Data.Time.Units (Microsecond)
import           Data.Unique (newUnique)

import           Cardano.BM.Configuration (inspectSeverity,
                     minSeverity, setMinSeverity, setSeverity)
import           Cardano.BM.Configuration.Model (empty, setSubTrace)
import           Cardano.BM.Configuration.Static (defaultConfigTesting)
import           Cardano.BM.Counters (diffTimeObserved, getMonoClock)
import qualified Cardano.BM.BaseTrace as BaseTrace
import           Cardano.BM.Data.Counter
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Observable
import           Cardano.BM.Data.Output
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.SubTrace
import           Cardano.BM.Data.Trace
import qualified Cardano.BM.Observer.Monadic as MonadicObserver
import qualified Cardano.BM.Observer.STM as STMObserver
import           Cardano.BM.Setup (newContext)
import qualified Cardano.BM.Setup as Setup
import           Cardano.BM.Trace (Trace, appendName, evalFilters, logInfo,
                    subTrace, traceInTVarIO, traceNamedInTVarIO)
import           Cardano.BM.Output.Switchboard (Switchboard(..))

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (Assertion, assertBool, testCase,
                     testCaseInfo)

\end{code}
%endif

\begin{code}
tests :: TestTree
tests = testGroup "testing Trace" [
        unit_tests
      , testCase "forked traces stress testing" stress_trace_in_fork
      , testCase "stress testing: ObservableTrace vs. NoTrace" timing_Observable_vs_Untimed
      , testCaseInfo "demonstrating nested named context logging" example_with_named_contexts
      ]

unit_tests :: TestTree
unit_tests = testGroup "Unit tests" [
        testCase "opening messages should not be traced" unit_noOpening_Trace
      , testCase "hierarchy of traces" unit_hierarchy
      , testCase "forked traces" unit_trace_in_fork
      , testCase "hierarchy of traces with NoTrace" $
            unit_hierarchy' [Neutral, NoTrace, (ObservableTrace observablesSet)]
                onlyLevelOneMessage
      , testCase "hierarchy of traces with DropOpening" $
            unit_hierarchy' [Neutral, DropOpening, (ObservableTrace observablesSet)]
                notObserveOpen
      , testCase "hierarchy of traces with UntimedTrace" $
            unit_hierarchy' [Neutral, UntimedTrace, UntimedTrace]
                observeOpenWithoutMeasures
      , testCase "changing the minimum severity of a trace at runtime"
            unit_trace_min_severity
      , testCase "changing the minimum severity of a named context at runtime"
            unit_named_min_severity
      , testCase "appending names should not exceed 80 chars" unit_append_name
      , testCase "creat subtrace which duplicates messages" unit_trace_duplicate
      , testCase "testing name filtering" unit_name_filtering
      ]
      where
        observablesSet = [MonotonicClock, MemoryStats]
        notObserveOpen :: [LogObject] -> Bool
        notObserveOpen = all (\case {ObserveOpen _ -> False; _ -> True})
        onlyLevelOneMessage :: [LogObject] -> Bool
        onlyLevelOneMessage = \case
            [LogMessage (LogItem _ _ "Message from level 1.")] -> True
            _                                                  -> False
        observeOpenWithoutMeasures :: [LogObject] -> Bool
        observeOpenWithoutMeasures = any $ \case
            ObserveOpen (CounterState _ counters) -> null counters
            _ -> False

\end{code}

\subsubsection{Helper routines}
\begin{code}
data TraceConfiguration = TraceConfiguration
    { tcOutputKind   :: OutputKind
    , tcName         :: LoggerName
    , tcSubTrace     :: SubTrace
    , tcSeverity     :: Severity
    }

setupTrace :: TraceConfiguration -> IO (Trace IO)
setupTrace (TraceConfiguration outk name subTr sev) = do
    c <- liftIO $ Cardano.BM.Configuration.Model.empty
    mockSwitchboard <- newMVar $ error "Switchboard uninitialized."
    ctx <- liftIO $ newContext name c sev $ Switchboard mockSwitchboard
    let logTrace0 = case outk of
            TVarList      tvar -> BaseTrace.natTrace liftIO $ traceInTVarIO tvar
            TVarListNamed tvar -> BaseTrace.natTrace liftIO $ traceNamedInTVarIO tvar

    setSubTrace (configuration ctx) name (Just subTr)
    logTrace' <- subTrace "" (ctx, logTrace0)
    return logTrace'

setTransformer_ :: Trace IO -> LoggerName -> Maybe SubTrace -> IO ()
setTransformer_ (ctx, _) name subtr = do
    let c = configuration ctx
        n = (loggerName ctx) <> "." <> name
    setSubTrace c n subtr

\end{code}

\subsubsection{Example of using named contexts with |Trace|}\label{code:example_with_named_contexts}
\begin{code}
example_with_named_contexts :: IO String
example_with_named_contexts = do
    cfg <- defaultConfigTesting
    logTrace <- Setup.setupTrace (Right cfg) "test"
    putStrLn "\n"
    logInfo logTrace "entering"
    logTrace0 <- appendName "simple-work-0" logTrace
    work0 <- complexWork0 logTrace0 "0"
    logTrace1 <- appendName "complex-work-1" logTrace
    work1 <- complexWork1 logTrace1 "42"

    Async.wait work0
    Async.wait work1
    -- the named context will include "complex" in the logged message
    logInfo logTrace "done."
    threadDelay 1000
    return ""
  where
    complexWork0 tr msg = Async.async $ logInfo tr ("let's see (0): " `append` msg)
    complexWork1 tr msg = Async.async $ do
        logInfo tr ("let's see (1): " `append` msg)
        trInner@(ctx,_) <- appendName "inner-work-1" tr
        let observablesSet = [MonotonicClock]
        setSubTrace (configuration ctx) "test.complex-work-1.inner-work-1.STM-action" $
            Just $ ObservableTrace observablesSet
        _ <- STMObserver.bracketObserveIO trInner "STM-action" setVar_
        logInfo trInner "let's see: done."
        -- logInfo logTrace' "let's see: done."

\end{code}

\subsubsection{Show effect of turning off observables}\label{timing_Observable_vs_Untimed}
\begin{code}
run_timed_action :: Trace IO -> IO Microsecond
run_timed_action logTrace = do
    runid <- newUnique
    t0 <- getMonoClock
    forM_ [(1::Int)..10] $ const $ observeAction logTrace
    t1 <- getMonoClock
    return $ diffTimeObserved (CounterState runid t0) (CounterState runid t1)
  where
    observeAction trace = do
        _ <- MonadicObserver.bracketObserveIO trace "" action
        return ()
    action = return $ forM [1::Int ..100] $ \_ -> reverse [1::Int ..1000]

timing_Observable_vs_Untimed :: Assertion
timing_Observable_vs_Untimed = do
    msgs1  <- STM.newTVarIO []
    traceObservable <- setupTrace $ TraceConfiguration
                                    (TVarList msgs1)
                                    "observables"
                                    (ObservableTrace observablesSet)
                                    Debug

    msgs2  <- STM.newTVarIO []
    traceUntimed <- setupTrace $ TraceConfiguration
                                    (TVarList msgs2)
                                    "no timing"
                                    UntimedTrace
                                    Debug

    msgs3  <- STM.newTVarIO []
    traceNoTrace <- setupTrace $ TraceConfiguration
                                    (TVarList msgs3)
                                    "no trace"
                                    NoTrace
                                    Debug

    t_observable <- run_timed_action traceObservable
    t_untimed    <- run_timed_action traceUntimed
    t_notrace    <- run_timed_action traceNoTrace

    assertBool
        ("Untimed consumed more time than ObservableTrace " ++ (show [t_untimed, t_observable]))
        (t_untimed < t_observable)
    assertBool
        ("NoTrace consumed more time than ObservableTrace" ++ (show [t_notrace, t_observable]))
        (t_notrace < t_observable)
    assertBool
        ("NoTrace consumed more time than Untimed" ++ (show [t_notrace, t_untimed]))
        True
  where
    observablesSet = [MonotonicClock, MemoryStats]

\end{code}

\subsubsection{Control tracing in a hierarchy of |Trace|s}\label{code:unit_hierarchy}
We can lay out traces in a hierarchical manner, that the children
forward traced items to the parent |Trace|.
A |NoTrace| introduced in this hierarchy will cut off a branch
from messaging to the root.
\begin{code}
unit_hierarchy :: Assertion
unit_hierarchy = do
    msgs <- STM.newTVarIO []
    trace0 <- setupTrace $ TraceConfiguration (TVarList msgs) "test" Neutral Debug
    logInfo trace0 "This should have been displayed!"

    -- subtrace of trace which traces nothing
    setTransformer_ trace0 "inner" (Just NoTrace)

    trace1 <- subTrace "inner" trace0
    logInfo trace1 "This should NOT have been displayed!"

    setTransformer_ trace1 "innermost" (Just Neutral)
    trace2 <- subTrace "innermost" trace1
    logInfo trace2 "This should NOT have been displayed also due to the trace one level above!"

    -- acquire the traced objects
    res <- STM.readTVarIO msgs

    -- only the first message should have been traced
    assertBool
        ("Found more or less messages than expected: " ++ show res)
        (length res == 1)

\end{code}

\subsubsection{Change a trace's minimum severity}\label{code:unit_trace_min_severity}
A trace is configured with a minimum severity and filters out messages that are labelled
with a lower severity. This minimum severity of the current trace can be changed.
\begin{code}
unit_trace_min_severity :: Assertion
unit_trace_min_severity = do
    msgs <- STM.newTVarIO []
    trace@(ctx,_) <- setupTrace $ TraceConfiguration (TVarList msgs) "test min severity" Neutral Debug
    logInfo trace "Message #1"

    -- raise the minimum severity to Warning
    setMinSeverity (configuration ctx) Warning
    msev <- Cardano.BM.Configuration.minSeverity (configuration ctx)
    assertBool ("min severity should be Warning, but is " ++ (show msev))
               (msev == Warning)

    -- this message will not be traced
    logInfo trace "Message #2"

    -- lower the minimum severity to Info
    setMinSeverity (configuration ctx) Info
    -- this message is traced
    logInfo trace "Message #3"

    -- acquire the traced objects
    res <- STM.readTVarIO msgs

    -- only the first and last messages should have been traced
    assertBool
        ("Found more or less messages than expected: " ++ show res)
        (length res == 2)
    assertBool
        ("Found Info message when Warning was minimum severity: " ++ show res)
        (all (\case {(LogMessage (LogItem _ Info "Message #2")) -> False; _ -> True}) res)

\end{code}

\subsubsection{Define a subtrace's behaviour to duplicate all messages}\label{code:unit_trace_duplicate}
The |SubTrace| will duplicate all messages that pass through it. Each message will be in its own named
context.
\begin{code}
unit_trace_duplicate :: Assertion
unit_trace_duplicate = do
    msgs <- STM.newTVarIO []
    trace0@(ctx,_) <- setupTrace $ TraceConfiguration (TVarList msgs) "test duplicate" Neutral Debug
    logInfo trace0 "Message #1"

    -- create a subtrace which duplicates all messages
    setSubTrace (configuration ctx) "test duplicate.orig" $ Just (TeeTrace "dup")
    trace <- subTrace "orig" trace0

    -- this message will be duplicated
    logInfo trace "You will see me twice!"

    -- acquire the traced objects
    res <- STM.readTVarIO msgs

    -- only the first and last messages should have been traced
    assertBool
        ("Found more or less messages than expected: " ++ show res)
        (length res == 3)

\end{code}

\subsubsection{Change the minimum severity of a named context}\label{code:unit_named_min_severity}
A trace of a named context can be configured with a minimum severity, such that the trace will
filter out messages that are labelled with a lower severity.
\begin{code}
unit_named_min_severity :: Assertion
unit_named_min_severity = do
    msgs <- STM.newTVarIO []
    trace0 <- setupTrace $ TraceConfiguration (TVarList msgs) "test named severity" Neutral Debug
    trace@(ctx, _) <- appendName "sev-change" trace0
    logInfo trace "Message #1"

    -- raise the minimum severity to Warning
    setSeverity (configuration ctx) (loggerName ctx) (Just Warning)
    msev <- Cardano.BM.Configuration.inspectSeverity (configuration ctx) (loggerName ctx)
    assertBool ("min severity should be Warning, but is " ++ (show msev))
               (msev == Just Warning)
    -- this message will not be traced
    logInfo trace "Message #2"

    -- lower the minimum severity to Info
    setSeverity (configuration ctx) (loggerName ctx) (Just Info)
    -- this message is traced
    logInfo trace "Message #3"

    -- acquire the traced objects
    res <- STM.readTVarIO msgs

    -- only the first and last messages should have been traced
    assertBool
        ("Found more or less messages than expected: " ++ show res)
        (length res == 2)
    assertBool
        ("Found Info message when Warning was minimum severity: " ++ show res)
        (all (\case {(LogMessage (LogItem _ Info "Message #2")) -> False; _ -> True}) res)

\end{code}

\begin{code}
unit_hierarchy' :: [SubTrace] -> ([LogObject] -> Bool) -> Assertion
unit_hierarchy' subtraces f = do
    let (t1 : t2 : t3 : _) = cycle subtraces
    msgs <- STM.newTVarIO []
    -- create trace of type 1
    trace1 <- setupTrace $ TraceConfiguration (TVarList msgs) "test" t1 Debug
    logInfo trace1 "Message from level 1."

    -- subtrace of type 2
    setTransformer_ trace1 "inner" (Just t2)
    trace2 <- subTrace "inner" trace1
    logInfo trace2 "Message from level 2."

    -- subsubtrace of type 3
    setTransformer_ trace2 "innermost" (Just t3)
    _ <- STMObserver.bracketObserveIO trace2 "innermost" setVar_
    logInfo trace2 "Message from level 3."
    -- acquire the traced objects
    res <- STM.readTVarIO msgs

    -- only the first message should have been traced
    assertBool
        ("Found more or less messages than expected: " ++ show res)
        (f res)

\end{code}

\subsubsection{Logging in parallel}\label{code:unit_trace_in_fork}
\begin{code}
unit_trace_in_fork :: Assertion
unit_trace_in_fork = do
    msgs <- STM.newTVarIO []
    trace <- setupTrace $ TraceConfiguration (TVarListNamed msgs) "test" Neutral Debug
    trace0 <- appendName "work0" trace
    trace1 <- appendName "work1" trace
    work0 <- work trace0
    threadDelay 5000
    work1 <- work trace1
    Async.wait $ work0
    Async.wait $ work1

    res <- STM.readTVarIO msgs
    let names@(_: namesTail) = map lnName res
    -- each trace should have its own name and log right after the other
    assertBool
        ("Consecutive loggernames are not different: " ++ show names)
        (and $ zipWith (/=) names namesTail)
  where
    work :: Trace IO -> IO (Async.Async ())
    work trace = Async.async $ do
        logInfoDelay trace "1"
        logInfoDelay trace "2"
        logInfoDelay trace "3"
    logInfoDelay :: Trace IO -> Text -> IO ()
    logInfoDelay trace msg =
        logInfo trace msg >>
        threadDelay 10000

\end{code}

\subsubsection{Stress testing parallel logging}\label{code:stress_trace_in_fork}
\begin{code}
stress_trace_in_fork :: Assertion
stress_trace_in_fork = do
    msgs <- STM.newTVarIO []
    trace <- setupTrace $ TraceConfiguration (TVarListNamed msgs) "test" Neutral Debug
    let names = map (\a -> ("work-" <> pack (show a))) [1..(10::Int)]
    ts <- forM names $ \name -> do
        trace' <- appendName name trace
        work trace'
    forM_ ts Async.wait

    res <- STM.readTVarIO msgs
    let resNames = map lnName res
    let frequencyMap = fromListWith (+) [(x, 1) | x <- resNames]

    -- each trace should have traced 'totalMessages' messages
    assertBool
        ("Frequencies of logged messages according to loggername: " ++ show frequencyMap)
        (all (\name -> (lookup ("test." <> name) frequencyMap) == Just totalMessages) names)
  where
    work :: Trace IO -> IO (Async.Async ())
    work trace = Async.async $ forM_ [1..totalMessages] $ (logInfo trace) . pack . show
    totalMessages :: Int
    totalMessages = 10

\end{code}

\subsubsection{Dropping |ObserveOpen| messages in a subtrace}\label{code:unit_noOpening_Trace}
\begin{code}
unit_noOpening_Trace :: Assertion
unit_noOpening_Trace = do
    msgs <- STM.newTVarIO []
    logTrace <- setupTrace $ TraceConfiguration (TVarList msgs) "test" DropOpening Debug
    _ <- STMObserver.bracketObserveIO logTrace "setTVar" setVar_
    res <- STM.readTVarIO msgs
    assertBool
        ("Found non-expected ObserveOpen message: " ++ show res)
        (all (\case {ObserveOpen _ -> False; _ -> True}) res)

\end{code}

\subsubsection{Assert maximum length of log context name}\label{code:unit_append_name}
The name of the log context cannot grow beyond a maximum number of characters, currently
the limit is set to 80.
\begin{code}
unit_append_name :: Assertion
unit_append_name = do
    cfg <- defaultConfigTesting
    trace0 <- Setup.setupTrace (Right cfg) "test"
    trace1 <- appendName bigName trace0
    (ctx2, _) <- appendName bigName trace1

    assertBool
        ("Found logger name with more than 80 chars: " ++ show (loggerName ctx2))
        (T.length (loggerName ctx2) <= 80)
  where
    bigName = T.replicate 30 "abcdefghijklmnopqrstuvwxyz"

\end{code}

\begin{code}
setVar_ :: STM.STM Integer
setVar_ = do
    t <- STM.newTVar 0
    STM.writeTVar t 42
    res <- STM.readTVar t
    return res

\end{code}

\subsubsection{Testing log context name filters}\label{code:unit_name_filtering}
\begin{code}
unit_name_filtering :: Assertion
unit_name_filtering = do
    let contextName = "test.sub.1"
    let loname = "sum"
    -- let logobject = LogValue loname (PureD 42.0)

    let filter1 = [ Drop (Exact "test.sub.1") ]
    assertBool ("Dropping a specific name should filter it out and thus return False")
               (False == evalFilters filter1 contextName)
    let filter2 = [ Drop (EndsWith ".1") ]
    assertBool ("Dropping a name ending with a specific text should filter out the context name and thus return False")
               (False == evalFilters filter2 contextName)
    let filter3 = [ Drop (StartsWith "test.") ]
    assertBool ("Dropping a name starting with a specific text should filter out the context name and thus return False")
               (False == evalFilters filter3 contextName)
    let filter4 = [ Drop (Contains ".sub.") ]
    assertBool ("Dropping a name starting containing a specific text should filter out the context name and thus return False")
               (False == evalFilters filter4 contextName)
    let filter5 = [ Drop (StartsWith "test."),
                    Unhide (Exact "test.sub.1") ]
    assertBool ("Dropping all and unhiding a specific name should the context name allow passing the filter")
               (True == evalFilters filter5 contextName)
    let filter6 = [ Drop (StartsWith "test."),
                    Unhide (EndsWith ".sum") ]
    assertBool ("Dropping all and unhiding a named value, the LogObject should pass the filter")
               (True == evalFilters filter6 (contextName <> "." <> loname))
    let filter7 = [ Drop (StartsWith "test."),
                    Unhide (EndsWith ".product") ]
    assertBool ("Dropping all and unhiding an inexistant named value, the LogObject should not pass the filter")
               (False == evalFilters filter7 (contextName <> "." <> loname))

\end{code}
