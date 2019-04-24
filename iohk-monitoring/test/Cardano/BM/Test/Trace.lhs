
\subsection{Cardano.BM.Test.Trace}
\label{code:Cardano.BM.Test.Trace}

%if style == newcode
\begin{code}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.BM.Test.Trace (
    TraceConfiguration (..)
  , tests
  ) where

import           Prelude hiding (lookup)

import qualified Control.Concurrent.STM.TVar as STM

import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Monad (forM, forM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.Either (isLeft, isRight)
import           Data.Map (fromListWith, lookup)
import           Data.Text (Text, append, pack)
import qualified Data.Text as T
#ifdef ENABLE_OBSERVABLES
import qualified Control.Monad.STM as STM
import           Data.Unique (newUnique)
#endif
import           System.Directory (getTemporaryDirectory, removeFile)
import           System.Mem (performMajorGC)
import           System.FilePath ((</>))

import           Cardano.BM.Configuration (Configuration, inspectSeverity,
                     minSeverity, setMinSeverity, setSeverity)
import           Cardano.BM.Configuration.Model (empty, setDefaultBackends,
                     setDefaultScribes, setSubTrace, setSetupBackends,
                     setSetupScribes)
import           Cardano.BM.Configuration.Static
import           Cardano.BM.Data.BackendKind (BackendKind (..))
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Observable
import           Cardano.BM.Data.Output
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.SubTrace
import           Cardano.BM.Output.Switchboard (MockSwitchboard (..))
#ifdef ENABLE_OBSERVABLES
import           Cardano.BM.Counters (diffTimeObserved, getMonoClock)
import           Cardano.BM.Data.Aggregated
import           Cardano.BM.Data.Counter
import qualified Cardano.BM.Observer.Monadic as MonadicObserver
import qualified Cardano.BM.Observer.STM as STMObserver
#endif
import           Cardano.BM.Output.Switchboard (traceMock)
import qualified Cardano.BM.Setup as Setup
import           Cardano.BM.Trace (Trace, appendName, evalFilters, logDebug,
                     logInfo, logInfoS, logNotice, logWarning, logError,
                     logCritical, logAlert, logEmergency)

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (Assertion, assertBool, testCase,
                     testCaseInfo)

\end{code}
%endif

\begin{code}
tests :: TestTree
tests = testGroup "Testing Trace" [
        unit_tests
      , testCase "forked traces stress testing" stressTraceInFork
#ifdef ENABLE_OBSERVABLES
      , testCase "stress testing: ObservableTrace vs. NoTrace" timingObservableVsUntimed
#endif
      , testCaseInfo "demonstrating logging" simpleDemo
      , testCaseInfo "demonstrating nested named context logging" exampleWithNamedContexts
      ]

unit_tests :: TestTree
unit_tests = testGroup "Unit tests" [
        testCase "opening messages should not be traced" unitNoOpeningTrace
    --   , testCase "hierarchy of traces" unitHierarchy
      , testCase "forked traces" unitTraceInFork
      , testCase "hierarchy of traces with NoTrace" $
            unitHierarchy' [Neutral, NoTrace, (ObservableTrace observablesSet)]
                onlyLevelOneMessage
      , testCase "hierarchy of traces with DropOpening" $
            unitHierarchy' [Neutral, DropOpening, (ObservableTrace observablesSet)]
                notObserveOpen
      , testCase "hierarchy of traces with UntimedTrace" $
            unitHierarchy' [Neutral, UntimedTrace, UntimedTrace]
                observeNoMeasures
      , testCase "changing the minimum severity of a trace at runtime"
            unitTraceMinSeverity
      , testCase "changing the minimum severity of a named context at runtime"
            unitNamedMinSeverity
      , testCase "appending names" unitAppendName
      , testCase "create subtrace which duplicates messages" unitTraceDuplicate
      , testCase "testing name filtering" unitNameFiltering
      , testCase "testing throwing of exceptions" unitExceptionThrowing
      , testCase "NoTrace: check lazy evaluation" unitTestLazyEvaluation
      , testCase "private messages should not be logged into private files" unitLoggingPrivate
      ]
      where
        observablesSet = [MonotonicClock, MemoryStats]
        notObserveOpen :: [LogObject a] -> Bool
        notObserveOpen = all (\case {LogObject _ _ (ObserveOpen _) -> False; _ -> True})
        notObserveClose :: [LogObject a] -> Bool
        notObserveClose = all (\case {LogObject _ _ (ObserveClose _) -> False; _ -> True})
        notObserveDiff :: [LogObject a] -> Bool
        notObserveDiff = all (\case {LogObject _ _ (ObserveDiff _) -> False; _ -> True})
        onlyLevelOneMessage :: [LogObject Text] -> Bool
        onlyLevelOneMessage = \case
            [LogObject _ _ (LogMessage "Message from level 1.")] -> True
            _                                                    -> False
        observeNoMeasures :: [LogObject a] -> Bool
        observeNoMeasures obs = notObserveOpen obs && notObserveClose obs && notObserveDiff obs

\end{code}

\subsubsection{Helper routines}
\begin{code}
data TraceConfiguration = TraceConfiguration
    { tcConfig       :: Configuration
    , tcOutputKind   :: MockSwitchboard Text
    , tcName         :: LoggerName
    , tcSubTrace     :: SubTrace
    }

setupTrace :: TraceConfiguration -> IO (Trace IO Text)
setupTrace (TraceConfiguration cfg mockSB name subTr) = do
    let logTrace = traceMock mockSB cfg

    setSubTrace cfg name (Just subTr)
    appendName name logTrace

\end{code}

\subsubsection{Simple demo of logging.}\label{code:simpleDemo}
\begin{code}
simpleDemo :: IO String
simpleDemo = do
    cfg <- defaultConfigTesting
    logTrace :: Trace IO String <- Setup.setupTrace (Right cfg) "test"
    putStrLn "\n"

    logDebug     logTrace "This is how a Debug message looks like."
    logInfo      logTrace "This is how an Info message looks like."
    logNotice    logTrace "This is how a Notice message looks like."
    logWarning   logTrace "This is how a Warning message looks like."
    logError     logTrace "This is how an Error message looks like."
    logCritical  logTrace "This is how a Critical message looks like."
    logAlert     logTrace "This is how an Alert message looks like."
    logEmergency logTrace "This is how an Emergency message looks like."

    return ""

\end{code}

\subsubsection{Example of using named contexts with |Trace|}\label{code:exampleWithNamedContexts}
\begin{code}
exampleWithNamedContexts :: IO String
exampleWithNamedContexts = do
    cfg <- defaultConfigTesting
    Setup.withTrace cfg "test" $ \(logTrace :: Trace IO Text) -> do
        putStrLn "\n"
        logInfo logTrace "entering"
        logTrace0 <- appendName "simple-work-0" logTrace
        work0 <- complexWork0 cfg logTrace0 "0"
        logTrace1 <- appendName "complex-work-1" logTrace
        work1 <- complexWork1 cfg logTrace1 "42"

        Async.wait work0
        Async.wait work1
        -- the named context will include "complex" in the logged message
        logInfo logTrace "done."
        threadDelay 100000
        -- force garbage collection to allow exceptions to be thrown
        performMajorGC
        threadDelay 100000

    return ""
  where
    complexWork0 _ tr msg = Async.async $ logInfo tr ("let's see (0): " `append` msg)
    complexWork1 cfg tr msg = Async.async $ do
        logInfo tr ("let's see (1): " `append` msg)
        trInner <- appendName "inner-work-1" tr
        let observablesSet = [MonotonicClock]
        setSubTrace cfg "test.complex-work-1.inner-work-1.STM-action" $
            Just $ ObservableTrace observablesSet
#ifdef ENABLE_OBSERVABLES
        _ <- STMObserver.bracketObserveIO cfg trInner Debug "STM-action" setVar_
#endif
        logInfo trInner "let's see: done."

\end{code}

\subsubsection{Show effect of turning off observables}
\begin{code}
#ifdef ENABLE_OBSERVABLES
runTimedAction :: Configuration -> Trace IO Text -> LoggerName -> Int -> IO Measurable
runTimedAction cfg logTrace name reps = do
    runid <- newUnique
    t0 <- getMonoClock
    forM_ [(1::Int)..reps] $ const $ observeAction logTrace
    t1 <- getMonoClock
    return $ diffTimeObserved (CounterState runid t0) (CounterState runid t1)
  where
    observeAction trace = do
        _ <- MonadicObserver.bracketObserveIO cfg trace Debug name action
        return ()
    action = return $ forM [1::Int ..100] $ \x -> [x] ++ (init $ reverse [1::Int ..10000])

timingObservableVsUntimed :: Assertion
timingObservableVsUntimed = do
    cfg1 <- defaultConfigTesting
    msgs1 <- STM.newTVarIO []
    traceObservable <- setupTrace $ TraceConfiguration cfg1
                                    (MockSB msgs1)
                                    "observables"
                                    (ObservableTrace observablesSet)
    cfg2 <- defaultConfigTesting
    msgs2 <- STM.newTVarIO []
    traceUntimed <- setupTrace $ TraceConfiguration cfg2
                                    (MockSB msgs2)
                                    "no timing"
                                    UntimedTrace
    cfg3 <- defaultConfigTesting
    msgs3 <- STM.newTVarIO []
    traceNoTrace <- setupTrace $ TraceConfiguration cfg3
                                    (MockSB msgs3)
                                    "no trace"
                                    NoTrace

    t_observable <- runTimedAction cfg1 traceObservable "observables" 100
    t_untimed    <- runTimedAction cfg2 traceUntimed    "no timing"   100
    t_notrace    <- runTimedAction cfg3 traceNoTrace    "no trace"    100

    ms <- STM.readTVarIO msgs1

    assertBool
        ("Untimed consumed more time than ObservableTrace " ++ (show [t_untimed, t_observable]) ++ show ms)
        (t_observable > t_untimed && not (null ms))
    assertBool
        ("NoTrace consumed more time than ObservableTrace" ++ (show [t_notrace, t_observable]))
        (t_observable > t_notrace)
    assertBool
        ("NoTrace consumed more time than Untimed" ++ (show [t_notrace, t_untimed]))
        True
  where
    observablesSet = [MonotonicClock, GhcRtsStats, MemoryStats, IOStats, ProcessStats]
#endif
\end{code}

\subsubsection{Control tracing in a hierarchy of |Trace|s}\label{code:unitHierarchy}
We can lay out traces in a hierarchical manner, that the children
forward traced items to the parent |Trace|.
A |NoTrace| introduced in this hierarchy will cut off a branch
from messaging to the root.
\begin{code}
_unitHierarchy :: Assertion
_unitHierarchy = do
    cfg <- defaultConfigTesting
    msgs <- STM.newTVarIO []
    basetrace <- setupTrace $ TraceConfiguration cfg (MockSB msgs) "test" Neutral
    logInfo basetrace "This should have been displayed!"

    -- subtrace of trace which traces nothing
    setSubTrace cfg "test.inner" (Just NoTrace)

    trace1 <- appendName "inner" basetrace
    logInfo trace1 "This should NOT have been displayed!"

    setSubTrace cfg "test.inner.innermost" (Just Neutral)
    trace2 <- appendName "innermost" trace1
    logInfo trace2 "This should NOT have been displayed also due to the trace one level above!"

    -- acquire the traced objects
    res <- STM.readTVarIO msgs

    -- only the first message should have been traced
    assertBool
        ("Found more or less messages than expected: " ++ show res)
        (length res == 1)

\end{code}

\subsubsection{Change a trace's minimum severity}\label{code:unitTraceMinSeverity}
A trace is configured with a minimum severity and filters out messages that are labelled
with a lower severity. This minimum severity of the current trace can be changed.
\begin{code}
unitTraceMinSeverity :: Assertion
unitTraceMinSeverity = do
    cfg <- defaultConfigTesting
    msgs <- STM.newTVarIO []
    trace <- setupTrace $ TraceConfiguration cfg (MockSB msgs) "test min severity" Neutral
    logInfo trace "Message #1"

    -- raise the minimum severity to Warning
    setMinSeverity cfg Warning
    msev <- Cardano.BM.Configuration.minSeverity cfg
    assertBool ("min severity should be Warning, but is " ++ (show msev))
               (msev == Warning)

    -- this message will not be traced
    logInfo trace "Message #2"

    -- lower the minimum severity to Info
    setMinSeverity cfg Info
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
        (all
            (\case
                LogObject _ (LOMeta _ _ Info _) (LogMessage "Message #2") -> False
                _ -> True)
            res)

\end{code}

\subsubsection{Define a subtrace's behaviour to duplicate all messages}\label{code:unitTraceDuplicate}
The |SubTrace| will duplicate all messages that pass through it. Each message will be in its own named
context.
\begin{code}
unitTraceDuplicate :: Assertion
unitTraceDuplicate = do
    cfg <- defaultConfigTesting
    msgs <- STM.newTVarIO []
    basetrace <- setupTrace $ TraceConfiguration cfg (MockSB msgs) "test-duplicate" Neutral
    logInfo basetrace "Message #1"

    -- create a subtrace which duplicates all messages
    setSubTrace cfg "test-duplicate.orig" $ Just (TeeTrace "test-duplicate.dup")
    trace <- appendName "orig" basetrace

    -- this message will be duplicated
    logInfo trace "You will see me twice!"

    -- acquire the traced objects
    res <- STM.readTVarIO msgs

    -- only the first and last messages should have been traced
    assertBool
        ("Found more or less messages than expected: " ++ show res)
        (length res == 3)

\end{code}

\subsubsection{Change the minimum severity of a named context}\label{code:unitNamedMinSeverity}
A trace of a named context can be configured with a minimum severity, such that the trace will
filter out messages that are labelled with a lower severity.
\begin{code}
unitNamedMinSeverity :: Assertion
unitNamedMinSeverity = do
    cfg <- defaultConfigTesting
    msgs <- STM.newTVarIO []
    basetrace <- setupTrace $ TraceConfiguration cfg (MockSB msgs) "test-named-severity" Neutral
    trace <- appendName "sev-change" basetrace
    logInfo trace "Message #1"

    -- raise the minimum severity to Warning
    setSeverity cfg "test-named-severity.sev-change" (Just Warning)
    msev <- Cardano.BM.Configuration.inspectSeverity cfg "test-named-severity.sev-change"
    assertBool ("min severity should be Warning, but is " ++ (show msev))
               (msev == Just Warning)
    -- this message will not be traced
    logInfo trace "Message #2"

    -- lower the minimum severity to Info
    setSeverity cfg "test-named-severity.sev-change" (Just Info)
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
        (all
            (\case
                LogObject _ (LOMeta _ _ Info _) (LogMessage "Message #2") -> False
                _ -> True)
            res)

\end{code}

\begin{code}
unitHierarchy' :: [SubTrace] -> ([LogObject Text] -> Bool) -> Assertion
unitHierarchy' subtraces f = do
    cfg <- liftIO Cardano.BM.Configuration.Model.empty
    let (t1 : t2 : t3 : _) = cycle subtraces
    msgs <- STM.newTVarIO []
    -- create trace of type 1
    trace1 <- setupTrace $ TraceConfiguration cfg (MockSB msgs) "test" t1
    logInfo trace1 "Message from level 1."

    -- subtrace of type 2
    setSubTrace cfg "test.inner" (Just t2)
    trace2 <- appendName "inner" trace1
    logInfo trace2 "Message from level 2."

    -- subsubtrace of type 3
    setSubTrace cfg "test.inner.innermost" (Just t3)
#ifdef ENABLE_OBSERVABLES
    _ <- STMObserver.bracketObserveIO cfg trace2 Debug "innermost" setVar_
#endif
    logInfo trace2 "Message from level 3."
    -- acquire the traced objects
    res <- STM.readTVarIO msgs

    -- only the first message should have been traced
    assertBool
        ("Found more or less messages than expected: " ++ show res)
        (f res)

\end{code}

\subsubsection{Logging in parallel}\label{code:unitTraceInFork}
\begin{code}
unitTraceInFork :: Assertion
unitTraceInFork = do
    cfg <- defaultConfigTesting
    msgs <- STM.newTVarIO []
    trace <- setupTrace $ TraceConfiguration cfg (MockSB msgs) "test" Neutral
    trace0 <- appendName "work0" trace
    trace1 <- appendName "work1" trace
    work0 <- work trace0
    threadDelay 5000
    work1 <- work trace1
    Async.wait $ work0
    Async.wait $ work1

    res <- STM.readTVarIO msgs
    let names@(_: namesTail) = map loName res
    -- each trace should have its own name and log right after the other
    assertBool
        ("Consecutive loggernames are not different: " ++ show names)
        (and $ zipWith (/=) names namesTail)
  where
    work :: Trace IO Text -> IO (Async.Async ())
    work trace = Async.async $ do
        logInfoDelay trace "1"
        logInfoDelay trace "2"
        logInfoDelay trace "3"
    logInfoDelay :: Trace IO Text -> Text -> IO ()
    logInfoDelay trace msg =
        logInfo trace msg >>
        threadDelay 10000

\end{code}

\subsubsection{Stress testing parallel logging}\label{code:stressTraceInFork}
\begin{code}
stressTraceInFork :: Assertion
stressTraceInFork = do
    cfg <- defaultConfigTesting
    msgs <- STM.newTVarIO []
    trace <- setupTrace $ TraceConfiguration cfg (MockSB msgs) "test" Neutral
    let names = map (\a -> ("work-" <> pack (show a))) [1..(10::Int)]
    ts <- forM names $ \name -> do
        trace' <- appendName name trace
        work trace'
    forM_ ts Async.wait

    res <- STM.readTVarIO msgs
    let resNames = map loName res
    let frequencyMap = fromListWith (+) [(x, 1) | x <- resNames]

    -- each trace should have traced totalMessages' messages
    assertBool
        ("Frequencies of logged messages according to loggername: " ++ show frequencyMap)
        (all (\name -> (lookup ("test." <> name) frequencyMap) == Just totalMessages) names)
  where
    work :: Trace IO Text -> IO (Async.Async ())
    work trace = Async.async $ forM_ [1..totalMessages] $ (logInfo trace) . pack . show
    totalMessages :: Int
    totalMessages = 10

\end{code}

\subsubsection{Dropping |ObserveOpen| messages in a subtrace}\label{code:unitNoOpeningTrace}
\begin{code}
unitNoOpeningTrace :: Assertion
unitNoOpeningTrace = do
    cfg <- defaultConfigTesting
    msgs <- STM.newTVarIO []
#ifdef ENABLE_OBSERVABLES
    logTrace <- setupTrace $ TraceConfiguration cfg (MockSB msgs) "test" DropOpening
    _ <- STMObserver.bracketObserveIO cfg logTrace Debug "setTVar" setVar_
#endif
    res <- STM.readTVarIO msgs
    assertBool
        ("Found non-expected ObserveOpen message: " ++ show res)
        (all (\case {LogObject _ _ (ObserveOpen _) -> False; _ -> True}) res)

\end{code}

\subsubsection{Assert maximum length of log context name}\label{code:unitAppendName}
The name of the log context cannot grow beyond a maximum number of characters, currently
the limit is set to 80.
\begin{code}
unitAppendName :: Assertion
unitAppendName = do
    cfg <- defaultConfigTesting
    msgs <- STM.newTVarIO []
    basetrace <- setupTrace $ TraceConfiguration cfg (MockSB msgs) "test" Neutral
    trace1 <- appendName bigName basetrace
    trace2 <- appendName bigName trace1
    forM_ [basetrace, trace1, trace2] $ (flip logInfo msg)
    res <- reverse <$> STM.readTVarIO msgs
    let loggernames = map loName res
    assertBool
        ("AppendName did not work properly. The loggernames for the messages are: " ++
            show loggernames)
        (loggernames == [ "test"
                        , "test." <> bigName
                        , "test." <> bigName <> "." <> bigName
                        ])
  where
    bigName = T.replicate 30 "abcdefghijklmnopqrstuvwxyz"
    msg = "Hello!"

\end{code}

\begin{code}
#ifdef ENABLE_OBSERVABLES
setVar_ :: STM.STM Integer
setVar_ = do
    t <- STM.newTVar 0
    STM.writeTVar t 42
    res <- STM.readTVar t
    return res
#endif
\end{code}

\subsubsection{Testing log context name filters}\label{code:unitNameFiltering}
\begin{code}
unitNameFiltering :: Assertion
unitNameFiltering = do
    let contextName = "test.sub.1"
    let loname = "sum"  -- would be part of a "LogValue loname 42"

    let filter1 = [ (Drop (Exact "test.sub.1"), Unhide []) ]
    assertBool ("Dropping a specific name should filter it out and thus return False")
               (False == evalFilters filter1 contextName)
    let filter2 = [ (Drop (EndsWith ".1"), Unhide []) ]
    assertBool ("Dropping a name ending with a specific text should filter out the context name and thus return False")
               (False == evalFilters filter2 contextName)
    let filter3 = [ (Drop (StartsWith "test."), Unhide []) ]
    assertBool ("Dropping a name starting with a specific text should filter out the context name and thus return False")
               (False == evalFilters filter3 contextName)
    let filter4 = [ (Drop (Contains ".sub."), Unhide []) ]
    assertBool ("Dropping a name starting containing a specific text should filter out the context name and thus return False")
               (False == evalFilters filter4 contextName)
    let filter5 = [ (Drop (StartsWith "test."),
                     Unhide [(Exact "test.sub.1")]) ]
    assertBool ("Dropping all and unhiding a specific name should the context name allow passing the filter")
               (True == evalFilters filter5 contextName)
    let filter6 = [ (Drop (StartsWith "test."),
                     Unhide [(EndsWith ".sum"),
                             (EndsWith ".other")]) ]
    assertBool ("Dropping all and unhiding some names, the LogObject should pass the filter")
               (True == evalFilters filter6 (contextName <> "." <> loname))
    let filter7 = [ (Drop (StartsWith "test."),
                     Unhide [(EndsWith ".product")]) ]
    assertBool ("Dropping all and unhiding an inexistant named value, the LogObject should not pass the filter")
               (False == evalFilters filter7 (contextName <> "." <> loname))
    let filter8 = [ (Drop (StartsWith "test."),
                     Unhide [(Exact "test.sub.1")]),
                    (Drop (StartsWith "something.else."),
                     Unhide [(EndsWith ".this")]) ]
    assertBool ("Disjunction of filters that should pass")
               (True == evalFilters filter8 contextName)
    let filter9 = [ (Drop (StartsWith "test."),
                     Unhide [(Exact ".that")]),
                    (Drop (StartsWith "something.else."),
                     Unhide [(EndsWith ".this")]) ]
    assertBool ("Disjunction of filters that should not pass")
               (False == evalFilters filter9 contextName)

\end{code}

\subsubsection{Exception throwing}\label{code:unitExceptionThrowing}
Exceptions encountered should be thrown. Lazy evaluation is really happening!
This test fails if run with a configuration |defaultConfigTesting|, because
this one will ignore all traced messages.
\begin{code}
unitExceptionThrowing :: Assertion
unitExceptionThrowing = do

    action <- work msg

    res <- Async.waitCatch action

    assertBool
        ("Exception should have been rethrown")
        (isLeft res)
  where
    msg :: Text
    msg = error "faulty message"
    work :: Text -> IO (Async.Async ())
    work message = Async.async $ do
        cfg <- defaultConfigStdout
        trace <- Setup.setupTrace (Right cfg) "test"

        logInfo trace message
        threadDelay 10000

\end{code}

\subsubsection{Check lazy evaluation of trace}\label{code:unitTestLazyEvaluation}
Exception should not be thrown when type of |Trace| is |NoTrace|.
\begin{code}
unitTestLazyEvaluation :: Assertion
unitTestLazyEvaluation = do

    action <- work msg

    res <- Async.waitCatch action

    assertBool
        ("Exception should not have been rethrown when type of Trace is NoTrace")
        (isRight res)
  where
    msg :: Text
    msg = error "faulty message"
    work :: Text -> IO (Async.Async ())
    work message = Async.async $ do
        cfg <- defaultConfigTesting
        basetrace <- Setup.setupTrace (Right cfg) "test"
        setSubTrace cfg "test.work" (Just NoTrace)
        trace <- appendName "work" basetrace

        logInfo trace message

\end{code}

\subsubsection{Check that private messages do not end up in public log files.}\label{code:unitLoggingPrivate}
\begin{code}
unitLoggingPrivate :: Assertion
unitLoggingPrivate = do

    tmpDir <- getTemporaryDirectory
    let privateFile = tmpDir </> "private.log"
        publicFile  = tmpDir </> "public.log"

    conf <- empty
    setDefaultBackends conf [KatipBK]
    setSetupBackends conf [KatipBK]
    setDefaultScribes conf [ "FileTextSK::" <> pack privateFile
                           , "FileTextSK::" <> pack publicFile
                           ]
    setSetupScribes conf [ ScribeDefinition
                            { scKind     = FileTextSK
                            , scName     = pack privateFile
                            , scPrivacy  = ScPrivate
                            , scRotation = Nothing
                            }
                         , ScribeDefinition
                            { scKind     = FileTextSK
                            , scName     = pack publicFile
                            , scPrivacy  = ScPublic
                            , scRotation = Nothing
                            }
                         ]

    Setup.withTrace conf "test" $ \trace -> do
        -- should log in both files
        logInfo  trace message
        -- should only log in private file
        logInfoS trace message

    countPublic  <- length . lines <$> readFile publicFile
    countPrivate <- length . lines <$> readFile privateFile

    -- delete files
    forM_ [privateFile, publicFile] removeFile

    assertBool
        ("Confidential file should contain 2 lines and it contains " ++ show countPrivate ++ ".\n" ++
         "Public file should contain 1 line and it contains "   ++ show countPublic  ++ ".\n"
        )
        (countPublic == 1 && countPrivate == 2)
  where
    message :: Text
    message = "Just a message"

\end{code}
