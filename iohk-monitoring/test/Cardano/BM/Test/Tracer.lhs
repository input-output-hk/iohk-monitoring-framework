
\subsection{Cardano.BM.Test.Tracer}
\label{code:Cardano.BM.Test.Tracer}

%if style == newcode
\begin{code}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Cardano.BM.Test.Tracer (
    tests
  ) where

import qualified Control.Concurrent.STM.TVar as STM
import           Control.Monad (forM_, void, when)
import           Data.Text (Text, pack, unpack)

import           Cardano.BM.Configuration.Static
import           Cardano.BM.Configuration (Configuration)
import           Cardano.BM.Configuration.Model (setSubTrace)
import           Cardano.BM.Internal.ElidingTracer
import           Cardano.BM.Data.Aggregated (Measurable(PureI))
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.SubTrace
import           Cardano.BM.Data.Tracer
import           Cardano.BM.Test.Mock (MockSwitchboard (..), traceMock)
import           Cardano.BM.Trace (Trace, appendName, traceNamedObject)

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (Assertion, assertBool, testCase)

\end{code}
%endif

\begin{code}
tests :: TestTree
tests = testGroup "Testing Extensions to Tracer" [
            testCase "simple tracing of messages in a named context" tracingInNamedContext,
            testCase "tracing with privacy and severity annotation" tracingWithPrivacyAndSeverityAnnotation,
            testCase "tracing with a predicate filter" tracingWithPredicateFilter,
            testCase "tracing with a filter that is evaluated in a monad" tracingWithMonadicFilter,
            testCase "tracing with filtering for both severity and privacy" tracingWithComplexFiltering,
            testCase "eliding equivalent messages on tracer" tracingElidedMessages,
            testCase "eliding equivalent messages only one" tracingElidedMessages1,
            testCase "eliding equivalent messages only two" tracingElidedMessages2,
            testCase "eliding equivalent messages from three" tracingElidedMessages3,
            testCase "eliding messages, output after n repeats" tracingElidedMessagesRepeat
        ]

\end{code}

\subsubsection{Helper routines}
\begin{code}
data TraceConfiguration = TraceConfiguration
    { _tcConfig       :: Configuration
    , _tcOutputKind   :: MockSwitchboard Text
    , _tcName         :: LoggerName
    , _tcSubTrace     :: SubTrace
    }

setupMockTrace :: TraceConfiguration -> IO (Trace IO Text)
setupMockTrace (TraceConfiguration cfg mockSB name subTr) = do
    let logTrace = traceMock mockSB cfg

    setSubTrace cfg name (Just subTr)
    return $ appendName name logTrace

\end{code}

\begin{code}
renderNamedItemTracing' :: Show a => Tracer m String -> Trace m a
renderNamedItemTracing' = contramap $ \(ctx,item) ->
    unpack ctx ++ ": " ++ show (loContent item) ++ ", (meta): " ++ show (loMeta item)

\end{code}

\subsubsection{Tracing messages in a named context}
\begin{code}
tracingInNamedContext :: Assertion
tracingInNamedContext = do
    let logTrace = appendName "named" $ renderNamedItemTracing' $ stdoutTracer

    void $ callFun2 logTrace

    assertBool "OK" True

callFun2 :: Trace IO Text -> IO Int
callFun2 logTrace = do
    let logTrace' = appendName "fun2" logTrace
    traceWith (toLogObject logTrace') ("in function 2" :: Text)
    callFun3 logTrace'

callFun3 :: Trace IO Text -> IO Int
callFun3 logTrace = do
    traceWith (toLogObject $ appendName "fun3" $ logTrace) ("in function 3" :: Text)
    return 42

\end{code}

\subsubsection{Tracing messages with pricacy and severity annotation}
A |Tracer| transformer creating a |LogObject| from |PrivacyAndSeverityAnnotated|.
\begin{code}
logObjectFromAnnotated :: Show a
    => Trace IO a
    -> Tracer IO (PrivacyAndSeverityAnnotated a)
logObjectFromAnnotated tr = Tracer $ \(PSA sev priv a) -> do
    lometa <- mkLOMeta sev priv
    traceWith tr $ (mempty, LogObject mempty lometa (LogMessage a))

\end{code}

\begin{code}
tracingWithPrivacyAndSeverityAnnotation :: Assertion
tracingWithPrivacyAndSeverityAnnotation = do
    let logTrace =
            logObjectFromAnnotated $ appendName "example3" $ renderNamedItemTracing' stdoutTracer

    traceWith logTrace $ PSA Info Confidential ("Hello" :: String)
    traceWith logTrace $ PSA Warning Public "World"

    assertBool "OK" True
\end{code}

\subsubsection{Filter Tracer}
\begin{code}
filterAppendNameTracing :: Monad m
    => m ((LoggerName, LogObject a) -> Bool)
    -> LoggerName
    -> Trace m a
    -> Trace m a
filterAppendNameTracing test name = (appendName name) . (condTracingM test)

tracingWithPredicateFilter :: Assertion
tracingWithPredicateFilter = do
    let appendF = filterAppendNameTracing oracle
        logTrace :: Trace IO Text = appendF "example4" (renderNamedItemTracing' stdoutTracer)

    traceWith (toLogObject logTrace) ("Hello" :: String)

    let logTrace' = appendF "inner" logTrace
    traceWith (toLogObject logTrace') ("World" :: String)

    let logTrace'' = appendF "innest" logTrace'
    traceWith (toLogObject logTrace'') ("!!" :: String)

    assertBool "OK" True
  where
    oracle :: Monad m => m ((LoggerName, LogObject a) -> Bool)
    oracle = return $ \(ctx, _lo) -> ctx /= "example4.inner"

\end{code}

\begin{code}
-- severity anotated
tracingWithMonadicFilter :: Assertion
tracingWithMonadicFilter = do
    let logTrace =
            condTracingM oracle $
                logObjectFromAnnotated $
                    appendName "test5" $ renderNamedItemTracing' stdoutTracer

    traceWith logTrace $ PSA Debug Confidential ("Hello"::String)
    traceWith logTrace $ PSA Warning Public "World"

    assertBool "OK" True
  where
    oracle :: Monad m => m (PrivacyAndSeverityAnnotated a -> Bool)
    oracle = return $ \(PSA sev _priv _) -> (sev > Debug)

\end{code}

tracing with combined filtering for name and severity
\begin{code}
tracingWithComplexFiltering :: Assertion
tracingWithComplexFiltering = do
    let logTrace0 =  -- the basis, will output using the local renderer to stdout
            appendName "test6" $ renderNamedItemTracing' stdoutTracer
        logTrace1 =  -- the trace from |Privacy...Annotated| to |LogObject|
            condTracingM oracleSev $ logObjectFromAnnotated $ logTrace0
        logTrace2 =
            appendName "row" $ condTracingM oracleName $ logTrace0
        logTrace3 =  -- oracle should eliminate messages from this trace
            appendName "raw" $ condTracingM oracleName $ logTrace0

    traceWith logTrace1 $ PSA Debug Confidential ("Hello" :: String)
    traceWith logTrace1 $ PSA Warning Public "World"
    lometa <- mkLOMeta Info Public
    traceWith logTrace2 $ (mempty, LogObject mempty lometa (LogMessage ", RoW!"))
    traceWith logTrace3 $ (mempty, LogObject mempty lometa (LogMessage ", RoW!"))

    assertBool "OK" True
  where
    oracleSev :: Monad m => m (PrivacyAndSeverityAnnotated a -> Bool)
    oracleSev = return $ \(PSA sev _priv _) -> (sev > Debug)
    oracleName :: Monad m => m ((LoggerName, LogObject a) -> Bool)
    oracleName = return $ \(ctx, _lo) -> (ctx == "row")

\end{code}

Tracer transformer for eliding repeated messages
\begin{code}

data MsgTy = Item1 Int
           | Elided1 Int
           | Elided2 Int
           deriving (Show)

instance HasSeverityAnnotation MsgTy
instance HasPrivacyAnnotation MsgTy
instance Transformable Text IO MsgTy where
    trTransformer _verb tr = Tracer $ \s -> do
        meta <- mkLOMeta (getSeverityAnnotation s) (getPrivacyAnnotation s)
        traceWith tr ("", LogObject mempty
                                    meta
                                    (LogMessage $ pack $ show s))

instance ElidingTracer (WithSeverity MsgTy) where
    -- only |Elided1| and |Elided2| can be elided
    doelide (WithSeverity _s (Elided1 _)) = True
    doelide (WithSeverity _s (Elided2 _)) = True
    doelide _ = False
    -- any |Elided1| is equivalent to another |Elided1|
    isEquivalent (WithSeverity _ (Elided1 _)) (WithSeverity _ (Elided1 _)) = True
    -- instances of |Elided2| are equivalent if they are equal
    isEquivalent (WithSeverity _ (Elided2 n1)) (WithSeverity _ (Elided2 n2)) = n1 == n2
    isEquivalent _ _ = False
    conteliding _tverb _tr _ (Nothing, _count) = return (Nothing, 0)
    conteliding _tverb tr ev (_old, count) = do
        when (count > 0 && count `mod` 100 == 0) $ do  -- report every 100th elided messages
            meta <- mkLOMeta (getSeverityAnnotation ev) (getPrivacyAnnotation ev)
            traceNamedObject tr (meta, LogValue "messages elided" (PureI $ toInteger count))
        return (Just ev, count + 1)

tracingElidedMessages :: Assertion
tracingElidedMessages = do
    cfg <- defaultConfigStdout
    msgs <- STM.newTVarIO []
    baseTrace <- setupMockTrace $ TraceConfiguration cfg (MockSB msgs) "eliding" Neutral

    s_elide <- newstate

    let msg11 = Elided1 1400
        msg12 = Elided1 1000
        msg21 = Elided2 999
        msg22 = Elided2 998
        msg23 = Elided2 998
        msg31 = Item1 42
        msg32 = Item1 42
        infoTracer = annotateSeverity
                     $ elideToLogObject NormalVerbosity s_elide $ baseTrace
    traceWith infoTracer msg11
    traceWith infoTracer msg12
    traceWith infoTracer msg31
    traceWith infoTracer msg11
    traceWith infoTracer msg12  -- elided
    traceWith infoTracer msg12  -- elided
    traceWith infoTracer msg11
    traceWith infoTracer msg31
    traceWith infoTracer msg21
    traceWith infoTracer msg22
    traceWith infoTracer msg23
    traceWith infoTracer msg31
    traceWith infoTracer msg32
    traceWith infoTracer msg31
    traceWith infoTracer msg32
    traceWith infoTracer msg31

    ms <- STM.readTVarIO msgs

    assertBool
        ("assert number of messages traced == 15: " ++ show (reverse $ map loContent ms) ++ " len = " ++ show (length ms))
        (15 == length ms)

\end{code}

The first elided message is output and the internal counter of elided messages is set to zero. When the non-equivalent message is traced, the last elided message is not output since this is the same as the first one. 
\begin{code}
tracingElidedMessages1 :: Assertion
tracingElidedMessages1 = do
    cfg <- defaultConfigStdout
    msgs <- STM.newTVarIO []
    baseTrace <- setupMockTrace $ TraceConfiguration cfg (MockSB msgs) "eliding2" Neutral

    s_elide <- newstate

    let msg11 = Elided1 1400
        msg31 = Item1 42
        tracer = annotateSeverity
                 $ elideToLogObject NormalVerbosity s_elide $ baseTrace
    traceWith tracer msg11
    traceWith tracer msg31

    ms <- STM.readTVarIO msgs

    assertBool
        ("assert number of messages traced == 2: " ++ (show $ reverse $ map loContent ms))
        (2 == length ms)
\end{code}

The first message is output. When the non-equivalent message is traced, the last message is output. Since the first and last messages are output, no count of elided messages is reported.
\begin{code}
tracingElidedMessages2 :: Assertion
tracingElidedMessages2 = do
    cfg <- defaultConfigStdout
    msgs <- STM.newTVarIO []
    baseTrace <- setupMockTrace $ TraceConfiguration cfg (MockSB msgs) "eliding1" Neutral

    s_elide <- newstate

    let msg11 = Elided1 1400
        msg12 = Elided1 1000
        msg31 = Item1 42
        tracer = annotateSeverity
                 $ elideToLogObject NormalVerbosity s_elide $ baseTrace
    traceWith tracer msg11
    traceWith tracer msg12
    traceWith tracer msg31

    ms <- STM.readTVarIO msgs

    assertBool
        ("assert number of messages traced == 3: " ++ (show $ reverse $ map loContent ms))
        (3 == length ms)
\end{code}

The second tracing of |msg12| increases the internal counter of elided messages to two. One (2 - 1) elided message is reported, and the last message is output.
\begin{code}
tracingElidedMessages3 :: Assertion
tracingElidedMessages3 = do
    cfg <- defaultConfigStdout
    msgs <- STM.newTVarIO []
    baseTrace <- setupMockTrace $ TraceConfiguration cfg (MockSB msgs) "eliding3" Neutral

    s_elide <- newstate

    let msg11 = Elided1 1400
        msg12 = Elided1 1000
        msg31 = Item1 42
        tracer = annotateSeverity
                 $ elideToLogObject NormalVerbosity s_elide $ baseTrace
    traceWith tracer msg11
    traceWith tracer msg12
    traceWith tracer msg12  -- elided
    traceWith tracer msg31

    ms <- STM.readTVarIO msgs

    assertBool
        ("assert number of messages traced == 4: " ++ (show $ reverse $ map loContent ms))
        (4 == length ms)

\end{code}

An elided message is output every \emph{n} occurences.
\begin{code}
tracingElidedMessagesRepeat :: Assertion
tracingElidedMessagesRepeat = do
    cfg <- defaultConfigStdout
    msgs <- STM.newTVarIO []
    baseTrace <- setupMockTrace $ TraceConfiguration cfg (MockSB msgs) "eliding3" Neutral

    s_elide <- newstate

    let msg11 = Elided1 1400
        msg12 = Elided1 1000
        msg31 = Item1 42
        tracer = annotateSeverity
                 $ elideToLogObject NormalVerbosity s_elide $ baseTrace
    traceWith tracer msg11
    traceWith tracer msg12
    let mlist = map Elided1 [1..320]
    forM_ mlist $ \m -> traceWith tracer m
    traceWith tracer msg31

    ms <- STM.readTVarIO msgs

    assertBool
        ("assert number of messages traced == 7: " ++ (show $ reverse $ map loContent ms))
        (7 == length ms)

\end{code}
