
\subsection{Cardano.BM.Test.Tracer}
\label{code:Cardano.BM.Test.Tracer}

%if style == newcode
\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Cardano.BM.Test.Tracer (
    tests
  ) where

import           Control.Monad (void)
import           Data.Functor.Contravariant (Contravariant (..))
import           Data.Text (Text, unpack)

import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.Tracer

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
            testCase "tracing with filtering for both severity and privacy" tracingWithComplexFiltering
        ]

\end{code}

\subsubsection{Utilities}
\begin{spec}
data LogNamed item = LogNamed
    { lnName :: LoggerName
    , lnItem :: item
    } deriving (Show)

named :: Tracer m (LogNamed a) -> Tracer m a
named = contramap (LogNamed mempty)

appendNamed :: LoggerName -> Tracer m (LogNamed a) -> Tracer m (LogNamed a)
appendNamed name = contramap $ (\(LogNamed oldName item) ->
    LogNamed (name <> "." <> oldName) item)

renderNamedItemTracing :: Show a => Tracer m String -> Tracer m (LogNamed a)
renderNamedItemTracing = contramap $ \item ->
    unpack (lnName item) ++ ": " ++ show (lnItem item)

\end{spec}

\begin{code}
renderNamedItemTracing' :: Show a => Tracer m String -> Tracer m (LogObject a)
renderNamedItemTracing' = contramap $ \item ->
    unpack (loName item) ++ ": " ++ show (loContent item) ++ ", (meta): " ++ show (loMeta item)

\end{code}

\subsubsection{Tracing messages in a named context}
\begin{code}
tracingInNamedContext :: Assertion
tracingInNamedContext = do
    let logTrace = addName "named" $ renderNamedItemTracing' $ stdoutTracer

    void $ callFun2 logTrace

    assertBool "OK" True

callFun2 :: Tracer IO (LogObject Text) -> IO Int
callFun2 logTrace = do
    let logTrace' = addName "fun2" logTrace
    traceWith (toLogObject logTrace') ("in function 2" :: Text)
    callFun3 logTrace'

callFun3 :: Tracer IO (LogObject Text) -> IO Int
callFun3 logTrace = do
    traceWith (toLogObject $ addName "fun3" $ logTrace) ("in function 3" :: Text)
    return 42

\end{code}

\subsubsection{Tracing messages with pricacy and severity annotation}
A |Tracer| transformer creating a |LogObject| from |PrivacyAndSeverityAnnotated|.
\begin{code}
logObjectFromAnnotated :: Show a
    => Tracer IO (LogObject a)
    -> Tracer IO (PrivacyAndSeverityAnnotated a)
logObjectFromAnnotated tr = Tracer $ \(PSA sev priv a) -> do
    lometa <- mkLOMeta sev priv
    traceWith tr $ LogObject "" lometa (LogMessage a)

\end{code}

\begin{code}
tracingWithPrivacyAndSeverityAnnotation :: Assertion
tracingWithPrivacyAndSeverityAnnotation = do
    let logTrace =
            logObjectFromAnnotated $ addName "example3" $ renderNamedItemTracing' stdoutTracer

    traceWith logTrace $ PSA Info Confidential ("Hello" :: String)
    traceWith logTrace $ PSA Warning Public "World"

    assertBool "OK" True
\end{code}

\subsubsection{Filter Tracer}
\begin{code}
filterAppendNameTracing :: Monad m
    => m (LogObject a -> Bool)
    -> LoggerName
    -> Tracer m (LogObject a)
    -> Tracer m (LogObject a)
filterAppendNameTracing test name = (addName name) . (condTracingM test)

tracingWithPredicateFilter :: Assertion
tracingWithPredicateFilter = do
    let appendF = filterAppendNameTracing oracle
        logTrace :: Tracer IO (LogObject Text) = appendF "example4" (renderNamedItemTracing' stdoutTracer)

    traceWith (toLogObject logTrace) ("Hello" :: String)

    let logTrace' = appendF "inner" logTrace
    traceWith (toLogObject logTrace') ("World" :: String)

    let logTrace'' = appendF "innest" logTrace'
    traceWith (toLogObject logTrace'') ("!!" :: String)

    assertBool "OK" True
  where
    oracle :: Monad m => m (LogObject a -> Bool)
    oracle = return $ ((/=) "example4.inner.") . loName

\end{code}

\begin{code}
-- severity anotated
tracingWithMonadicFilter :: Assertion
tracingWithMonadicFilter = do
    let logTrace =
            condTracingM oracle $
                logObjectFromAnnotated $
                    addName "test5" $ renderNamedItemTracing' stdoutTracer

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
            addName "test6" $ renderNamedItemTracing' stdoutTracer
        logTrace1 =  -- the trace from |Privacy...Annotated| to |LogObject|
            condTracingM oracleSev $ logObjectFromAnnotated $ logTrace0
        logTrace2 =
            addName "row" $ condTracingM oracleName $ logTrace0
        logTrace3 =  -- oracle should eliminate messages from this trace
            addName "raw" $ condTracingM oracleName $ logTrace0

    traceWith logTrace1 $ PSA Debug Confidential ("Hello" :: String)
    traceWith logTrace1 $ PSA Warning Public "World"
    lometa <- mkLOMeta Info Public
    traceWith logTrace2 $ LogObject "" lometa (LogMessage ", RoW!")
    traceWith logTrace3 $ LogObject "" lometa (LogMessage ", RoW!")

    assertBool "OK" True
  where
    oracleSev :: Monad m => m (PrivacyAndSeverityAnnotated a -> Bool)
    oracleSev = return $ \(PSA sev _priv _) -> (sev > Debug)
    oracleName :: Monad m => m (LogObject a -> Bool)
    oracleName = return $ \(LogObject name _ _) -> (name == "row")  -- we only see the names from us to the leaves

\end{code}
