
\subsection{Cardano.BM.Test.LogItem}
\label{code:Cardano.BM.Test.LogItem}

%if style == newcode
\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.BM.Test.LogItem (
    tests
  ) where

import           Data.Aeson (Value(..), encode, decode, eitherDecode)
import           Data.Text (Text)

import           Cardano.BM.Data.Aggregated
import           Cardano.BM.Data.BackendKind
import           Cardano.BM.Data.Counter
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Severity

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (Assertion , assertEqual, testCase)
import qualified Data.Aeson.KeyMap as KeyMap

\end{code}
%endif

\begin{code}
tests :: TestTree
tests = testGroup "Testing en/de-coding of LogItem" [
            testCase "en/de-code LogMessage" testLogMessage,
            testCase "en/de-code LogValue" testLogValue,
            testCase "en/de-code LogError" testLogError,
            testCase "en/de-code LogStructured" testLogStructured,
            testCase "en/de-code ObserveOpen" testObserveOpen,
            testCase "en/de-code ObserveDiff" testObserveDiff,
            testCase "en/de-code ObserveClose" testObserveClose,
            testCase "en/de-code AggregatedMessage" testAggregatedMessage,
            testCase "en/de-code MonitoringEffect" testMonitoringEffect,
            testCase "en/de-code Command" testCommand,
            testCase "en/de-code KillPill" testKillPill
        ]

\end{code}

\subsubsection{En/de-coding tests}
\begin{code}
testLogMessage :: Assertion
testLogMessage = do
    meta <- mkLOMeta Info Public
    let m :: LogObject Text = LogObject "test" meta (LogMessage "hello")
    let encoded = encode m
    let decoded = decode encoded :: Maybe (LogObject Text)
    assertEqual "unequal" (Just m) decoded

testLogValue :: Assertion
testLogValue = do
    meta <- mkLOMeta Info Public
    let m :: LogObject Text = LogObject "test" meta (LogValue "value" (PureI 42))
    let encoded = encode m
    let decoded = decode encoded :: Maybe (LogObject Text)
    assertEqual "unequal" (Just m) decoded

testLogError :: Assertion
testLogError = do
    meta <- mkLOMeta Info Public
    let m :: LogObject Text = LogObject "test" meta (LogError "error")
    let encoded = encode m
    let decoded = decode encoded :: Maybe (LogObject Text)
    assertEqual "unequal" (Just m) decoded

testLogStructured :: Assertion
testLogStructured = do
    meta <- mkLOMeta Info Public
    let m :: LogObject Text = LogObject "test" meta . LogStructured $
          KeyMap.singleton "foo" (String "bar")
    let encoded = encode m
    let decoded = eitherDecode encoded :: Either String (LogObject Text)
    assertEqual "unequal" (Right m) decoded

testObserveOpen :: Assertion
testObserveOpen = do
    meta <- mkLOMeta Info Public
    let cs = CounterState [Counter StatInfo "some" (Bytes 789),
                           Counter RTSStats "gcn" (PureI 42)]
    let m :: LogObject Text = LogObject "test" meta (ObserveOpen cs)
    let encoded = encode m
    let decoded = decode encoded :: Maybe (LogObject Text)
    assertEqual "unequal" (Just m) decoded

testObserveDiff :: Assertion
testObserveDiff = do
    meta <- mkLOMeta Info Public
    let cs = CounterState [Counter StatInfo "some" (Bytes 789),
                           Counter RTSStats "gcn" (PureI 42)]
    let m :: LogObject Text = LogObject "test" meta (ObserveDiff cs)
    let encoded = encode m
    let decoded = decode encoded :: Maybe (LogObject Text)
    assertEqual "unequal" (Just m) decoded

testObserveClose :: Assertion
testObserveClose = do
    meta <- mkLOMeta Info Public
    let cs = CounterState [Counter StatInfo "some" (Bytes 789),
                           Counter RTSStats "gcn" (PureI 42)]
    let m :: LogObject Text = LogObject "test" meta (ObserveClose cs)
    let encoded = encode m
    let decoded = decode encoded :: Maybe (LogObject Text)
    assertEqual "unequal" (Just m) decoded

testAggregatedMessage :: Assertion
testAggregatedMessage = do
    meta <- mkLOMeta Info Public
    let as = [("test1", AggregatedEWMA (EWMA 0.8 (PureD 47.32))),
              ("test2", AggregatedStats (Stats 1 4 (BaseStats 0 1 2 0.5 0.5) (BaseStats 1 1 2 1 0) (BaseStats (-1) 3 2 77 0)))]
    let m :: LogObject Text = LogObject "test" meta (AggregatedMessage as)
    let encoded = encode m
    let decoded = decode encoded :: Maybe (LogObject Text)
    assertEqual "unequal" (Just m) decoded

testMonitoringEffect :: Assertion
testMonitoringEffect = do
    meta <- mkLOMeta Info Public
    let m :: LogObject Text = LogObject "test" meta (MonitoringEffect (MonitorAlterGlobalSeverity Notice))
    let encoded = encode m
    let decoded = decode encoded :: Maybe (LogObject Text)
    assertEqual "unequal" (Just m) decoded

testCommand :: Assertion
testCommand = do
    meta <- mkLOMeta Info Public
    let m :: LogObject Text = LogObject "test" meta (Command (DumpBufferedTo KatipBK))
    let encoded = encode m
    let decoded = decode encoded :: Maybe (LogObject Text)
    assertEqual "unequal" (Just m) decoded

testKillPill :: Assertion
testKillPill = do
    meta <- mkLOMeta Info Public
    let m :: LogObject Text = LogObject "test" meta KillPill
    let encoded = encode m
    let decoded = decode encoded :: Maybe (LogObject Text)
    assertEqual "unequal" (Just m) decoded

\end{code}
