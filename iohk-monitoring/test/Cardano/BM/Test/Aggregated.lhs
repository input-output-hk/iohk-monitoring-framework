
\subsection{Testing aggregation}
\label{code:Cardano.BM.Test.Aggregated}

%if style == newcode
\begin{code}

-- Acceptable only because this is test code and because it is in code that will
-- be deprecated in the future.
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.BM.Test.Aggregated (
    tests
    ,prop_Aggregation_comm
  ) where

import           System.IO.Unsafe (unsafePerformIO)

import           Control.Concurrent (threadDelay)

import           Cardano.BM.Arbitrary.Aggregated ()
import           Cardano.BM.Data.Aggregated
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Severity

import           Test.QuickCheck (Property, (===), (.&&.), (.||.), property)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty)

\end{code}
%endif

\begin{code}

tests :: TestTree
tests = testGroup "Aggregation measurements" [
            propertyTests
          , unitTests1
          , unitTests2
        ]

propertyTests :: TestTree
propertyTests = testGroup "Properties" [
        testProperty "minimal" prop_Aggregation_minimal
      , testProperty "commutative" prop_Aggregation_comm
    ]

unitTests1 :: TestTree
unitTests1 = testGroup "Unit tests for Aggregated" [
        testCase "compare equal >" unitAggregatedEqualGT
      , testCase "compare equal <" unitAggregatedEqualLT
      , testCase "compare different >" unitAggregatedDiffGT
      , testCase "compare different <" unitAggregatedDiffLT
    ]

unitTests2 :: TestTree
unitTests2 = testGroup "Unit tests for Aggregation" [
        testCase "initial -1" unitAggregationInitialMinus1
      , testCase "initial +1" unitAggregationInitialPlus1
      , testCase "initial +0" unitAggregationInitialZero
      , testCase "initial +1, -1" unitAggregationInitialPlus1Minus1
      , testCase "stepwise" unitAggregationStepwise
    ]

\end{code}

\subsubsection{Property tests}

\begin{code}

prop_Aggregation_minimal :: Bool
prop_Aggregation_minimal = True

lometa :: LOMeta
lometa = unsafePerformIO $ mkLOMeta Debug Public

prop_Aggregation_comm :: Integer -> Integer -> Aggregated -> Property
prop_Aggregation_comm v1 v2 ag =
    let ns = utc2ns $ tstamp lometa
        Right agg2 = updateAggregation (PureI v2) ag ns
        Right agg1 = updateAggregation (PureI v1) ag ns
        Right (AggregatedStats stats21) = updateAggregation (PureI v1) agg2 ns
        Right (AggregatedStats stats12) = updateAggregation (PureI v2) agg1 ns
    in
    fbasic stats21 === fbasic stats12 .&&.
    (v1 == v2) `implies` (flast stats21 === flast stats12)

-- implication: if p1 is true, then return p2; otherwise true
implies :: Bool -> Property -> Property
implies p1 p2 = property (not p1) .||. p2

\end{code}

\subsubsection{Unit tests for Aggregation}
\begin{code}

unitAggregationInitialMinus1 :: Assertion
unitAggregationInitialMinus1 = do
    let ns = utc2ns $ tstamp lometa
        Right (AggregatedStats stats1) = updateAggregation (-1) firstStateAggregatedStats ns
    flast stats1 @?= (-1)
    (fbasic stats1) @?= BaseStats (-1) 0 2 (-0.5) 0.5
    (fdelta stats1) @?= BaseStats (-1) (-1) 2 (-1) 0
        -- AggregatedStats (Stats (-1) x (BaseStats (-1) 0 2 (-0.5) 0.5) (BaseStats (-1) (-1) 2 (-1) 0) (BaseStats x x 2 x 0))
unitAggregationInitialPlus1 :: Assertion
unitAggregationInitialPlus1 = do
    let ns = utc2ns $ tstamp lometa
        Right (AggregatedStats stats1) = updateAggregation 1 firstStateAggregatedStats ns
    flast stats1 @?= 1
    (fbasic stats1) @?= BaseStats 0 1 2 0.5 0.5
    (fdelta stats1) @?= BaseStats 1 1 2 1 0
        -- AggregatedStats (Stats 1 x (BaseStats 0 1 2 0.5 0.5) (BaseStats 1 1 2 1 0) (BaseStats x x 2 x 0))
unitAggregationInitialZero :: Assertion
unitAggregationInitialZero = do
    let ns = utc2ns $ tstamp lometa
        Right (AggregatedStats stats1) = updateAggregation 0 firstStateAggregatedStats ns
    flast stats1 @?= 0
    (fbasic stats1) @?= BaseStats 0 0 2 0 0
    (fdelta stats1) @?= BaseStats 0 0 2 0 0
        -- AggregatedStats (Stats 0 x (BaseStats 0 0 2 0 0) (BaseStats 0 0 2 0 0) (BaseStats x x 2 x 0))
unitAggregationInitialPlus1Minus1 :: Assertion
unitAggregationInitialPlus1Minus1 = do
    let ns = utc2ns $ tstamp lometa
        Right agg1 = updateAggregation (PureI 1) firstStateAggregatedStats ns
        Right (AggregatedStats stats1) = updateAggregation (PureI (-1)) agg1 ns
    (fbasic stats1) @?= BaseStats (PureI (-1)) (PureI 1) 3   0.0  2.0
    (fdelta stats1) @?= BaseStats (PureI (-2)) (PureI 1) 3 (-0.5) 4.5

unitAggregationStepwise :: Assertion
unitAggregationStepwise = do
    stats0 <- pure $ singletonStats (Bytes 3000)
    -- putStrLn (show stats0)
    threadDelay 50000   -- 0.05 s
    t1 <- mkLOMeta Debug Public
    Right stats1 <- pure $ updateAggregation (Bytes 5000) stats0 (utc2ns $ tstamp t1)
    -- putStrLn (show stats1)
    -- showTimedMean stats1
    threadDelay 50000   -- 0.05 s
    t2 <- mkLOMeta Debug Public
    Right stats2 <- pure $ updateAggregation (Bytes 1000) stats1 (utc2ns $ tstamp t2)
    -- putStrLn (show stats2)
    -- showTimedMean stats2
    checkTimedMean stats2
    threadDelay 50000   -- 0.05 s
    t3 <- mkLOMeta Debug Public
    Right stats3 <- pure $ updateAggregation (Bytes 3000) stats2 (utc2ns $ tstamp t3)
    -- putStrLn (show stats3)
    -- showTimedMean stats3
    checkTimedMean stats3
    threadDelay 50000   -- 0.05 s
    t4 <- mkLOMeta Debug Public
    Right stats4 <- pure $ updateAggregation (Bytes 1000) stats3 (utc2ns $ tstamp t4)
    -- putStrLn (show stats4)
    -- showTimedMean stats4
    checkTimedMean stats4
  where
    checkTimedMean (AggregatedEWMA _) = return ()
    checkTimedMean (AggregatedStats s) = do
        let mean = meanOfStats (ftimed s)
        assertBool "the mean should be >= the minimum" (mean >= getDouble (fmin (ftimed s)))
        assertBool "the mean should be =< the maximum" (mean <= getDouble (fmax (ftimed s)))

\end{code}

commented out:
\begin{spec}
    showTimedMean (AggregatedEWMA _) = return ()
    showTimedMean (AggregatedStats s) = putStrLn $ "mean = " ++ show (meanOfStats (ftimed s))
                                                             ++ showUnits (fmin (ftimed s))
\end{spec}

\begin{code}
firstStateAggregatedStats :: Aggregated
firstStateAggregatedStats = AggregatedStats $
                                Stats
                                    z
                                    z'
                                    (BaseStats z  z  1 0 0)
                                    (BaseStats z  z  1 0 0)
                                    (BaseStats z' z' 1 0 0)
  where
    z  = PureI 0
    z' = Nanoseconds 0

\end{code}

\subsubsection{Unit tests for Aggregated}
\begin{code}

unitAggregatedEqualGT :: Assertion
unitAggregatedEqualGT = do
    assertBool "comparing seconds"
        ((Seconds 3) > (Seconds 2))
    assertBool "comparing microseconds"
        ((Microseconds 3000) > (Microseconds 2000))
    assertBool "comparing nanoseconds"
        ((Nanoseconds 3000000) > (Nanoseconds 2000000))
    assertBool "comparing bytes"
        ((Bytes 2048) > (Bytes 1024))
    assertBool "comparing doubles"
        ((PureD 2.34) > (PureD 1.42))
    assertBool "comparing integers"
        ((PureI 2) > (PureI 1))
    assertBool "comparing severities"
        ((Severity Error) > (Severity Warning))

unitAggregatedEqualLT :: Assertion
unitAggregatedEqualLT = do
    assertBool "comparing seconds"
        ((Seconds 2) < (Seconds 3))
    assertBool "comparing microseconds"
        ((Microseconds 2000) < (Microseconds 3000))
    assertBool "comparing nanoseconds"
        ((Nanoseconds 2000000) < (Nanoseconds 3000000))
    assertBool "comparing bytes"
        ((Bytes 1024) < (Bytes 2048))
    assertBool "comparing doubles"
        ((PureD 1.34) < (PureD 2.42))
    assertBool "comparing integers"
        ((PureI 1) < (PureI 2))
    assertBool "comparing severities"
        ((Severity Info) < (Severity Notice))

unitAggregatedDiffGT :: Assertion
unitAggregatedDiffGT = do
    assertBool "comparing time (µs vs. s)"
        ((Microseconds 3000000) > (Seconds 2))
    assertBool "comparing time (µs vs. ns)"
        ((Microseconds 30) > (Nanoseconds 29999))
    assertBool "comparing nanoseconds"
        ((Nanoseconds 3000000) > (Microseconds 2900))
    assertBool "comparing bytes"
        ((Bytes 2048) > (PureI 1024))
    assertBool "comparing doubles"
        ((PureD 2.34) > (PureI 1))
    assertBool "comparing integers"
        ((PureI 2) > (PureD 1.42))

unitAggregatedDiffLT :: Assertion
unitAggregatedDiffLT = do
    assertBool "comparing time (µs vs. s)"
        ((Microseconds 2999999) < (Seconds 3))
    assertBool "comparing time (µs vs. ns)"
        ((Microseconds 30) < (Nanoseconds 30001))
    assertBool "comparing nanoseconds"
        ((Nanoseconds 3000000) < (Microseconds 3001))
    assertBool "comparing bytes"
        ((PureI 1024) < (Bytes 2048))
    assertBool "comparing doubles"
        ((PureD 2.34) < (PureI 3))
    assertBool "comparing integers"
        ((PureI 2) < (PureD 3.42))

\end{code}
