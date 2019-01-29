
\subsection{Testing aggregation}

%if style == newcode
\begin{code}
module Cardano.BM.Test.Aggregated (
    tests
    ,prop_Aggregation_comm
  ) where

import           System.IO.Unsafe (unsafePerformIO)

import           Control.Concurrent (threadDelay)

import           Cardano.BM.Arbitrary.Aggregated ()
import           Cardano.BM.Data.Aggregated
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Output.Aggregation (updateAggregation)

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

\end{code}
%endif

\begin{code}

tests :: TestTree
tests = testGroup "aggregation measurements" [
            property_tests
          , unit_tests
        ]

property_tests :: TestTree
property_tests = testGroup "Properties" [
        testProperty "minimal" prop_Aggregation_minimal
      , testProperty "commutative" prop_Aggregation_comm
    ]

unit_tests :: TestTree
unit_tests = testGroup "Unit tests" [
        testCase "initial_minus_1" unit_Aggregation_initial_minus_1
      , testCase "initial_plus_1" unit_Aggregation_initial_plus_1
      , testCase "initial_0" unit_Aggregation_initial_zero
      , testCase "initial_plus_1" unit_Aggregation_initial_plus_1_minus_1
      , testCase "stepwise" unit_Aggregation_stepwise
    ]

prop_Aggregation_minimal :: Bool
prop_Aggregation_minimal = True

lometa :: LOMeta
lometa = unsafePerformIO $ mkLOMeta

prop_Aggregation_comm :: Integer -> Integer -> Aggregated -> Bool
prop_Aggregation_comm v1 v2 ag =
    let AggregatedStats stats1 = updateAggregation (PureI v1) (updateAggregation (PureI v2) ag lometa Nothing) lometa Nothing
        AggregatedStats stats2 = updateAggregation (PureI v2) (updateAggregation (PureI v1) ag lometa Nothing) lometa Nothing
    in
    fbasic stats1 == fbasic stats2 &&
    (v1 == v2) `implies` (flast stats1 == flast stats2)

-- implication: if p1 is true, then return p2; otherwise true
implies :: Bool -> Bool -> Bool
implies p1 p2 = (not p1) || p2

unit_Aggregation_initial_minus_1 :: Assertion
unit_Aggregation_initial_minus_1 = do
    let AggregatedStats stats1 = updateAggregation (-1) firstStateAggregatedStats lometa Nothing
    flast stats1 @?= (-1)
    (fbasic stats1) @?= BaseStats (-1) 0 2 (-0.5) 0.5
    (fdelta stats1) @?= BaseStats 0 0 1 0 0
        -- AggregatedStats (Stats (-1) 0 (BaseStats (-1) 0 2 (-0.5) 0.5) (BaseStats 0 0 1 0 0) (BaseStats 0 0 1 0 0))
unit_Aggregation_initial_plus_1 :: Assertion
unit_Aggregation_initial_plus_1 = do
    let AggregatedStats stats1 = updateAggregation 1 firstStateAggregatedStats lometa Nothing
    flast stats1 @?= 1
    (fbasic stats1) @?= BaseStats 0 1 2 0.5 0.5
    (fdelta stats1) @?= BaseStats 0 0 1 0 0
        -- AggregatedStats (Stats 1 0 (BaseStats 0 1 2 0.5 0.5) (BaseStats 0 0 1 0 0) (BaseStats 0 0 1 0 0))
unit_Aggregation_initial_zero :: Assertion
unit_Aggregation_initial_zero = do
    let AggregatedStats stats1 = updateAggregation 0 firstStateAggregatedStats lometa Nothing
    flast stats1 @?= 0
    (fbasic stats1) @?= BaseStats 0 0 2 0 0
    (fdelta stats1) @?= BaseStats 0 0 1 0 0
        -- AggregatedStats (Stats 0 0 (BaseStats 0 0 2 0 0) (BaseStats 0 0 1 0 0) (BaseStats 0 0 1 0 0))
unit_Aggregation_initial_plus_1_minus_1 :: Assertion
unit_Aggregation_initial_plus_1_minus_1 = do
    let AggregatedStats stats1 = updateAggregation (-1) (updateAggregation 1 firstStateAggregatedStats lometa Nothing) lometa Nothing
    (fbasic stats1) @?= BaseStats (-1) 1 3 0.0 2.0
    (fdelta stats1) @?= BaseStats (-2) 0 2 (-1.0) 2.0

unit_Aggregation_stepwise :: Assertion
unit_Aggregation_stepwise = do
    stats0 <- pure $ singletonStats (Bytes 3000)
    putStrLn $ show stats0
    threadDelay 50000   -- 0.05 s
    t1 <- mkLOMeta
    stats1 <- pure $ updateAggregation (Bytes 5000) stats0 t1 Nothing
    putStrLn $ show stats1
    showTimedMean stats1
    threadDelay 50000   -- 0.05 s
    t2 <- mkLOMeta
    stats2 <- pure $ updateAggregation (Bytes 1000) stats1 t2 Nothing
    putStrLn $ show stats2
    showTimedMean stats2
    checkTimedMean stats2
    threadDelay 50000   -- 0.05 s
    t3 <- mkLOMeta
    stats3 <- pure $ updateAggregation (Bytes 3000) stats2 t3 Nothing
    putStrLn $ show stats3
    showTimedMean stats3
    checkTimedMean stats3
    threadDelay 50000   -- 0.05 s
    t4 <- mkLOMeta
    stats4 <- pure $ updateAggregation (Bytes 1000) stats3 t4 Nothing
    putStrLn $ show stats4
    showTimedMean stats4
    checkTimedMean stats4
  where
    checkTimedMean (AggregatedEWMA _) = return ()
    checkTimedMean (AggregatedStats s) = do
        let mean = meanOfStats (ftimed s)
        assertBool "the mean should be >= the minimum" (mean >= getDouble (fmin (ftimed s)))
        assertBool "the mean should be =< the maximum" (mean <= getDouble (fmax (ftimed s)))
    showTimedMean (AggregatedEWMA _) = return ()
    showTimedMean (AggregatedStats s) = putStrLn $ "mean = " ++ show (meanOfStats (ftimed s)) ++ showUnits (fmin (ftimed s))

firstStateAggregatedStats :: Aggregated
firstStateAggregatedStats = AggregatedStats (Stats 0 0 (BaseStats 0 0 1 0 0) (BaseStats 0 0 0 0 0) (BaseStats 0 0 0 0 0))

\end{code}
