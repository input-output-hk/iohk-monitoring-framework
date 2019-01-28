
\subsection{Testing aggregation}

%if style == newcode
\begin{code}
module Cardano.BM.Test.Aggregated (
    tests
    ,prop_Aggregation_comm
  ) where

import           System.IO.Unsafe (unsafePerformIO)

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

firstStateAggregatedStats :: Aggregated
firstStateAggregatedStats = AggregatedStats (Stats 0 0 (BaseStats 0 0 1 0 0) (BaseStats 0 0 0 0 0) (BaseStats 0 0 0 0 0))

\end{code}
