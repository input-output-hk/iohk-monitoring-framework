
\subsection{Testing aggregation}

%if style == newcode
\begin{code}
module Cardano.BM.Test.Aggregated (
    tests
    ,prop_Aggregation_comm
  ) where

import           Cardano.BM.Arbitrary.Aggregated ()
import           Cardano.BM.Data.Aggregated
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
    ]

prop_Aggregation_minimal :: Bool
prop_Aggregation_minimal = True

prop_Aggregation_comm :: Integer -> Integer -> Aggregated -> Bool
prop_Aggregation_comm v1 v2 ag =
    let AggregatedStats stats1 = updateAggregation (PureI v1) $ updateAggregation (PureI v2) ag
        AggregatedStats stats2 = updateAggregation (PureI v2) $ updateAggregation (PureI v1) ag
    in
    fmin   stats1 == fmin   stats2 &&
    fmax   stats1 == fmax   stats2 &&
    fcount stats1 == fcount stats2 &&
    abs (fsum_A stats1 - fsum_A stats2) < 1.0e-4 &&
    abs (fsum_B stats1 - fsum_B stats2) < 1.0e-4 &&
    (v1 == v2) `implies` (flast stats1 == flast stats2)

-- implication: if p1 is true, then return p2; otherwise true
implies :: Bool -> Bool -> Bool
implies p1 p2 = (not p1) || p2

unit_Aggregation_initial_minus_1 :: Assertion
unit_Aggregation_initial_minus_1 =
    updateAggregation (-1) firstStateAggregatedStats @?=
        AggregatedStats (Stats (-1) (-1) 0 2 (-0.5) 0.5)
unit_Aggregation_initial_plus_1 :: Assertion
unit_Aggregation_initial_plus_1 =
    updateAggregation 1 firstStateAggregatedStats @?= AggregatedStats (Stats 1 0 1 2 0.5 0.5)
unit_Aggregation_initial_zero :: Assertion
unit_Aggregation_initial_zero =
    updateAggregation 0 firstStateAggregatedStats @?= AggregatedStats (Stats 0 0 0 2 0 0)

firstStateAggregatedStats :: Aggregated
firstStateAggregatedStats = AggregatedStats (Stats 0 0 0 1 0 0)

\end{code}
