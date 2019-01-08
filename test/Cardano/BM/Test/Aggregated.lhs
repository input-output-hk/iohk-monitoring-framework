
\subsection{Testing aggregation}

%if style == newcode
\begin{code}
module Cardano.BM.Test.Aggregated (
    tests
    ,prop_Aggregation_comm
  ) where

import           Cardano.BM.Arbitrary.Aggregated ()
import           Cardano.BM.Data.Aggregated

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
    let Just (AggregatedStats stats1) = updateAggregation (PureI v1) $ updateAggregation (PureI v2) (Just ag)
        Just (AggregatedStats stats2) = updateAggregation (PureI v2) $ updateAggregation (PureI v1) (Just ag)
    in
    fmin   stats1 == fmin   stats2 &&
    fmax   stats1 == fmax   stats2 &&
    fcount stats1 == fcount stats2 &&
    abs (fsum_A stats1 - fsum_A stats2) < 1.0e-4 &&
    abs (fsum_B stats1 - fsum_B stats2) < 1.0e-4

unit_Aggregation_initial_minus_1 :: Assertion
unit_Aggregation_initial_minus_1 =
    updateAggregation (-1) Nothing @?= Just (AggregatedStats (Stats (-1) (-1) (-1) (1) (-1) 0))
unit_Aggregation_initial_plus_1 :: Assertion
unit_Aggregation_initial_plus_1 =
    updateAggregation 1 Nothing  @?= Just (AggregatedStats (Stats 1 1 1 1 1 0))
unit_Aggregation_initial_zero :: Assertion
unit_Aggregation_initial_zero =
    updateAggregation 0 Nothing @?= Just (AggregatedStats (Stats 0 0 0 1 0 0))

\end{code}
