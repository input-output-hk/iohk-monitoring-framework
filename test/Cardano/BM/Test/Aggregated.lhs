
\subsection{Testing aggregation}

%if style == newcode
\begin{code}
module Cardano.BM.Test.Aggregated (
    tests
  ) where

import           Cardano.BM.Aggregated
import           Cardano.BM.Arbitrary.Aggregated ()

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
  let Just (Aggregated stats1 last1 delta1) = updateAggregation v1 $ updateAggregation v2 (Just ag)
      Just (Aggregated stats2 last2 delta2) = updateAggregation v2 $ updateAggregation v1 (Just ag)
  in
  stats1 == stats2 && ((v1 == v2) `implies` (last1 == last2))
                   && ((v1 == v2) `implies` (delta1 == delta2))

-- implication: if p1 is true, then return p2; otherwise true
implies :: Bool -> Bool -> Bool
implies p1 p2 = (not p1) || p2

unit_Aggregation_initial_minus_1 :: Assertion
unit_Aggregation_initial_minus_1 =
  updateAggregation (-1) Nothing @?= Just (Aggregated {
                                            fstats=Stats (-1) (-1) 1 (-1) 1
                                          , flast=(-1)
                                          , fdelta=Stats 0 0 0 0 0 } )
unit_Aggregation_initial_plus_1 :: Assertion
unit_Aggregation_initial_plus_1 =
  updateAggregation 1 Nothing  @?= Just (Aggregated
                                             (Stats 1 1 1 1 1)
                                             1
                                             (Stats 0 0 0 0 0) )
unit_Aggregation_initial_zero :: Assertion
unit_Aggregation_initial_zero =
  updateAggregation 0 Nothing @?= Just (Aggregated
                                             (Stats 0 0 1 0 0)
                                             0
                                             (Stats 0 0 0 0 0) )
\end{code}
