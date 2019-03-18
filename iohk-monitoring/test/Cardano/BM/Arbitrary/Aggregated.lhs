
\subsection{\tt{instance} Arbitrary Aggregated}
\label{code:Cardano.BM.Arbitrary.Aggregated}

%if style == newcode
\begin{code}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.BM.Arbitrary.Aggregated

where

import           Test.QuickCheck

import           Cardano.BM.Data.Aggregated
\end{code}
%endif

We define an instance of |Arbitrary| for an |Aggregated| which lets |QuickCheck|
generate arbitrary instances of |Aggregated|.
For this an arbitrary list of |Integer| is generated and this list is aggregated into a structure of |Aggregated|.

\begin{code}
instance Arbitrary Aggregated where
    arbitrary = do
        vs' <- arbitrary :: Gen [Integer]
        let vs = 42 : 17 : vs'
            ds = map (\(a,b) -> a - b) $ zip vs (tail vs)
            (m1, s1) = updateMeanVar $ map fromInteger vs
            (m2, s2) = updateMeanVar $ map fromInteger ds
            mkBasicStats = BaseStats
                                (PureI (minimum vs))
                                (PureI (maximum vs))
                                (fromIntegral $ length vs)
                                (m1)
                                (s1)
            mkDeltaStats = BaseStats
                                (PureI (minimum ds))
                                (PureI (maximum ds))
                                (fromIntegral $ length ds)
                                (m2)
                                (s2)
            mkTimedStats = BaseStats
                                (Nanoseconds 0)
                                (Nanoseconds 0)
                                (0)
                                (0)
                                (0)
        return $ AggregatedStats (Stats
                                     (PureI (last vs))
                                     (Nanoseconds 0)
                                     mkBasicStats
                                     mkDeltaStats
                                     mkTimedStats)

\end{code}

Estimators for mean and variance must be updated the same way as in the code.
\begin{code}
updateMeanVar :: [Double] -> (Double, Double)
updateMeanVar [] = (0,0)
updateMeanVar (val : vals) = updateMeanVar' (val,0) 1 vals
    where
        updateMeanVar' (m,s) _ [] = (m,s)
        updateMeanVar' (m,s) cnt (a : r) =
            let delta = a - m
                newcount = cnt + 1
                m' = m + (delta / newcount)
                s' = s + (delta * (a - m'))
            in
            updateMeanVar' (m',s') newcount r
        
\end{code}