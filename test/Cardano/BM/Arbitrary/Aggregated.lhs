
\subsection{\tt{instance} Arbitrary Aggregated}

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
            (m, s) = updateMeanVar $ map fromInteger vs
        return $ AggregatedStats (Stats
                                (PureI (last vs))
                                (PureI (minimum vs))
                                (PureI (maximum vs))
                                (toInteger $ length vs)
                                (m)
                                (s)
                            )

\end{code}

Estimators for mean and variance must be updated the same way as in the code.
\begin{code}
updateMeanVar :: [Double] -> (Double, Double)
updateMeanVar [] = (0,0)
updateMeanVar (a : r) = updateMeanVar' (a,0) 1 r
updateMeanVar' (m,s) _ [] = (m,s)
updateMeanVar' (m,s) cnt (a : r) =
    let delta = a - m
        newcount = cnt + 1
        m' = m + (delta / newcount)
        s' = s + (delta * (a - m'))
    in
    updateMeanVar' (m',s') newcount r
        
\end{code}