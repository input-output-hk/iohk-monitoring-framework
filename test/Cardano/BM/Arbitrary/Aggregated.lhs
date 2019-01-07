
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
        let sum2 = foldr (\e a -> a + e * e) 0
            vs = 42 : 17 : vs'
        return $ AggregatedStats (Stats
                                (PureI (last vs))
                                (PureI (minimum vs))
                                (PureI (maximum vs))
                                (toInteger $ length vs)
                                (PureI (sum vs))
                                (PureI (sum2 vs))
                            )

\end{code}
