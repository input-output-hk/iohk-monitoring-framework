
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
        let delta as = map (uncurry (-)) $ zip as (tail as)
            sum2 = foldr (\e a -> a + e * e) 0
            vs = 42 : 17 : vs'
        return $ Aggregated (Stats
                                (Pure (minimum vs))
                                (Pure (maximum vs))
                                (toInteger $ length vs)
                                (Pure (sum vs))
                                (Pure (sum2 vs))
                            )
                            (Pure (last vs))
                            (Stats
                                (Pure (minimum $ delta vs))
                                (Pure (maximum $ delta vs))
                                (toInteger $ length vs)
                                (Pure (sum $ delta vs))
                                (Pure (sum2 $ delta vs))
                            )

\end{code}
