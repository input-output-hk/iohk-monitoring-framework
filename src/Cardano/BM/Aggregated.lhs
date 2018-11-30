\subsection{Cardano.BM.Aggregated}

\begin{code}
module Cardano.BM.Aggregated
  (
    Aggregated (..)
  , Stats (..)
  , updateAggregation
  ) where
\end{code}

\begin{code}

data Stats = Stats {
    fmin   :: Integer,
    fmax   :: Integer,
    fcount :: Integer,
    fsum_A :: Integer,
    fsum_B :: Integer
    } deriving (Show, Eq)

data Aggregated = Aggregated {
    fstats :: Stats,
    flast  :: Integer,
    fdelta :: Stats
    } deriving (Show, Eq)
\end{code}

\subsubsection{Update aggregation}

We distinguish an unitialized from an already initialized aggregation:

\begin{code}
updateAggregation :: Integer -> Maybe Aggregated -> Maybe Aggregated
updateAggregation v Nothing =
    Just $
        Aggregated { fstats = Stats {
                       fmin=v   , fmax=v     , fcount=1
                     , fsum_A=v , fsum_B=v*v}
                   , flast = v
                   , fdelta = Stats {
                       fmin=0  , fmax=0   , fcount=0
                     , fsum_A=0, fsum_B=0 }
                   }
updateAggregation v (Just (Aggregated (Stats _min _max _count _sumA _sumB)
                                      _last
                                      (Stats _dmin _dmax _dcount _dsumA _dsumB)
                          )) =
    let delta = v - _last
    in
    Just $
        Aggregated { fstats = Stats {
                       fmin=(min _min v)
                     , fmax=(max _max v)
                     , fcount=(_count + 1)
                     , fsum_A=(_sumA + v)
                     , fsum_B=(_sumB + v * v)
                     }
                   , flast = v
                   , fdelta = Stats {
                       fmin=(min _dmin delta)
                     , fmax=(max _dmax delta)
                     , fcount=(_dcount + 1)
                     , fsum_A=(_dsumA + delta)
                     , fsum_B=(_dsumB + delta * delta)
                     }
                   }
\end{code}

