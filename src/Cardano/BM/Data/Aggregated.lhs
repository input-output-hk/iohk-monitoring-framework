
\subsection{Cardano.BM.Data.Aggregated}

%if style == newcode
\begin{code}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
module Cardano.BM.Data.Aggregated
  (
    Aggregated (..)
  , Stats (..)
  , Measurable (..)
  , updateAggregation
  ) where

import           GHC.Generics (Generic)
import           Data.Aeson (ToJSON)
\end{code}
%endif

\subsubsection{Stats}\label{code:Stats}
\begin{code}

data Measurable = Microseconds Integer
                | Bytes Integer
                | Pure Integer
                deriving (Eq, Ord, Generic, ToJSON)

instance Num Measurable where
    (+) (Microseconds a) (Microseconds b) = Microseconds (a+b)
    (+) (Bytes a)        (Bytes b)        = Bytes        (a+b)
    (+) (Pure a)         (Pure b)         = Pure         (a+b)
    (+)  _                _               = error "Trying to add values with different units"

    (*) (Microseconds a) (Microseconds b) = Microseconds (a*b)
    (*) (Bytes a)        (Bytes b)        = Bytes        (a*b)
    (*) (Pure a)         (Pure b)         = Pure         (a*b)
    (*)  _                _               = error "Trying to multiply values with different units"

    abs (Microseconds a) = Microseconds (abs a)
    abs (Bytes a)        = Bytes        (abs a)
    abs (Pure a)         = Pure         (abs a)

    signum (Microseconds a) = Microseconds (signum a)
    signum (Bytes a)        = Bytes        (signum a)
    signum (Pure a)         = Pure         (signum a)

    negate (Microseconds a) = Microseconds (negate a)
    negate (Bytes a)        = Bytes        (negate a)
    negate (Pure a)         = Pure         (negate a)

    fromInteger = Pure

instance Show Measurable where
    show v@(Microseconds a) = show a ++ showUnits v
    show v@(Bytes a)        = show a ++ showUnits v
    show v@(Pure a)         = show a ++ showUnits v

showUnits :: Measurable -> String
showUnits (Microseconds _) = " µs"
showUnits (Bytes _)        = " B"
showUnits (Pure _)         = ""

mean :: Measurable -> Integer -> Float
mean (Microseconds suma) n = fromInteger suma / fromInteger n
mean (Bytes suma)        n = fromInteger suma / fromInteger n
mean (Pure suma)         n = fromInteger suma / fromInteger n

data Stats = Stats {
    fmin   :: Measurable,
    fmax   :: Measurable,
    fcount :: Integer,
    fsum_A :: Measurable,
    fsum_B :: Measurable
    } deriving (Eq, Generic, ToJSON)

-- show instance in S.I.

instance Show Stats where
    show (Stats smin smax scount ssum _) =
        "{ min = " ++ show smin ++
        ", max = " ++ show smax ++
        ", mean = " ++ show (mean ssum scount) ++ showUnits ssum ++
        ", count = " ++ show scount ++
        " }"

\end{code}

\subsubsection{Aggregated}\label{code:Aggregated}
\begin{code}
data Aggregated = Aggregated {
    fstats :: Stats,
    flast  :: Measurable,
    fdelta :: Stats
    -- dy/dx we need to keep the notion of x
    -- since we now only compute the diff on ys
    } deriving (Eq, Generic, ToJSON)

instance Show Aggregated where
    show (Aggregated astats curr _) =
        "{ last measurement = " ++ show curr ++
        ", stats = " ++ show astats ++
        " }"

\end{code}

\subsubsection{Update aggregation}\label{code:updateAggregation}

We distinguish an unitialized from an already initialized aggregation:

\begin{code}
updateAggregation :: Measurable -> Maybe Aggregated -> Maybe Aggregated
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
