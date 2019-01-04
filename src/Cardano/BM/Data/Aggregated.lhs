
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
  , singleton
  , updateAggregation
  ) where

import           GHC.Generics (Generic)
import           Data.Aeson (ToJSON)
import           Data.Scientific (fromFloatDigits)
\end{code}
%endif

\subsubsection{Stats}\label{code:Stats}
\begin{code}

data Measurable = Microseconds Integer
                | Seconds Integer
                | Bytes Integer
                | Pure Integer
                --  Field Text
                deriving (Eq, Ord, Generic, ToJSON)

instance Num Measurable where
    (+) (Microseconds a) (Microseconds b) = Microseconds (a+b)
    (+) (Seconds a)      (Seconds b)      = Seconds      (a+b)
    (+) (Bytes a)        (Bytes b)        = Bytes        (a+b)
    (+) (Pure a)         (Pure b)         = Pure         (a+b)
    (+)  _                _               = error "Trying to add values with different units"

    (*) (Microseconds a) (Microseconds b) = Microseconds (a*b)
    (*) (Seconds a)      (Seconds b)      = Seconds      (a*b)
    (*) (Bytes a)        (Bytes b)        = Bytes        (a*b)
    (*) (Pure a)         (Pure b)         = Pure         (a*b)
    (*)  _                _               = error "Trying to multiply values with different units"

    abs (Microseconds a) = Microseconds (abs a)
    abs (Seconds a)      = Seconds      (abs a)
    abs (Bytes a)        = Bytes        (abs a)
    abs (Pure a)         = Pure         (abs a)

    signum (Microseconds a) = Microseconds (signum a)
    signum (Seconds a)      = Seconds      (signum a)
    signum (Bytes a)        = Bytes        (signum a)
    signum (Pure a)         = Pure         (signum a)

    negate (Microseconds a) = Microseconds (negate a)
    negate (Seconds a)      = Seconds      (negate a)
    negate (Bytes a)        = Bytes        (negate a)
    negate (Pure a)         = Pure         (negate a)

    fromInteger = Pure

instance Show Measurable where
    show = showSI

showUnits :: Measurable -> String
showUnits (Microseconds _) = " µs"
showUnits (Seconds _)      = " s"
showUnits (Bytes _)        = " B"
showUnits (Pure _)         = ""

showMean :: Measurable -> Integer -> String
showMean   (Microseconds suma) n = show (fromFloatDigits (mean suma (n*1000000))) ++
                                    showUnits (Seconds 0)
showMean v@(Seconds suma)      n = show (mean suma n) ++ showUnits v
showMean v@(Bytes suma)        n = show (mean suma n) ++ showUnits v
showMean v@(Pure suma)         n = show (mean suma n) ++ showUnits v

showStdDev :: Measurable -> Measurable -> Integer -> String
showStdDev   (Microseconds suma) (Microseconds sumb) n = show (fromFloatDigits
                                                              (stdDev suma sumb n 1000000)) ++
                                                         showUnits (Seconds 0)
showStdDev v@(Seconds suma)      (Seconds sumb)      n = show (stdDev suma sumb n 1) ++ showUnits v
showStdDev v@(Bytes suma)        (Bytes sumb)        n = show (stdDev suma sumb n 1) ++ showUnits v
showStdDev v@(Pure suma)         (Pure sumb)         n = show (stdDev suma sumb n 1) ++ showUnits v
showStdDev _                      _                  _ = error "Different units or quantities used"

stdDev :: Integer -> Integer -> Integer -> Integer -> Float
stdDev suma sumb n scale = let
                        mu = mean suma n
                        muSquares = fromInteger sumb / fromInteger n
                    in
                    (sqrt (muSquares - (mu*mu))) / fromInteger scale

mean :: Integer -> Integer -> Float
mean suma n = fromInteger suma / fromInteger n

-- show in S.I.
showSI :: Measurable -> String
showSI (Microseconds a) = show (fromFloatDigits ((fromInteger a) / (1000000::Float))) ++
                          showUnits (Seconds a)
showSI v@(Seconds a)    = show a ++ showUnits v
showSI v@(Bytes a)      = show a ++ showUnits v
showSI v@(Pure a)       = show a ++ showUnits v

data Stats = Stats {
    flast  :: Measurable,
    fmin   :: Measurable,
    fmax   :: Measurable,
    fcount :: Integer,
    fsum_A :: Measurable,
    fsum_B :: Measurable
    } deriving (Eq, Generic, ToJSON)

instance Show Stats where
    show (Stats slast smin smax scount ssum ssumB) =
        "{ last = " ++ show slast ++
        ", min = " ++ show smin ++
        ", max = " ++ show smax ++
        ", mean = " ++ showMean ssum scount ++
        ", std-dev = " ++ showStdDev ssum ssumB scount ++
        ", count = " ++ show scount ++
        " }"

\end{code}

\subsubsection{Aggregated}\label{code:Aggregated}
\begin{code}
data Aggregated = AggregatedStats Stats
                | AggregatedEWMA StatsEWMA
                deriving (Eq, Generic, ToJSON)

type StatsEWMA = [Int] -- placekeeper

instance Semigroup Stats where
    (<>) aStats bStats = Stats {
                            flast  = flast bStats, -- right associative
                            fmin   = min (fmin aStats) (fmin bStats),
                            fmax   = max (fmax aStats) (fmax bStats),
                            fcount = fcount aStats + fcount bStats,
                            fsum_A = fsum_A aStats + fsum_A bStats,
                            fsum_B = fsum_B aStats + fsum_B bStats
                        }

instance Semigroup Aggregated where
    (<>) (AggregatedStats aStats) (AggregatedStats bStats) =
        AggregatedStats (aStats <> bStats)
    (<>) _ _ = error "Cannot combine different objects"

singleton :: Measurable -> Aggregated
singleton a =
    let
        stats = Stats {
                    flast  = a,
                    fmin   = a,
                    fmax   = a,
                    fcount = 1,
                    fsum_A = a,
                    fsum_B = a*a
                }
    in
    AggregatedStats stats

instance Show Aggregated where
    show (AggregatedStats astats) =
        "{ stats = " ++ show astats ++
        " }"
    show (AggregatedEWMA a) = show a

\end{code}

\subsubsection{Update aggregation}\label{code:updateAggregation}

We distinguish an unitialized from an already initialized aggregation:

\begin{code}
updateAggregation :: Measurable -> Maybe Aggregated -> Maybe Aggregated
updateAggregation v Nothing =
    Just $ singleton v
updateAggregation v (Just agg@(AggregatedStats _)) =
    Just $ agg <> singleton v
updateAggregation _ _ = undefined

\end{code}
