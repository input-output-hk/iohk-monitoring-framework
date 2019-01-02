
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
    fmin   :: Measurable,
    fmax   :: Measurable,
    fcount :: Integer,
    fsum_A :: Measurable,
    fsum_B :: Measurable
    } deriving (Eq, Generic, ToJSON)

instance Show Stats where
    show (Stats smin smax scount ssum ssumB) =
        "{ min = " ++ show smin ++
        ", max = " ++ show smax ++
        ", mean = " ++ showMean ssum scount ++
        ", std-dev = " ++ showStdDev ssum ssumB scount ++
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
