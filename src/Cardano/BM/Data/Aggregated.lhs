
\subsection{Cardano.BM.Data.Aggregated}

%if style == newcode
\begin{code}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.BM.Data.Aggregated
  ( Aggregated (..)
  , Stats (..)
  , BaseStats (..)
  , EWMA (..)
  , Measurable (..)
  , showSI
  , getInteger
  , getDouble
  , meanOfStats
  , stdevOfStats
  , stats2Text
  , singletonStats
  ) where

import           GHC.Generics (Generic)
import           Data.Aeson (ToJSON)
import           Data.Scientific (fromFloatDigits)
import           Data.Text (Text, pack)
import           Data.Word (Word64)
\end{code}
%endif

\subsubsection{Measurable}\label{code:Measurable}\index{Measurable}
A |Measurable| may consist of different types of values.
Time measurements are strict, so are |Bytes| which are externally measured.
The real or integral numeric values are lazily linked, so we can decide later
to drop them.
\begin{code}
data Measurable = Microseconds {-# UNPACK #-} !Word64
                | Nanoseconds  {-# UNPACK #-} !Word64
                | Seconds      {-# UNPACK #-} !Word64
                | Bytes        {-# UNPACK #-} !Word64
                | PureD        Double
                | PureI        Integer
                deriving (Eq, Ord, Generic, ToJSON)

\end{code}

|Measurable| can be transformed to an integral value.
\begin{code}
getInteger :: Measurable -> Integer
getInteger (Microseconds a) = toInteger a
getInteger (Nanoseconds a)  = toInteger a
getInteger (Seconds a)      = toInteger a
getInteger (Bytes a)        = toInteger a
getInteger (PureI a)        = a
getInteger (PureD a)        = round a

\end{code}

|Measurable| can be transformed to a rational value.
\begin{code}
getDouble :: Measurable -> Double
getDouble (Microseconds a) = fromIntegral a
getDouble (Nanoseconds a)  = fromIntegral a
getDouble (Seconds a)      = fromIntegral a
getDouble (Bytes a)        = fromIntegral a
getDouble (PureI a)        = fromInteger a
getDouble (PureD a)        = a

\end{code}

It is a numerical value, thus supports functions to operate on numbers.
\index{Measurable!instance of Num}
\begin{code}
instance Num Measurable where
    (+) (Microseconds a) (Microseconds b) = Microseconds (a+b)
    (+) (Nanoseconds a)  (Nanoseconds b)  = Nanoseconds  (a+b)
    (+) (Seconds a)      (Seconds b)      = Seconds      (a+b)
    (+) (Bytes a)        (Bytes b)        = Bytes        (a+b)
    (+) (PureI a)        (PureI b)        = PureI        (a+b)
    (+) (PureD a)        (PureD b)        = PureD        (a+b)
    (+)  _                _               = error "Trying to add values with different units"

    (*) (Microseconds a) (Microseconds b) = Microseconds (a*b)
    (*) (Nanoseconds a)  (Nanoseconds b)  = Nanoseconds  (a*b)
    (*) (Seconds a)      (Seconds b)      = Seconds      (a*b)
    (*) (Bytes a)        (Bytes b)        = Bytes        (a*b)
    (*) (PureI a)        (PureI b)        = PureI        (a*b)
    (*) (PureD a)        (PureD b)        = PureD        (a*b)
    (*)  _                _               = error "Trying to multiply values with different units"

    abs (Microseconds a) = Microseconds (abs a)
    abs (Nanoseconds a)  = Nanoseconds (abs a)
    abs (Seconds a)      = Seconds      (abs a)
    abs (Bytes a)        = Bytes        (abs a)
    abs (PureI a)        = PureI        (abs a)
    abs (PureD a)        = PureD        (abs a)

    signum (Microseconds a) = Microseconds (signum a)
    signum (Nanoseconds a)  = Nanoseconds  (signum a)
    signum (Seconds a)      = Seconds      (signum a)
    signum (Bytes a)        = Bytes        (signum a)
    signum (PureI a)        = PureI        (signum a)
    signum (PureD a)        = PureD        (signum a)

    negate (Microseconds a) = Microseconds (negate a)
    negate (Nanoseconds a)  = Nanoseconds (negate a)
    negate (Seconds a)      = Seconds      (negate a)
    negate (Bytes a)        = Bytes        (negate a)
    negate (PureI a)        = PureI        (negate a)
    negate (PureD a)        = PureD        (negate a)

    fromInteger = PureI

\end{code}

Pretty printing of |Measurable|. \index{Measurable!instance of Show}
\begin{code}
instance Show Measurable where
    show (Microseconds a) = show a  -- ++ showUnits v
    show (Nanoseconds a)  = show a  -- ++ showUnits v
    show (Seconds a)      = show a  -- ++ showUnits v
    show (Bytes a)        = show a  -- ++ showUnits v
    show (PureI a)        = show a  -- ++ showUnits v
    show (PureD a)        = show a  -- ++ showUnits v

showUnits :: Measurable -> String
showUnits (Microseconds _) = " µs"
showUnits (Nanoseconds _)  = " ns"
showUnits (Seconds _)      = " s"
showUnits (Bytes _)        = " B"
showUnits (PureI _)        = ""
showUnits (PureD _)        = ""

-- show in S.I. units
showSI :: Measurable -> String
showSI (Microseconds a) = show (fromFloatDigits ((fromIntegral a) / (1000000::Float))) ++
                          showUnits (Seconds a)
showSI (Nanoseconds a)  = show (fromFloatDigits ((fromIntegral a) / (1000000000::Float))) ++
                          showUnits (Seconds a)
showSI v@(Seconds a)    = show a ++ showUnits v
showSI v@(Bytes a)      = show a ++ showUnits v
showSI v@(PureI a)      = show a ++ showUnits v
showSI v@(PureD a)      = show a ++ showUnits v

\end{code}

\subsubsection{Stats}\label{code:Stats}\index{Stats}
A |Stats| statistics is strictly computed.
\begin{code}
data BaseStats = BaseStats {
    fmin   :: !Measurable,
    fmax   :: !Measurable,
    fcount :: !Word64,
    fsum_A :: !Double,
    fsum_B :: !Double
    } deriving (Generic, ToJSON, Show)

instance Eq BaseStats where
    (BaseStats mina maxa counta sumAa sumBa) == (BaseStats minb maxb countb sumAb sumBb) =
        mina == minb && maxa == maxb && counta == countb &&
        abs (sumAa - sumAb) < 1.0e-4 &&
        abs (sumBa - sumBb) < 1.0e-4

data Stats = Stats {
    flast  :: !Measurable,
    fold   :: !Measurable,
    fbasic :: !BaseStats,
    fdelta :: !BaseStats,
    ftimed :: !BaseStats
    } deriving (Eq, Generic, ToJSON, Show)

\end{code}

\begin{code}
meanOfStats :: BaseStats -> Double
meanOfStats = fsum_A

\end{code}

\begin{code}
stdevOfStats :: BaseStats -> Double
stdevOfStats s =
    if fcount s < 2
    then 0
    else sqrt $ (fsum_B s) / (fromInteger $ fromIntegral (fcount s) - 1)

\end{code}

\index{Stats!instance of Semigroup}
\todo[inline]{|instance Semigroup Stats| disabled for the moment, because not needed.}
We use a parallel algorithm to update the estimation of mean and variance from two sample statistics.
(see \url{https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Parallel_algorithm})

\begin{spec}
instance Semigroup Stats where
    (<>) a b = let counta = fcount a
                   countb = fcount b
                   newcount = counta + countb
                   delta = fsum_A b - fsum_A a
               in
               Stats { flast  = flast b  -- right associative
                     , fmin   = min (fmin a) (fmin b)
                     , fmax   = max (fmax a) (fmax b)
                     , fcount = newcount
                     , fsum_A = fsum_A a + (delta / fromInteger newcount)
                     , fsum_B = fsum_B a + fsum_B b + (delta * delta) * (fromInteger (counta * countb) / fromInteger newcount)
                     }

\end{spec}

\label{code:stats2Text}\index{stats2Text}
\begin{code}
stats2Text :: Stats -> Text
stats2Text (Stats slast _ sbasic sdelta stimed) =
    pack $
        "{ last=" ++ show slast ++
        ", basic-stats=" ++ showStats' (sbasic) ++
        ", delta-stats=" ++ showStats' (sdelta) ++
        ", timed-stats=" ++ showStats' (stimed) ++
        " }"
  where
    showStats' :: BaseStats -> String
    showStats' s = 
        ", { min=" ++ show  (fmin s) ++
        ", max=" ++ show (fmax s) ++
        ", mean=" ++ show (meanOfStats s) ++ showUnits (fmin s) ++
        ", std-dev=" ++ show (stdevOfStats s) ++
        ", count=" ++ show (fcount s) ++
        " }"

\end{code}

\subsubsection{Exponentially Weighted Moving Average (EWMA)}\label{code:EWMA}\index{EWMA}
Following \url{https://en.wikipedia.org/wiki/Moving_average#Exponential_moving_average} we calculate
the exponential moving average for a series of values $ Y_t $ according to:

$$
S_t =
\begin{cases}
  Y_1,       & t = 1 \\
  \alpha \cdot Y_t + (1 - \alpha) \cdot S_{t-1},    & t > 1
\end{cases}
$$
\begin{code}
data EWMA = EmptyEWMA { alpha :: Double }
          | EWMA { alpha :: Double
                 , avg   :: Measurable
                 } deriving (Show, Eq, Generic, ToJSON)

\end{code}

\subsubsection{Aggregated}\label{code:Aggregated}\index{Aggregated}
\begin{code}
data Aggregated = AggregatedStats Stats
                | AggregatedEWMA EWMA
                deriving (Eq, Generic, ToJSON)

\end{code}

\index{Aggregated!instance of Semigroup}
\todo[inline]{|instance Semigroup Aggregated| disabled for the moment, because not needed.}
\begin{spec}
instance Semigroup Aggregated where
    (<>) (AggregatedStats a) (AggregatedStats b) =
        AggregatedStats (a <> b)
    (<>) _ _ = error "Cannot combine different objects"

\end{spec}

\label{code:singletonStats}\index{singletonStats}
\begin{code}
singletonStats :: Measurable -> Aggregated
singletonStats a =
    let stats = Stats { flast  = a
                      , fold   = 0
                      , fbasic = BaseStats
                                 { fmin   = a
                                 , fmax   = a
                                 , fcount = 1
                                 , fsum_A = getDouble a
                                 , fsum_B = 0 }
                      , fdelta = BaseStats
                                 { fmin   = 0
                                 , fmax   = 0
                                 , fcount = 0
                                 , fsum_A = 0
                                 , fsum_B = 0 }
                      , ftimed = BaseStats
                                 { fmin   = 0
                                 , fmax   = 0
                                 , fcount = 0
                                 , fsum_A = 0
                                 , fsum_B = 0 }
                      }
    in
    AggregatedStats stats

\end{code}

\index{Aggregated!instance of Show}
\begin{code}
instance Show Aggregated where
    show (AggregatedStats astats) =
        "{ stats = " ++ show astats ++ " }"
    show (AggregatedEWMA a) = show a

\end{code}
