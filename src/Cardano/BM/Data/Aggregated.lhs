
\subsection{Cardano.BM.Data.Aggregated}

%if style == newcode
\begin{code}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.BM.Data.Aggregated
  ( Aggregated (..)
  , Stats (..)
  , EWMA (..)
  , Measurable (..)
  , getInteger
  , getRational
  , singleton
  , updateAggregation
  ) where

import           GHC.Generics (Generic)
import           Data.Aeson (ToJSON)
import           Data.Scientific (fromFloatDigits)
\end{code}
%endif

\subsubsection{Measurable}\label{code:Measurable}\index{Measurable}
A |Measurable| may consist of different types of values.

\begin{code}
data Measurable = Microseconds Integer
                | Seconds Integer
                | Bytes Integer
                | PureI Integer
                | PureD Rational
                deriving (Eq, Ord, Generic, ToJSON)

\end{code}

|Measurable| can be transformed to an integral value.
\begin{code}
getInteger :: Measurable -> Integer
getInteger (Microseconds a) = a
getInteger (Seconds a)      = a
getInteger (Bytes a)        = a
getInteger (PureI a)        = a
getInteger (PureD a)        = round a
-- getInteger _                = error "do not know this instance"

\end{code}

|Measurable| can be transformed to a rational value.
\begin{code}
getRational :: Measurable -> Rational
getRational (Microseconds a) = toRational a
getRational (Seconds a)      = toRational a
getRational (Bytes a)        = toRational a
getRational (PureI a)        = toRational a
getRational (PureD a)        = toRational a
-- getInteger _                = error "do not know this instance"

\end{code}

It is a numerical value, thus supports functions to operate on numbers.
\index{Measurable!instance of Num}
\begin{code}
instance Num Measurable where
    (+) (Microseconds a) (Microseconds b) = Microseconds (a+b)
    (+) (Seconds a)      (Seconds b)      = Seconds      (a+b)
    (+) (Bytes a)        (Bytes b)        = Bytes        (a+b)
    (+) (PureI a)        (PureI b)        = PureI        (a+b)
    (+) (PureD a)        (PureD b)        = PureD        (a+b)
    (+)  _                _               = error "Trying to add values with different units"

    (*) (Microseconds a) (Microseconds b) = Microseconds (a*b)
    (*) (Seconds a)      (Seconds b)      = Seconds      (a*b)
    (*) (Bytes a)        (Bytes b)        = Bytes        (a*b)
    (*) (PureI a)        (PureI b)        = PureI        (a*b)
    (*) (PureD a)        (PureD b)        = PureD        (a*b)
    (*)  _                _               = error "Trying to multiply values with different units"

    abs (Microseconds a) = Microseconds (abs a)
    abs (Seconds a)      = Seconds      (abs a)
    abs (Bytes a)        = Bytes        (abs a)
    abs (PureI a)        = PureI        (abs a)
    abs (PureD a)        = PureD        (abs a)

    signum (Microseconds a) = Microseconds (signum a)
    signum (Seconds a)      = Seconds      (signum a)
    signum (Bytes a)        = Bytes        (signum a)
    signum (PureI a)        = PureI        (signum a)
    signum (PureD a)        = PureD        (signum a)

    negate (Microseconds a) = Microseconds (negate a)
    negate (Seconds a)      = Seconds      (negate a)
    negate (Bytes a)        = Bytes        (negate a)
    negate (PureI a)        = PureI        (negate a)
    negate (PureD a)        = PureD        (negate a)

    fromInteger = PureI

\end{code}

Pretty printing of |Measurable|. \index{Measurable!instance of Show}
\begin{code}
instance Show Measurable where
    show = showSI

showUnits :: Measurable -> String
showUnits (Microseconds _) = " µs"
showUnits (Seconds _)      = " s"
showUnits (Bytes _)        = " B"
showUnits (PureI _)        = ""
showUnits (PureD _)        = ""

showMean :: Measurable -> Integer -> String
showMean   (Microseconds suma) n = show (fromFloatDigits (mean suma (n*1000000))) ++
                                    showUnits (Seconds 0)
showMean v@(Seconds suma)      n = show (mean suma n) ++ showUnits v
showMean v@(Bytes suma)        n = show (mean suma n) ++ showUnits v
showMean v@(PureI suma)        n = show (mean suma n) ++ showUnits v
showMean v@(PureD suma)        n = show (mean (round suma) n) ++ showUnits v

showStdDev :: Measurable -> Measurable -> Integer -> String
showStdDev   (Microseconds suma) (Microseconds sumb) n = show (fromFloatDigits
                                                              (stdDev suma sumb n 1000000)) ++
                                                         showUnits (Seconds 0)
showStdDev v@(Seconds suma)      (Seconds sumb)      n = show (stdDev suma sumb n 1) ++ showUnits v
showStdDev v@(Bytes suma)        (Bytes sumb)        n = show (stdDev suma sumb n 1) ++ showUnits v
showStdDev v@(PureI suma)        (PureI sumb)        n = show (stdDev suma sumb n 1) ++ showUnits v
showStdDev v@(PureD suma)        (PureD sumb)        n = show (stdDev (round suma) (round sumb) n 1) ++ showUnits v
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
showSI v@(PureI a)      = show a ++ showUnits v
showSI v@(PureD a)      = show (fromRational a :: Float) ++ showUnits v

\end{code}

\subsubsection{Stats}\label{code:Stats}\index{Stats}
\begin{code}
data Stats = Stats {
    flast  :: Measurable,
    fmin   :: Measurable,
    fmax   :: Measurable,
    fcount :: Integer,
    fsum_A :: Measurable,
    fsum_B :: Measurable
    } deriving (Eq, Generic, ToJSON)

\end{code}

\index{Stats!instance of Semigroup}
\begin{code}
instance Semigroup Stats where
    (<>) a b = Stats { flast  = flast b  -- right associative
                     , fmin   = min (fmin a) (fmin b)
                     , fmax   = max (fmax a) (fmax b)
                     , fcount = fcount a + fcount b
                     , fsum_A = fsum_A a + fsum_A b
                     , fsum_B = fsum_B a + fsum_B b
                     }

\end{code}

\index{Stats!instance of Show}
\begin{code}
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

\subsubsection{Exponentially Weighted Moving Average (EWMA)}\label{code:EWMA}\index{EWMA}
\begin{code}
data EWMA = EmptyEWMA { alpha :: Float }
          | EWMA { alpha :: Float
                 , avg   :: Measurable
                 } deriving (Show, Eq, Generic, ToJSON)

\end{code}

\subsubsection{Aggregated}\label{code:Aggregated}\index{Aggregated}
\todo[inline]{the sums |fsum_A| and even more so |fsum_B| can grow to very large numbers!
\newline
We need to implement another incremental method to update mean and variance.}
\begin{code}
data Aggregated = AggregatedStats Stats
                | AggregatedEWMA EWMA
                deriving (Eq, Generic, ToJSON)

\end{code}

\index{Aggregated!instance of Semigroup}
\begin{code}
instance Semigroup Aggregated where
    (<>) (AggregatedStats a) (AggregatedStats b) =
        AggregatedStats (a <> b)
    (<>) _ _ = error "Cannot combine different objects"

\end{code}

\index{singleton}
\begin{code}
singleton :: Measurable -> Aggregated
singleton a =
    let stats = Stats { flast  = a
                      , fmin   = a
                      , fmax   = a
                      , fcount = 1
                      , fsum_A = a
                      , fsum_B = a*a
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

\subsubsection{Update aggregation}\label{code:updateAggregation}\index{updateAggregation}
We distinguish an unitialized from an already initialized aggregation:
\begin{code}
updateAggregation :: Measurable -> Maybe Aggregated -> Maybe Aggregated
updateAggregation v Nothing =
    Just $ singleton v
updateAggregation v (Just agg@(AggregatedStats _)) =
    Just $ agg <> singleton v
updateAggregation v (Just (AggregatedEWMA e)) =
    Just $ AggregatedEWMA $ ewma e v

\end{code}

\subsubsection{Calculation of EWMA}\label{code:ewma}\index{ewma}
Following \url{https://en.wikipedia.org/wiki/Moving_average#Exponential_moving_average} we calculate
the exponential moving average for a series of values $ Y_t $ according to:

$$
S_t =
\begin{cases}
  Y_1,       & t = 1 \\
  \alpha \cdot Y_t + (1 - \alpha) \cdot S_{t-1},    & t > 1
\end{cases}
$$
\\
The pattern matching below ensures that the |EWMA| will start with the first value passed in,
and will not change type, once determined.
\begin{code}
ewma :: EWMA -> Measurable -> EWMA
ewma (EmptyEWMA a) v = EWMA a v
ewma (EWMA a (Microseconds s)) (Microseconds y) =
    EWMA a $ Microseconds $ round $ a * (fromInteger y) + (1 - a) * (fromInteger s)
ewma (EWMA a (Seconds s)) (Seconds y) =
    EWMA a $ Seconds $ round $ a * (fromInteger y) + (1 - a) * (fromInteger s)
ewma (EWMA a (Bytes s)) (Bytes y) =
    EWMA a $ Bytes $ round $ a * (fromInteger y) + (1 - a) * (fromInteger s)
ewma (EWMA a (Pure s)) (Pure y) =
    EWMA a $ Pure $ round $ a * (fromInteger y) + (1 - a) * (fromInteger s)
ewma _ _ = error "Cannot average on values of different type"

\end{code}
