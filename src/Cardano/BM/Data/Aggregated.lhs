
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
  , getDouble
  , meanOfStats
  , stdevOfStats
  , stats2Text
  , singleton
  ) where

import           GHC.Generics (Generic)
import           Data.Aeson (ToJSON)
import           Data.Scientific (fromFloatDigits)
import           Data.Text (Text, pack)
\end{code}
%endif

\subsubsection{Measurable}\label{code:Measurable}\index{Measurable}
A |Measurable| may consist of different types of values.

\begin{code}
data Measurable = Microseconds Integer
                | Seconds Integer
                | Bytes Integer
                | PureI Integer
                | PureD Double
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

\end{code}

|Measurable| can be transformed to a rational value.
\begin{code}
getDouble :: Measurable -> Double
getDouble (Microseconds a) = fromInteger a
getDouble (Seconds a)      = fromInteger a
getDouble (Bytes a)        = fromInteger a
getDouble (PureI a)        = fromInteger a
getDouble (PureD a)        = a

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

-- show in S.I. units
showSI :: Measurable -> String
showSI (Microseconds a) = show (fromFloatDigits ((fromInteger a) / (1000000::Float))) ++
                          showUnits (Seconds a)
showSI v@(Seconds a)    = show a ++ showUnits v
showSI v@(Bytes a)      = show a ++ showUnits v
showSI v@(PureI a)      = show a ++ showUnits v
showSI v@(PureD a)      = show a ++ showUnits v

\end{code}

\subsubsection{Stats}\label{code:Stats}\index{Stats}
\begin{code}
data Stats = Stats {
    flast  :: Measurable,
    fmin   :: Measurable,
    fmax   :: Measurable,
    fcount :: Integer,
    fsum_A :: Double,
    fsum_B :: Double
    } deriving (Eq, Generic, ToJSON, Show)

\end{code}

\begin{code}
meanOfStats :: Stats -> Double
meanOfStats s = fsum_A s

\end{code}

\begin{code}
stdevOfStats :: Stats -> Double
stdevOfStats s =
    if fcount s < 2
    then 0
    else sqrt $ (fsum_B s) / (fromInteger $ (fcount s) - 1)

\end{code}

\index{Stats!instance of Semigroup}
We use a parallel algorithm to update the estimation of mean and variance from two sample statistics.
(see \url{https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Parallel_algorithm})

\begin{code}
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

\end{code}

\label{code:stats2Text}\index{stats2Text}
\begin{code}
stats2Text :: Stats -> Text
stats2Text s@(Stats slast smin smax scount _ _) =
    pack $
        "{ last = " ++ show slast ++
        ", min = " ++ show smin ++
        ", max = " ++ show smax ++
        ", mean = " ++ show (meanOfStats s) ++ showUnits slast ++
        ", std-dev = " ++ show (stdevOfStats s) ++
        ", count = " ++ show scount ++
        " }"

\end{code}

\subsubsection{Exponentially Weighted Moving Average (EWMA)}\label{code:EWMA}\index{EWMA}
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
                      , fsum_A = getDouble a
                      , fsum_B = 0
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
