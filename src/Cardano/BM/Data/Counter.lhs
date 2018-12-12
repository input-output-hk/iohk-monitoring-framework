
\subsection{Cardano.BM.Data.Counter}

%if style == newcode
\begin{code}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.BM.Data.Counter
  ( Counter (..)
  , CounterState (..)
  , diffCounters
  )
  where

import           Data.Aeson (ToJSON, toEncoding, toJSON)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (catMaybes)
import           Data.Text (Text)
import           Data.Time.Units (Microsecond, toMicroseconds)
import           Data.Unique (Unique, hashUnique)

import           GHC.Generics (Generic)

\end{code}
%endif


\subsubsection{Counter}\label{code:Counter}
\begin{code}
data Counter = MonotonicClockTime Text Microsecond
             | MemoryCounter Text Integer
             | StatInfo Text Integer
             | IOCounter Text Integer
             | CpuCounter Text Integer
               deriving (Eq, Show, Generic, ToJSON)

instance ToJSON Microsecond where
    toJSON     = toJSON     . toMicroseconds
    toEncoding = toEncoding . toMicroseconds

\end{code}

\subsubsection{CounterState}\label{code:CounterState}
\begin{code}
data CounterState = CounterState {
      csIdentifier :: Unique
    , csCounters   :: [Counter]
    }
    deriving (Generic, ToJSON)

instance ToJSON Unique where
    toJSON     = toJSON     . hashUnique
    toEncoding = toEncoding . hashUnique

instance Show CounterState where
    show cs = (show . hashUnique) (csIdentifier cs)
           <> " => " <> (show $ csCounters cs)

diffCounters :: [Counter] -> [Counter] -> [Counter]
diffCounters openings closings =
    getMonotonicClockTimeDiff openings closings ++
    getCountersDiff openings closings isMemoryCounter MemoryCounter ++
    getCountersDiff openings closings isStatInfo      StatInfo      ++
    getCountersDiff openings closings isIOCounter     IOCounter     ++
    getCountersDiff openings closings isCpuCounter    CpuCounter
  where
    getCountersDiff :: [Counter]
                    -> [Counter]
                    -> (Counter -> Bool)
                    -> (Text -> Integer -> Counter)
                    -> [Counter]
    getCountersDiff as bs predicate constructor =
        let
            as' = filter predicate as
            bs' = filter predicate bs
            aPairs = getPairs as'
            bPairs = HM.fromList $ getPairs bs'
        in
            catMaybes $ (flip map) aPairs $ \(name, startValue) ->
                case HM.lookup name bPairs of
                    Nothing       -> Nothing
                    Just endValue -> Just (constructor name (endValue - startValue))
          where
            getPairs :: [Counter] -> [(Text, Integer)]
            getPairs (MonotonicClockTime _ _ : xs) =         getPairs xs
            getPairs (MemoryCounter      t v : xs) = (t,v) : getPairs xs
            getPairs (StatInfo           t v : xs) = (t,v) : getPairs xs
            getPairs (IOCounter          t v : xs) = (t,v) : getPairs xs
            getPairs (CpuCounter         t v : xs) = (t,v) : getPairs xs
            getPairs _ = []

    getMonotonicClockTimeDiff :: [Counter] -> [Counter] -> [Counter]
    getMonotonicClockTimeDiff as bs =
        let
            as' = filter isMonotonicClockTime as
            bs' = filter isMonotonicClockTime bs
            aPairs = getPairs as'
            bPairs = HM.fromList $ getPairs bs'
        in
            catMaybes $ (flip map) aPairs $ \(name, startValue) ->
                case HM.lookup name bPairs of
                    Nothing       -> Nothing
                    Just endValue -> Just (MonotonicClockTime name (endValue - startValue))
          where
            getPairs :: [Counter] -> [(Text, Microsecond)]
            getPairs (MonotonicClockTime t v : xs) = (t,v) : getPairs xs
            getPairs _                             = []

    isMonotonicClockTime :: Counter -> Bool
    isMonotonicClockTime (MonotonicClockTime _ _) = True
    isMonotonicClockTime _                        = False

    isMemoryCounter :: Counter -> Bool
    isMemoryCounter (MemoryCounter _ _) = True
    isMemoryCounter _                   = False

    isStatInfo :: Counter -> Bool
    isStatInfo (StatInfo _ _) = True
    isStatInfo _              = False

    isIOCounter :: Counter -> Bool
    isIOCounter (IOCounter _ _) = True
    isIOCounter _               = False

    isCpuCounter :: Counter -> Bool
    isCpuCounter (CpuCounter _ _) = True
    isCpuCounter _                = False

\end{code}
