
\subsection{Cardano.BM.Data.Counter}

%if style == newcode
\begin{code}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.BM.Data.Counter
  ( Counter (..)
  , CounterType (..)
  , CounterState (..)
  , diffCounters
  , nameCounter
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
data Counter = Counter
                { cType :: CounterType
                , cName :: Text
                , cValue :: Integer
                }
               deriving (Eq, Show, Generic, ToJSON)

data CounterType = MonotonicClockTime
                 | MemoryCounter
                 | StatInfo
                 | IOCounter
                 | CpuCounter
                   deriving (Eq, Show, Generic, ToJSON)

nameCounter :: Counter -> Text
nameCounter (Counter MonotonicClockTime _ _) = "Time"
nameCounter (Counter MemoryCounter      _ _) = "Mem"
nameCounter (Counter StatInfo           _ _) = "Stat"
nameCounter (Counter IOCounter          _ _) = "IO"
nameCounter (Counter CpuCounter         _ _) = "Cpu"

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
    getCountersDiff openings closings MonotonicClockTime ++
    getCountersDiff openings closings MemoryCounter      ++
    getCountersDiff openings closings StatInfo           ++
    getCountersDiff openings closings IOCounter          ++
    getCountersDiff openings closings CpuCounter
  where
    getCountersDiff :: [Counter]
                    -> [Counter]
                    -> CounterType
                    -> [Counter]
    getCountersDiff as bs counterType =
        let
            predicate = (counterType ==) . cType
            as' = filter predicate as
            bs' = filter predicate bs
            aPairs = getPairs as'
            bPairs = HM.fromList $ getPairs bs'
        in
            catMaybes $ (flip map) aPairs $ \(name, startValue) ->
                case HM.lookup name bPairs of
                    Nothing       -> Nothing
                    Just endValue -> Just (Counter counterType name (endValue - startValue))
          where
            getPairs :: [Counter] -> [(Text, Integer)]
            getPairs (Counter _ t v : xs) = (t,v) : getPairs xs
            getPairs _ = []

\end{code}
