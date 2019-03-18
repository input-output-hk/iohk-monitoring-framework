
\subsection{Cardano.BM.Data.Counter}
\label{code:Cardano.BM.Data.Counter}

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

import           Cardano.BM.Data.Aggregated (Measurable(..))

\end{code}
%endif


\subsubsection{Counter}\label{code:Counter}\index{Counter}\label{code:CounterType}\index{CounterType}
\begin{code}
data Counter = Counter
               { cType  :: CounterType
               , cName  :: Text
               , cValue :: Measurable
               }
               deriving (Eq, Show, Generic, ToJSON)

data CounterType = MonotonicClockTime
                 | MemoryCounter
                 | StatInfo
                 | IOCounter
                 | NetCounter
                 | CpuCounter
                 | RTSStats
                   deriving (Eq, Show, Generic, ToJSON)

instance ToJSON Microsecond where
    toJSON     = toJSON     . toMicroseconds
    toEncoding = toEncoding . toMicroseconds

\end{code}

\subsubsection{Names of counters}\label{code:nameCounter}\index{nameCounter}
\begin{code}
nameCounter :: Counter -> Text
nameCounter (Counter MonotonicClockTime _ _) = "Time-interval"
nameCounter (Counter MemoryCounter      _ _) = "Mem"
nameCounter (Counter StatInfo           _ _) = "Stat"
nameCounter (Counter IOCounter          _ _) = "IO"
nameCounter (Counter NetCounter         _ _) = "Net"
nameCounter (Counter CpuCounter         _ _) = "Cpu"
nameCounter (Counter RTSStats           _ _) = "RTS"

\end{code}

\subsubsection{CounterState}\label{code:CounterState}\index{CounterState}
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

\end{code}

\subsubsection{Difference between counters}\label{code:diffCounters}\index{diffCounters}
\begin{code}
diffCounters :: [Counter] -> [Counter] -> [Counter]
diffCounters openings closings =
    getCountersDiff openings closings
  where
    getCountersDiff :: [Counter]
                    -> [Counter]
                    -> [Counter]
    getCountersDiff as bs =
        let
            getName counter = nameCounter counter <> cName counter

            asNames = map getName as
            aPairs = zip asNames as

            bsNames = map getName bs
            bs' = zip bsNames bs
            bPairs = HM.fromList bs'
        in
            catMaybes $ (flip map) aPairs $ \(name, Counter _ _ startValue) ->
                case HM.lookup name bPairs of
                    Nothing       -> Nothing
                    Just counter  -> let endValue = cValue counter
                                     in Just counter {cValue = endValue - startValue}

\end{code}
