
\subsection{Cardano.BM.Counters}

Here the platform is chosen on which we compile this program.

Currently, we mainly support |Linux| with its 'proc' filesystem.

\begin{code}
{-# LANGUAGE CPP #-}

#if defined(linux_HOST_OS)
#define LINUX
#endif

module Cardano.BM.Counters
    (
      Platform.readCounters
    , diffTimeObserved
    , getMonoClock
    ) where

#ifdef LINUX
import qualified Cardano.BM.Counters.Linux as Platform
#else
import qualified Cardano.BM.Counters.Dummy as Platform
#endif

import           Cardano.BM.Counters.Common (getMonoClock)
import           Cardano.BM.Data.Aggregated (Measurable (..))
import           Cardano.BM.Data.Counter

\end{code}

\subsubsection{Calculate difference between clocks}\label{code:diffTimeObserved}\index{diffTimeObserved}

\begin{code}
diffTimeObserved :: CounterState -> CounterState -> Measurable
diffTimeObserved (CounterState id0 startCounters) (CounterState id1 endCounters) =
    let
        startTime = getMonotonicTime startCounters
        endTime   = getMonotonicTime endCounters
    in
    if (id0 == id1)
      then endTime - startTime
      else error "these clocks are not from the same experiment"
  where
    getMonotonicTime counters = case (filter isMonotonicClockCounter counters) of
        [(Counter MonotonicClockTime _ mus)] -> mus
        _                                    -> error "A time measurement is missing!"

    isMonotonicClockCounter :: Counter -> Bool
    isMonotonicClockCounter = (MonotonicClockTime ==) . cType

\end{code}

