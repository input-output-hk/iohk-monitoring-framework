
\subsection{Cardano.BM.Counters.Dummy}

This is a dummy definition of |readCounters| on platforms that do not support the
'proc' filesystem from which we would read the counters.

The only supported measurement is monotonic clock time for now.

\todo[inline]{we could well imagine that some day we support all platforms}

%if style == newcode
\begin{code}
module Cardano.BM.Counters.Dummy
    (
      readCounters
    ) where

import           Data.Foldable (foldrM)
import           Data.Set (member)

import           Cardano.BM.Counters.Common (getMonoClock)
import           Cardano.BM.Data.Counter
import           Cardano.BM.Data.Observable
import           Cardano.BM.Data.SubTrace
\end{code}
%endif

\begin{code}
readCounters :: SubTrace -> IO [Counter]
readCounters NoTrace             = return []
readCounters Neutral             = return []
readCounters UntimedTrace        = return []
readCounters DropOpening         = return []
readCounters (ObservableTrace tts) = foldrM (\(sel, fun) a ->
    if sel `member` tts
    then (fun >>= \xs -> return $ a ++ xs)
    else return a) [] selectors
  where
    selectors = [ (MonotonicClock, getMonoClock)
                -- , (MemoryStats, readProcStatM)
                -- , (ProcessStats, readProcStats)
                -- , (IOStats, readProcIO)
                ]
\end{code}