
\subsection{Cardano.BM.Counters.Common}

Common functions that serve |readCounters| on all platforms.

%if style == newcode
\begin{code}
module Cardano.BM.Counters.Common
    (
      nominalTimeToMicroseconds
    , getMonoClock
    , readRTSStats
    ) where

import           Data.Text (Text)
import           Data.Time.Units (Microsecond, fromMicroseconds)

import           GHC.Clock (getMonotonicTimeNSec)
import           GHC.Word (Word64)
import qualified GHC.Stats as GhcStats

import           Cardano.BM.Data.Counter
\end{code}
%endif

\begin{code}
nominalTimeToMicroseconds :: Word64 -> Microsecond
nominalTimeToMicroseconds = fromMicroseconds . toInteger . (`div` 1000)
\end{code}

\subsubsection{Read monotonic clock}\label{code:getMonoClock}
\begin{code}
getMonoClock :: IO [Counter]
getMonoClock = do
    t <- getMonotonicTimeNSec
    return [ Counter MonotonicClockTime "monoclock" $ toInteger $ nominalTimeToMicroseconds t ]

\end{code}

\subsubsection{Read GHC RTS statistics}\label{code:readRTSStats}
Read counters from GHC's |RTS| (runtime system). The values returned are as per the last 
GC (garbage collection) run.
\begin{code}
readRTSStats :: IO [Counter]
readRTSStats = do
    iscollected <- GhcStats.getRTSStatsEnabled
    if iscollected
        then ghcstats
        else return []
  where
    ghcstats :: IO [Counter]
    ghcstats = do
        -- need to run GC?
        rts <- GhcStats.getRTSStats
        let getrts = ghcval rts
        return [ getrts (toInteger . GhcStats.allocated_bytes, "bytesAllocated")
               , getrts (toInteger . GhcStats.max_live_bytes, "liveBytes")
               , getrts (toInteger . GhcStats.max_large_objects_bytes, "largeBytes")
               , getrts (toInteger . GhcStats.max_compact_bytes, "compactBytes")
               , getrts (toInteger . GhcStats.max_slop_bytes, "slopBytes")
               , getrts (toInteger . GhcStats.max_mem_in_use_bytes, "usedMemBytes")
               , getrts (toInteger . GhcStats.gc_cpu_ns, "gcCpuNs")
               , getrts (toInteger . GhcStats.gc_elapsed_ns, "gcElapsedNs")
               , getrts (toInteger . GhcStats.cpu_ns, "cpuNs")
               , getrts (toInteger . GhcStats.elapsed_ns, "elapsedNs")
               , getrts (toInteger . GhcStats.gcs, "gcNum")
               , getrts (toInteger . GhcStats.major_gcs, "gcMajorNum")
               ]
    ghcval :: GhcStats.RTSStats -> ((GhcStats.RTSStats -> Integer), Text) -> Counter
    ghcval s (f, n) = Counter RTSStats n (f s)
\end{code}
