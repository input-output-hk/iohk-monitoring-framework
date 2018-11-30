
\subsection{Cardano.BM.Counters.Common}

Common functions that serve |readCounters| on all platforms.

%if False
\begin{code}
module Cardano.BM.Counters.Common
    (
      nominalTimeToMicroseconds
    , getMonoClock
    ) where

import           Data.Time.Units (Microsecond, fromMicroseconds)

import           GHC.Clock (getMonotonicTimeNSec)
import           GHC.Word (Word64)

import           Cardano.BM.Data.Counter
\end{code}
%endif

\begin{code}
nominalTimeToMicroseconds :: Word64 -> Microsecond
nominalTimeToMicroseconds = fromMicroseconds . toInteger . (`div` 1000)
\end{code}

\begin{code}
getMonoClock :: IO [Counter]
getMonoClock = do
    t <- getMonotonicTimeNSec
    return [ MonotonicClockTime "monoclock" $ nominalTimeToMicroseconds t ]
\end{code}

