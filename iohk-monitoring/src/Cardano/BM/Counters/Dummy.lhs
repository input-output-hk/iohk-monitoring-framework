
\subsection{Cardano.BM.Counters.Dummy}
\label{code:Cardano.BM.Counters.Dummy}

This is a dummy definition of |readCounters| on platforms that do not support the
'proc' filesystem from which we would read the counters.

The only supported measurements are monotonic clock time and RTS statistics for now.

%if style == newcode
\begin{code}
{-# LANGUAGE CPP               #-}
module Cardano.BM.Counters.Dummy
    (
      readCounters
    ) where

#ifdef ENABLE_OBSERVABLES
import           Data.Foldable (foldrM)
import           Cardano.BM.Counters.Common (getMonoClock, readRTSStats)
import           Cardano.BM.Data.Observable
#endif
import           Cardano.BM.Data.Counter
import           Cardano.BM.Data.SubTrace
\end{code}
%endif

\label{code:Dummy.readCounters}\index{Counters!Dummy!readCounters}
\begin{code}
readCounters :: SubTrace -> IO [Counter]
readCounters NoTrace               = return []
readCounters Neutral               = return []
readCounters (TeeTrace _)          = return []
readCounters (FilterTrace _)       = return []
readCounters UntimedTrace          = return []
readCounters DropOpening           = return []
#ifdef ENABLE_OBSERVABLES
readCounters (ObservableTrace tts) = foldrM (\(sel, fun) a ->
    if any (== sel) tts
    then (fun >>= \xs -> return $ a ++ xs)
    else return a) [] selectors
  where
    selectors = [ (MonotonicClock, getMonoClock)
                , (GhcRtsStats   , readRTSStats)
                ]
#else
readCounters (ObservableTrace _)   = return []
#endif
\end{code}