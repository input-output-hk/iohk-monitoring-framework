\begin{code}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main
  ( main )
  where

import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Monad (forever)

import           Cardano.BM.Configuration.Static (defaultConfigStdout)
import           Cardano.BM.Counters(readCounters)
import           Cardano.BM.Data.Counter
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Observable
import           Cardano.BM.Data.SubTrace (SubTrace(ObservableTraceSelf))
import           Cardano.BM.Data.Severity (Severity(Notice))
import           Cardano.BM.Setup (setupTrace_, shutdown)
import           Cardano.BM.Trace (Trace, appendName, logInfo, traceNamedObject)

\end{code}

\subsubsection{Entry procedure}
\begin{code}
main :: IO ()
main = do
    c <- defaultConfigStdout
    (tr :: Trace IO String, sb) <- setupTrace_ c "counters"

    let trace = appendName "node-metrics" tr
        counters = [MemoryStats, ProcessStats, NetStats, IOStats, GhcRtsStats, SysStats]
    thr <- Async.async $ forever $ do
        cts <- readCounters (ObservableTraceSelf counters)
        traceCounters trace cts
        logInfo trace "traced counters."
        threadDelay 5000000   -- 5 seconds

    Async.link thr
    threadDelay 30000000  -- 30 seconds
    Async.cancel thr
    shutdown sb
    return ()
  where
      traceCounters :: Trace IO String -> [Counter] -> IO ()
      traceCounters _tr [] = pure ()
      traceCounters tr (c@(Counter _ct cn cv) : cs) = do
        mle <- mkLOMeta Notice Confidential
        traceNamedObject tr (mle, LogValue (nameCounter c <> "." <> cn) cv)
        traceCounters tr cs

\end{code}
