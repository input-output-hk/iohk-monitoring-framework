{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main )
  where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent (threadDelay)

import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.Aggregated (Measurable (..))
import           Cardano.BM.Data.BackendKind
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Output
import           Cardano.BM.Data.Severity
import           Cardano.BM.Setup
import           Cardano.BM.Trace

import           System.Random


-- | make configuration
config :: IO CM.Configuration
config = do
    c <- CM.empty
    CM.setMinSeverity c Debug
    CM.setSetupBackends c [KatipBK, AggregationBK, EKGViewBK]
    -- per default each messages is sent to the logs, if not otherwise defined (see below: 'CM.setBackend')
    CM.setDefaultBackends c [KatipBK]
    CM.setSetupScribes c [ ScribeDefinition {
                              scName = "stdout"
                            , scKind = StdoutSK
                            , scRotation = Nothing
                            }
                      ]
    CM.setDefaultScribes c ["StdoutSK::stdout"]
    -- forward the random number to aggregation:
    CM.setBackend c "complex.random" (Just [AggregationBK])
    -- forward the aggregated output to the EKG view:
    CM.setBackend c "complex.random.aggregated" (Just [EKGViewBK])
    -- start EKG on http://localhost:12789
    CM.setEKGport c 12789

    return c

-- | thread that outputs a random number to a |Trace|
randomThr :: Trace IO -> IO (Async.Async ())
randomThr trace = do
    trace' <- appendName "random" trace
    logInfo trace' "starting random generator"
    proc <- Async.async (loop trace')
    return proc
  where
    loop tr = do
        threadDelay 800000
        num <- randomRIO (42-42, 42+42) :: IO Double
        traceNamedObject tr (LP (LogValue "rr" (PureD num)))
        loop tr

-- | main entry point
main :: IO ()
main = do
    -- create configuration
    c <- config

    -- create initial top-level |Trace| 
    tr <- setupTrace (Right c) "complex"

    logNotice tr "starting program; hit CTRL-C to terminate"

    -- start thread sending unbounded sequence of random numbers
    -- to a trace with aggregates them into a statistics
    proc_random <- randomThr tr

    -- wait for random thread to finish, ignoring any exception
    _ <- Async.waitCatch proc_random

    return ()
