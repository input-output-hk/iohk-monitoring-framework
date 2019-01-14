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
import           Cardano.BM.Data.Observable
import           Cardano.BM.Data.Output
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.SubTrace
import           Cardano.BM.Observer.Monadic (bracketObserveIO)
import           Cardano.BM.Setup
import           Cardano.BM.Trace

import           System.Random


-- | make configuration
config :: IO CM.Configuration
config = do
    c <- CM.empty
    CM.setMinSeverity c Debug
    CM.setSetupBackends c [KatipBK, AggregationBK, EKGViewBK]
    -- per default each messages is sent to the logs, if not otherwise defined
    -- (see below: 'CM.setBackend')
    CM.setDefaultBackends c [KatipBK]
    CM.setSetupScribes c [ ScribeDefinition {
                              scName = "stdout"
                            , scKind = StdoutSK
                            , scRotation = Nothing
                            }
                         , ScribeDefinition {
                              scName = "out.json"
                            , scKind = FileJsonSK
                            , scRotation = Nothing
                            }
                         , ScribeDefinition {
                              scName = "out.txt"
                            , scKind = FileTextSK
                            , scRotation = Nothing
                            }
                         ]
    -- per default each messages is sent to the logs, if not otherwise defined (see below: 'CM.setScribe')
    CM.setDefaultScribes c ["StdoutSK::stdout", "FileJsonSK::out.json"]
    CM.setScribes c "complex.random" (Just ["StdoutSK::stdout", "FileTextSK::out.txt"])
    CM.setScribes c "complex.random.aggregated" (Just ["StdoutSK::stdout"])
    -- define a subtrace whose behaviour is to copy all log items,
    -- and pass them up with a name added to their context
    CM.setSubTrace c "complex.random" (Just $ TeeTrace "copy")
    -- define a subtrace whose behaviour is to copy all log items,
    -- and pass them up with a name added to their context
    CM.setSubTrace c "complex.observeIO" (Just $ ObservableTrace [GhcRtsStats,MemoryStats])
    -- forward the random number to aggregation:
    CM.setBackends c "complex.random" (Just [AggregationBK, KatipBK])
    CM.setBackends c "complex.random.copy" (Just [AggregationBK])
    -- forward the observed values to aggregation:
    CM.setBackends c "complex.observeIO" (Just [KatipBK])
    -- forward the aggregated output to the EKG view:
    CM.setBackends c "complex.random.aggregated" (Just [EKGViewBK])
    CM.setBackends c "complex.random.copy.aggregated" (Just [EKGViewBK])
    CM.setBackends c "complex.observeIO.aggregated" (Just [EKGViewBK])
    -- start EKG on http://localhost:12789
    CM.setEKGport c 12789

    return c

-- | thread that outputs a random number to a |Trace|
randomThr :: Trace IO -> IO (Async.Async ())
randomThr trace = do
  logInfo trace "starting random generator"
  trace' <- subTrace "random" trace
  proc <- Async.async (loop trace')
  return proc
  where
    loop tr = do
        threadDelay 800000
        num <- randomRIO (42-42, 42+42) :: IO Double
        traceNamedObject tr (LP (LogValue "rr" (PureD num)))
        loop tr

-- | thread that outputs a random number to a |Trace|
observeIO :: Trace IO -> IO (Async.Async ())
observeIO trace = do
  logInfo trace "starting observer"
  proc <- Async.async (loop trace)
  return proc
  where
    loop tr = do
        threadDelay 1000000  -- 1 second
        bracketObserveIO trace "observeIO" $ do
            num <- randomRIO (100000, 2000000) :: IO Int
            _ <- return $ reverse $ reverse $ 42 : [1 .. num]
            threadDelay 50000  -- .05 second
            pure ()
        loop tr

-- | main entry point
main :: IO ()
main = do
    -- create configuration
    c <- config

    -- create initial top-level |Trace|
    tr <- setupTrace (Right c) "complex"

    logNotice tr "starting program; hit CTRL-C to terminate"
    logInfo tr "watch its progress on http://localhost:12789"

    -- start thread sending unbounded sequence of random numbers
    -- to a trace which aggregates them into a statistics (sent to EKG)
    proc_random <- randomThr tr

    -- start thread endlessly reversing lists of random length
    proc_obsvIO <- observeIO tr

    -- wait for observer thread to finish, ignoring any exception
    _ <- Async.waitCatch proc_obsvIO
    -- wait for random thread to finish, ignoring any exception
    _ <- Async.waitCatch proc_random

    return ()
