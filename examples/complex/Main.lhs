\subsubsection{Module header and import directives}
\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main )
  where

import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Monad (forM, forM_)
import           GHC.Conc.Sync (STM, TVar, atomically, newTVar, readTVar, writeTVar)
import           Data.Text (pack)
import           Network.Download (openURI)
import           System.Random
import qualified Data.ByteString.Char8 as BS8

import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.Aggregated (Measurable (..))
import           Cardano.BM.Data.BackendKind
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Observable
import           Cardano.BM.Data.Output
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.SubTrace
import           Cardano.BM.Observer.Monadic (bracketObserveIO)
import qualified Cardano.BM.Observer.STM as STM
import           Cardano.BM.Setup
import           Cardano.BM.Trace

\end{code}

\subsubsection{Define configuration}
The output can be viewed in EKG on \url{http://localhost:12789}.
\begin{code}
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
    CM.setScribes c "complex.observeDownload" (Just ["FileTextSK::out.txt"])
    -- define a subtrace whose behaviour is to copy all log items,
    -- and pass them up with a name added to their context
    CM.setSubTrace c "complex.random" (Just $ TeeTrace "copy")
    -- define a subtrace whose behaviour is to observe statistics,
    -- from ghc (RTS) and memory
    CM.setSubTrace c "complex.observeIO" (Just $ ObservableTrace [GhcRtsStats,MemoryStats])
    forM_ [(1::Int)..10] $ \x ->
      CM.setSubTrace
        c
        ("complex.observeSTM." <> (pack $ show x))
        (Just $ ObservableTrace [GhcRtsStats,MemoryStats])
    -- define a subtrace whose behaviour is to observe statistics,
    -- from ghc (RTS), memory and I/O
    CM.setSubTrace c "complex.observeDownload" (Just $ ObservableTrace [IOStats])
    -- forward the random number to aggregation:
    CM.setBackends c "complex.random" (Just [AggregationBK, KatipBK])
    CM.setBackends c "complex.random.copy" (Just [AggregationBK])
    forM_ [(1::Int)..10] $ \x ->
      CM.setBackends
        c
        ("complex.observeSTM." <> (pack $ show x))
        (Just [AggregationBK])
    CM.setBackends c "complex.observeDownload" (Just [AggregationBK, KatipBK])
    -- forward the observed values to aggregation:
    CM.setBackends c "complex.observeIO" (Just [KatipBK])
    -- forward the aggregated output to the EKG view:
    CM.setBackends c "complex.random.aggregated" (Just [EKGViewBK])
    CM.setBackends c "complex.random.copy.aggregated" (Just [EKGViewBK])
    CM.setBackends c "complex.observeIO.aggregated" (Just [EKGViewBK])
    forM_ [(1::Int)..10] $ \x ->
      CM.setBackends
        c
        ("complex.observeSTM." <> (pack $ show x) <> ".aggregated")
        (Just [EKGViewBK])
    CM.setBackends c "complex.observeDownload.aggregated" (Just [EKGViewBK])
    -- start EKG on http://localhost:12789
    CM.setEKGport c 12789

    return c

\end{code}

\subsubsection{Thread that outputs a random number to a |Trace|}
\begin{code}
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
        traceNamedObject tr (LogValue "rr" (PureD num))
        loop tr

\end{code}

\subsubsection{Thread that observes an |IO| action}
\begin{code}
observeIO :: Trace IO -> IO (Async.Async ())
observeIO trace = do
  logInfo trace "starting observer"
  proc <- Async.async (loop trace)
  return proc
  where
    loop tr = do
        threadDelay 1000000  -- 1 second
        bracketObserveIO tr "observeIO" $ do
            num <- randomRIO (100000, 2000000) :: IO Int
            _ <- return $ reverse $ reverse $ 42 : [1 .. num]
            threadDelay 50000  -- .05 second
            pure ()
        loop tr

\end{code}

\subsubsection{Thread that observes an |IO| action which downloads a txt in
order to observe the I/O statistics}
\begin{code}
observeDownload :: Trace IO -> IO (Async.Async ())
observeDownload trace = do
  proc <- Async.async (loop trace)
  return proc
  where
    loop tr = do
        threadDelay 1000000  -- 1 second
        tr' <- appendName "observeDownload" tr
        bracketObserveIO tr' "" $ do
            license <- openURI "http://www.gnu.org/licenses/gpl.txt"
            case license of
              Right bs -> logNotice tr' $ pack $ BS8.unpack bs
              Left _ ->  return ()
            threadDelay 50000  -- .05 second
            pure ()
        loop tr

\end{code}

\subsubsection{Threads that observe |STM| actions on the same TVar}
\begin{code}
observeSTM :: Trace IO -> IO [Async.Async ()]
observeSTM trace = do
  logInfo trace "starting STM observer"
  tvar <- atomically $ newTVar ([1..1000]::[Int])
  -- spawn 10 threads
  proc <- forM [(1::Int)..10] $ \x -> Async.async (loop trace tvar (pack $ show x))
  return proc
  where
    loop tr tvarlist name = do
        threadDelay 1000000  -- 1 second
        STM.bracketObserveIO tr ("observeSTM." <> name) (stmAction tvarlist)
        loop tr tvarlist name

stmAction :: TVar [Int] -> STM ()
stmAction tvarlist = do
  list <- readTVar tvarlist
  writeTVar tvarlist $ reverse $ reverse $ list
  pure ()

\end{code}

\subsubsection{Main entry point}
\begin{code}
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

    -- start threads endlessly observing STM actions operating on the same TVar
    proc_obsvSTMs <- observeSTM tr

    -- start thread endlessly which downloads sth in order to check the I/O usage
    proc_obsvDownload <- observeDownload tr

    -- wait for observer thread to finish, ignoring any exception
    _ <- Async.waitCatch proc_obsvIO
    -- wait for observer thread to finish, ignoring any exception
    _ <- forM proc_obsvSTMs Async.waitCatch
    -- wait for random thread to finish, ignoring any exception
    _ <- Async.waitCatch proc_random
    -- wait for thread which download to finish, ignoring any exception
    _ <- Async.waitCatch proc_obsvDownload

    return ()

\end{code}