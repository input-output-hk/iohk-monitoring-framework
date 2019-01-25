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
import           System.Random

import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.Aggregated (Measurable (..))
import           Cardano.BM.Data.AggregatedKind
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
    CM.setDefaultBackends c [KatipBK]
    CM.setSetupScribes c [ ScribeDefinition {
                              scName = "stdout"
                            , scKind = StdoutSK
                            , scRotation = Nothing
                            }
                         , ScribeDefinition {
                              scName = "out.odd.json"
                            , scKind = FileJsonSK
                            , scRotation = Nothing
                            }
                         , ScribeDefinition {
                              scName = "out.even.json"
                            , scKind = FileJsonSK
                            , scRotation = Nothing
                            }
                         , ScribeDefinition {
                              scName = "out.txt"
                            , scKind = FileTextSK
                            , scRotation = Nothing
                            }
                         ]
    CM.setDefaultScribes c ["StdoutSK::stdout"]
    CM.setScribes c "complex.random" (Just ["StdoutSK::stdout", "FileTextSK::out.txt"])
    CM.setScribes c "#aggregated.complex.random" (Just ["StdoutSK::stdout"])
    forM_ [(1::Int)..10] $ \x ->
      if odd x
      then
        CM.setScribes c ("#aggregation.complex.observeSTM." <> (pack $ show x)) $ Just [ "FileJsonSK::out.odd.json" ]
      else
        CM.setScribes c ("#aggregation.complex.observeSTM." <> (pack $ show x)) $ Just [ "FileJsonSK::out.even.json" ]

    CM.setSubTrace c "complex.random" (Just $ TeeTrace "ewma")
    CM.setSubTrace c "#ekgview"
      (Just $ FilterTrace [ (Drop (StartsWith "#ekgview.#aggregation.complex.random"),
                             Unhide [(EndsWith ".count"),
                                     (EndsWith ".avg"),
                                     (EndsWith ".mean")]),
                            (Drop (StartsWith "#ekgview.#aggregation.complex.observeIO"),
                             Unhide [(Contains "close.RTS.liveBytes.last"),
                                     (Contains "close.RTS")])
                          ])
    CM.setSubTrace c "complex.observeIO" (Just $ ObservableTrace [GhcRtsStats,MemoryStats])
    forM_ [(1::Int)..10] $ \x ->
      CM.setSubTrace
        c
        ("complex.observeSTM." <> (pack $ show x))
        (Just $ ObservableTrace [GhcRtsStats,MemoryStats])

    CM.setBackends c "complex.random" (Just [AggregationBK, KatipBK])
    CM.setBackends c "complex.random.ewma" (Just [AggregationBK])
    CM.setBackends c "complex.observeIO" (Just [AggregationBK])
    forM_ [(1::Int)..10] $ \x ->
      CM.setBackends
        c
        ("complex.observeSTM." <> (pack $ show x))
        (Just [AggregationBK])

    CM.setAggregatedKind c "complex.random.rr" (Just StatsAK)
    CM.setAggregatedKind c "complex.random.ewma.rr" (Just (EwmaAK 0.42))

    CM.setBackends c "#aggregation.complex.observeIO" (Just [EKGViewBK])
    CM.setBackends c "#aggregation.complex.random" (Just [EKGViewBK])
    CM.setBackends c "#aggregation.complex.random.ewma" (Just [EKGViewBK])
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
        threadDelay 500000  -- 0.5 second
        num <- randomRIO (42-42, 42+42) :: IO Double
        lo <- LogObject <$> mkLOMeta <*> pure (LogValue "rr" (PureD num))
        traceNamedObject tr lo
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
        threadDelay 5000000  -- 5 seconds
        _ <- bracketObserveIO tr "observeIO" $ do
            num <- randomRIO (100000, 200000) :: IO Int
            ls <- return $ reverse $ init $ reverse $ 42 : [1 .. num]
            pure $ const ls ()
        loop tr

\end{code}

\subsubsection{Thread that observes an |IO| action which downloads a txt in
order to observe the I/O statistics}
\todo[inline]{disabled for now! on Mac OSX this function was blocking all IO.}
\begin{spec}
observeDownload :: Trace IO -> IO ()
observeDownload trace = loop trace
  where
    loop tr = do
        threadDelay 10000000  -- 10 seconds
        tr' <- appendName "observeDownload" tr
        bracketObserveIO tr' "" $ do
            license <- openURI "http://www.gnu.org/licenses/gpl.txt"
            case license of
              Right bs -> logNotice tr' $ pack $ take 100 $ BS8.unpack bs
              Left _ ->  return ()
            threadDelay 500000  -- .5 second
            pure ()
        loop tr

\end{spec}

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
        threadDelay 10000000  -- 10 seconds
        STM.bracketObserveIO tr ("observeSTM." <> name) (stmAction tvarlist)
        loop tr tvarlist name

stmAction :: TVar [Int] -> STM ()
stmAction tvarlist = do
  list <- readTVar tvarlist
  writeTVar tvarlist $ reverse $ init $ reverse $ list
  pure ()

\end{code}

\subsubsection{Main entry point}
\begin{code}
main :: IO ()
main = do
    -- create configuration
    c <- config

    -- create initial top-level Trace
    tr <- setupTrace (Right c) "complex"

    logNotice tr "starting program; hit CTRL-C to terminate"
    logInfo tr "watch its progress on http://localhost:12789"

    {- start thread sending unbounded sequence of random numbers
       to a trace which aggregates them into a statistics (sent to EKG) -}
    procRandom <- randomThr tr

    -- start thread endlessly reversing lists of random length
    procObsvIO <- observeIO tr

    -- start threads endlessly observing STM actions operating on the same TVar
    procObsvSTMs <- observeSTM tr


    -- wait for observer thread to finish, ignoring any exception
    _ <- forM procObsvSTMs Async.waitCatch
    -- wait for observer thread to finish, ignoring any exception
    _ <- Async.waitCatch procObsvIO
    -- wait for random thread to finish, ignoring any exception
    _ <- Async.waitCatch procRandom

    return ()

\end{code}
