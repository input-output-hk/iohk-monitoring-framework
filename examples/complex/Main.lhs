\subsubsection{Module header and import directives}
\begin{code}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

#if defined(linux_HOST_OS)
#define LINUX
#endif

{- define the parallel procedures that create messages -}
#define RUN_ProcMessageOutput
#define RUN_ProcObserveIO
#define RUN_ProcObseverSTM
#define RUN_ProcObseveDownload
#define RUN_ProcRandom

module Main
  ( main )
  where

import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Monad (forM, forM_)
import           GHC.Conc.Sync (STM, TVar, atomically, newTVar, readTVar, writeTVar)
import           Data.Text (pack)
#ifdef LINUX
import qualified Data.ByteString.Char8 as BS8
import           Network.Download (openURI)
#endif
import           System.Random

import qualified Cardano.BM.Configuration.Editor as CME
import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.Aggregated (Measurable (..))
import           Cardano.BM.Data.AggregatedKind
import           Cardano.BM.Data.BackendKind
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Observable
import           Cardano.BM.Data.Output
import           Cardano.BM.Data.Rotation
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.SubTrace
import           Cardano.BM.Observer.Monadic (bracketObserveIO)
import qualified Cardano.BM.Observer.STM as STM
import           Cardano.BM.Setup
import           Cardano.BM.Trace

\end{code}

\subsubsection{Define configuration}
Selected values can be viewed in EKG on \url{http://localhost:12789}.
\begin{code}
config :: IO CM.Configuration
config = do
    c <- CM.empty
    CM.setMinSeverity c Debug
    CM.setSetupBackends c [ KatipBK
#ifdef ENABLE_AGGREGATION
                          , AggregationBK
#endif
                          , EKGViewBK
                          ]
    CM.setDefaultBackends c [KatipBK]
    CM.setSetupScribes c [ ScribeDefinition {
                              scName = "stdout"
                            , scKind = StdoutSK
                            , scPrivacy = ScPublic
                            , scRotation = Nothing
                            }
                         , ScribeDefinition {
                              scName = "logs/out.odd.json"
                            , scKind = FileJsonSK
                            , scPrivacy = ScPublic
                            , scRotation = Nothing
                            }
                         , ScribeDefinition {
                              scName = "logs/out.even.json"
                            , scKind = FileJsonSK
                            , scPrivacy = ScPublic
                            , scRotation = Nothing
                            }
                         , ScribeDefinition {
                              scName = "logs/downloading.json"
                            , scKind = FileJsonSK
                            , scPrivacy = ScPublic
                            , scRotation = Nothing
                            }
                         , ScribeDefinition {
                              scName = "logs/out.txt"
                            , scKind = FileTextSK
                            , scPrivacy = ScPublic
                            , scRotation = Just $ RotationParameters
                                              { rpLogLimitBytes = 5000 -- 5kB
                                              , rpMaxAgeHours   = 24
                                              , rpKeepFilesNum  = 3
                                              }
                            }
                         ]
    CM.setDefaultScribes c ["StdoutSK::stdout"]
    CM.setScribes c "complex.random" (Just ["StdoutSK::stdout", "FileTextSK::logs/out.txt"])
    CM.setScribes c "#aggregated.complex.random" (Just ["StdoutSK::stdout"])
    forM_ [(1::Int)..10] $ \x ->
      if odd x
      then
        CM.setScribes c ("#aggregation.complex.observeSTM." <> (pack $ show x)) $ Just [ "FileJsonSK::logs/out.odd.json" ]
      else
        CM.setScribes c ("#aggregation.complex.observeSTM." <> (pack $ show x)) $ Just [ "FileJsonSK::logs/out.even.json" ]

#ifdef LINUX
    CM.setSubTrace c "complex.observeDownload" (Just $ ObservableTrace [IOStats,NetStats])
    CM.setBackends c "complex.observeDownload" (Just [KatipBK])
    CM.setScribes c "complex.observeDownload" (Just ["StdoutSK::stdout", "FileJsonSK::logs/downloading.json"])
#endif
    CM.setSubTrace c "complex.random" (Just $ TeeTrace "ewma")
    CM.setSubTrace c "#ekgview"
      (Just $ FilterTrace [ (Drop (StartsWith "#ekgview.#aggregation.complex.random"),
                             Unhide [(EndsWith ".count"),
                                     (EndsWith ".avg"),
                                     (EndsWith ".mean")]),
                            (Drop (StartsWith "#ekgview.#aggregation.complex.observeIO"),
                             Unhide [(Contains "diff.RTS.cpuNs.timed.")]),
                            (Drop (StartsWith "#ekgview.#aggregation.complex.observeSTM"),
                             Unhide [(Contains "diff.RTS.gcNum.timed.")]),
                            (Drop (StartsWith "#ekgview.#aggregation.complex.message"),
                             Unhide [(Contains ".timed.m")])
                          ])
    CM.setSubTrace c "complex.observeIO" (Just $ ObservableTrace [GhcRtsStats,MemoryStats])
    forM_ [(1::Int)..10] $ \x ->
      CM.setSubTrace
        c
        ("complex.observeSTM." <> (pack $ show x))
        (Just $ ObservableTrace [GhcRtsStats,MemoryStats])

#ifdef ENABLE_AGGREGATION
    CM.setBackends c "complex.message" (Just [AggregationBK, KatipBK])
    CM.setBackends c "complex.random" (Just [AggregationBK, KatipBK])
    CM.setBackends c "complex.random.ewma" (Just [AggregationBK])
    CM.setBackends c "complex.observeIO" (Just [AggregationBK])
#endif
    forM_ [(1::Int)..10] $ \x -> do
#ifdef ENABLE_AGGREGATION
      CM.setBackends c
        ("complex.observeSTM." <> (pack $ show x))
        (Just [AggregationBK])
#endif
      CM.setBackends c
        ("#aggregation.complex.observeSTM." <> (pack $ show x))
        (Just [KatipBK])

    CM.setAggregatedKind c "complex.random.rr" (Just StatsAK)
    CM.setAggregatedKind c "complex.random.ewma.rr" (Just (EwmaAK 0.42))

    CM.setBackends c "#aggregation.complex.message" (Just [EKGViewBK])
    CM.setBackends c "#aggregation.complex.observeIO" (Just [EKGViewBK])
    CM.setBackends c "#aggregation.complex.random" (Just [EKGViewBK])
    CM.setBackends c "#aggregation.complex.random.ewma" (Just [EKGViewBK])
    CM.setEKGport c 12789
    CM.setGUIport c 13789

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
        lo <- LogObject <$> (mkLOMeta Debug) <*> pure (LogValue "rr" (PureD num))
        traceConditionally tr lo
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
        _ <- bracketObserveIO tr Debug "observeIO" $ do
            num <- randomRIO (100000, 200000) :: IO Int
            ls <- return $ reverse $ init $ reverse $ 42 : [1 .. num]
            pure $ const ls ()
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
        threadDelay 10000000  -- 10 seconds
        STM.bracketObserveIO tr Debug ("observeSTM." <> name) (stmAction tvarlist)
        loop tr tvarlist name

stmAction :: TVar [Int] -> STM ()
stmAction tvarlist = do
  list <- readTVar tvarlist
  writeTVar tvarlist $ reverse $ init $ reverse $ list
  pure ()

\end{code}

\subsubsection{Thread that observes an |IO| action which downloads a text in
order to observe the I/O statistics}
\begin{code}
#ifdef LINUX
observeDownload :: Trace IO -> IO (Async.Async ())
observeDownload trace = do
  proc <- Async.async (loop trace)
  return proc
  where
    loop tr = do
        threadDelay 1000000  -- 1 second
        tr' <- appendName "observeDownload" tr
        bracketObserveIO tr' Debug "" $ do
            license <- openURI "http://www.gnu.org/licenses/gpl.txt"
            case license of
              Right bs -> logNotice tr' $ pack $ BS8.unpack bs
              Left _ ->  return ()
            threadDelay 50000  -- .05 second
            pure ()
        loop tr
#endif
\end{code}

\subsubsection{Thread that periodically outputs a message}
\begin{code}
msgThr :: Trace IO -> IO (Async.Async ())
msgThr trace = do
  logInfo trace "start messaging .."
  trace' <- subTrace "message" trace
  Async.async (loop trace')
  where
    loop tr = do
        threadDelay 3000000  -- 3 seconds
        logNotice tr "N O T I F I C A T I O N ! ! !"
        logDebug tr "a detailed debug message."
        logError tr "Boooommm .."
        loop tr

\end{code}

\subsubsection{Main entry point}
\begin{code}
main :: IO ()
main = do
    -- create configuration
    c <- config

    -- start configuration editor
    CME.startup c

    -- create initial top-level Trace
    tr <- setupTrace (Right c) "complex"

    logNotice tr "starting program; hit CTRL-C to terminate"
    logInfo tr "watch its progress on http://localhost:12789"

#ifdef RUN_ProcRandom
    {- start thread sending unbounded sequence of random numbers
       to a trace which aggregates them into a statistics (sent to EKG) -}
    procRandom <- randomThr tr
#endif
#ifdef RUN_ProcObserveIO
    -- start thread endlessly reversing lists of random length
    procObsvIO <- observeIO tr
#endif
#ifdef RUN_ProcObseverSTM 
    -- start threads endlessly observing STM actions operating on the same TVar
    procObsvSTMs <- observeSTM tr
#endif
#ifdef LINUX
#ifdef RUN_ProcObseveDownload 
    -- start thread endlessly which downloads sth in order to check the I/O usage
    procObsvDownload <- observeDownload tr
#endif
#endif

#ifdef RUN_ProcMessageOutput
    -- start a thread to output a text messages every n seconds
    procMsg <- msgThr tr
    -- wait for message thread to finish, ignoring any exception
    _ <- Async.waitCatch procMsg
#endif

#ifdef LINUX
#ifdef RUN_ProcObseveDownload 
    -- wait for download thread to finish, ignoring any exception
    _ <- Async.waitCatch procObsvDownload
#endif
#endif
#ifdef RUN_ProcObseverSTM 
    -- wait for observer thread to finish, ignoring any exception
    _ <- forM procObsvSTMs Async.waitCatch
#endif
#ifdef RUN_ProcObserveIO
    -- wait for observer thread to finish, ignoring any exception
    _ <- Async.waitCatch procObsvIO
#endif
#ifdef RUN_ProcRandom
    -- wait for random thread to finish, ignoring any exception
    _ <- Async.waitCatch procRandom
#endif
    return ()

\end{code}
