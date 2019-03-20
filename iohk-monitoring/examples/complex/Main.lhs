\subsubsection{Module header and import directives}
\begin{code}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if defined(linux_HOST_OS)
#define LINUX
#endif

{- define the parallel procedures that create messages -}
#define RUN_ProcMessageOutput
#define RUN_ProcObserveIO
#define RUN_ProcObseverSTM
#define RUN_ProcObseveDownload
#define RUN_ProcRandom
#define RUN_ProcBufferDump

module Main
  ( main )
  where

import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Monad (forM_)
#ifdef ENABLE_OBSERVABLES
import           Control.Monad (forM)
import           GHC.Conc.Sync (atomically, STM, TVar, newTVar, readTVar, writeTVar)
#ifdef LINUX
import qualified Data.ByteString.Char8 as BS8
import           Network.Download (openURI)
#endif
#endif
import           Data.Text (Text, pack)
import           System.Random

#ifdef ENABLE_GUI
import qualified Cardano.BM.Configuration.Editor as CME
#endif
import           Cardano.BM.Configuration (Configuration)
import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.Aggregated (Measurable (..))
import           Cardano.BM.Data.AggregatedKind
import           Cardano.BM.Data.BackendKind
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Output
import           Cardano.BM.Data.Rotation
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.SubTrace
#ifdef ENABLE_OBSERVABLES
import           Cardano.BM.Data.Observable
import           Cardano.BM.Observer.Monadic (bracketObserveIO)
import qualified Cardano.BM.Observer.STM as STM
#endif
import           Cardano.BM.Output.Switchboard
import           Cardano.BM.Setup
import           Cardano.BM.Trace

\end{code}

\subsubsection{Define configuration}
Selected values can be viewed in EKG on \url{http://localhost:12789}.
\\
The configuration editor listens on \url{http://localhost:13789}.
\begin{code}
prepare_configuration :: IO CM.Configuration
prepare_configuration = do
    c <- CM.empty
    CM.setMinSeverity c Debug
    CM.setSetupBackends c [ KatipBK
#ifdef ENABLE_AGGREGATION
                          , AggregationBK
#endif
#ifdef ENABLE_EKG
                          , EKGViewBK
#endif
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
    -- CM.setScribes c "#aggregated.complex.random" (Just ["StdoutSK::stdout"])
    forM_ [(1::Int)..10] $ \x ->
      if odd x
      then
        CM.setScribes c ("#aggregation.complex.observeSTM." <> (pack $ show x)) $ Just [ "FileJsonSK::logs/out.odd.json" ]
      else
        CM.setScribes c ("#aggregation.complex.observeSTM." <> (pack $ show x)) $ Just [ "FileJsonSK::logs/out.even.json" ]

#ifdef LINUX
#ifdef ENABLE_OBSERVABLES
    CM.setSubTrace c "complex.observeDownload" (Just $ ObservableTrace [IOStats,NetStats])
#endif
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
#ifdef ENABLE_OBSERVABLES
    CM.setSubTrace c "complex.observeIO" (Just $ ObservableTrace [GhcRtsStats,MemoryStats])
    forM_ [(1::Int)..10] $ \x ->
      CM.setSubTrace
        c
        ("complex.observeSTM." <> (pack $ show x))
        (Just $ ObservableTrace [GhcRtsStats,MemoryStats])
#endif

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

    CM.setBackends c "#aggregation.complex.random" (Just [LogBufferBK])

#ifdef ENABLE_EKG
    CM.setBackends c "#aggregation.complex.message" (Just [EKGViewBK])
    CM.setBackends c "#aggregation.complex.observeIO" (Just [EKGViewBK])
    -- CM.setBackends c "#aggregation.complex.random" (Just [EKGViewBK])
    CM.setBackends c "#aggregation.complex.random.ewma" (Just [EKGViewBK])
    CM.setEKGport c 12789
#endif
    CM.setGUIport c 13789

    return c

\end{code}

\subsubsection{Dump the log buffer periodically}
\begin{code}
dumpBuffer :: Switchboard Text -> Trace IO Text -> IO (Async.Async ())
dumpBuffer sb trace = do
  logInfo trace "starting buffer dump"
--   trace' <- appendName "buffer" trace
--   trace' <- modifyName (const "#buffer") trace
  proc <- Async.async (loop trace)
  return proc
  where
    loop tr = do
        threadDelay 25000000  -- 25 seconds
        buf <- readLogBuffer sb
        forM_ buf $ \(logname, LogObject _ lometa locontent) -> do
            -- tr' <- appendName logname tr
            tr' <- modifyName (\n -> "#buffer." <> n <> logname) tr
            traceNamedObject tr' (lometa, locontent)
        loop tr
\end{code}

\subsubsection{Thread that outputs a random number to a |Trace|}
\begin{code}
randomThr :: Trace IO Text -> IO (Async.Async ())
randomThr trace = do
  logInfo trace "starting random generator"
  trace' <- appendName "random" trace
  proc <- Async.async (loop trace')
  return proc
  where
    loop tr = do
        threadDelay 500000  -- 0.5 second
        num <- randomRIO (42-42, 42+42) :: IO Double
        lo <- (,) <$> (mkLOMeta Debug Public) <*> pure (LogValue "rr" (PureD num))
        traceNamedObject tr lo
        loop tr

\end{code}

\subsubsection{Thread that observes an |IO| action}
\begin{code}
#ifdef ENABLE_OBSERVABLES
observeIO :: Configuration -> Trace IO Text -> IO (Async.Async ())
observeIO config trace = do
  logInfo trace "starting observer"
  proc <- Async.async (loop trace)
  return proc
  where
    loop tr = do
        threadDelay 5000000  -- 5 seconds
        _ <- bracketObserveIO config tr Debug "observeIO" $ do
            num <- randomRIO (100000, 200000) :: IO Int
            ls <- return $ reverse $ init $ reverse $ 42 : [1 .. num]
            pure $ const ls ()
        loop tr
#endif
\end{code}

\subsubsection{Threads that observe |STM| actions on the same TVar}
\begin{code}
#ifdef ENABLE_OBSERVABLES
observeSTM :: Configuration -> Trace IO Text -> IO [Async.Async ()]
observeSTM config trace = do
  logInfo trace "starting STM observer"
  tvar <- atomically $ newTVar ([1..1000]::[Int])
  -- spawn 10 threads
  proc <- forM [(1::Int)..10] $ \x -> Async.async (loop trace tvar (pack $ show x))
  return proc
  where
    loop tr tvarlist name = do
        threadDelay 10000000  -- 10 seconds
        STM.bracketObserveIO config tr Debug ("observeSTM." <> name) (stmAction tvarlist)
        loop tr tvarlist name

stmAction :: TVar [Int] -> STM ()
stmAction tvarlist = do
  list <- readTVar tvarlist
  writeTVar tvarlist $ reverse $ init $ reverse $ list
  pure ()
#endif

\end{code}

\subsubsection{Thread that observes an |IO| action which downloads a text in
order to observe the I/O statistics}
\begin{code}
#ifdef LINUX
#ifdef ENABLE_OBSERVABLES
observeDownload :: Configuration -> Trace IO Text -> IO (Async.Async ())
observeDownload config trace = do
  proc <- Async.async (loop trace)
  return proc
  where
    loop tr = do
        threadDelay 1000000  -- 1 second
        tr' <- appendName "observeDownload" tr
        bracketObserveIO config tr' Debug "" $ do
            license <- openURI "http://www.gnu.org/licenses/gpl.txt"
            case license of
              Right bs -> logNotice tr' $ pack $ BS8.unpack bs
              Left _ ->  return ()
            threadDelay 50000  -- .05 second
            pure ()
        loop tr
#endif
#endif
\end{code}

\subsubsection{Thread that periodically outputs a message}
\begin{code}
msgThr :: Trace IO Text -> IO (Async.Async ())
msgThr trace = do
  logInfo trace "start messaging .."
  trace' <- appendName "message" trace
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
    c <- prepare_configuration

#ifdef ENABLE_GUI
    -- start configuration editor
    CME.startup c
#endif

    -- create initial top-level Trace
    (tr :: Trace IO Text, sb) <- setupTrace_ c "complex"

    logNotice tr "starting program; hit CTRL-C to terminate"
-- user can watch the progress only if EKG is enabled.
#ifdef ENABLE_EKG
    logInfo tr "watch its progress on http://localhost:12789"
#endif

#ifdef RUN_ProcBufferDump
    procDump <- dumpBuffer sb tr
#endif

#ifdef RUN_ProcRandom
    {- start thread sending unbounded sequence of random numbers
       to a trace which aggregates them into a statistics (sent to EKG) -}
    procRandom <- randomThr tr
#endif
#ifdef RUN_ProcObserveIO
    -- start thread endlessly reversing lists of random length
#ifdef ENABLE_OBSERVABLES
    procObsvIO <- observeIO c tr
#endif
#endif
#ifdef RUN_ProcObseverSTM
    -- start threads endlessly observing STM actions operating on the same TVar
#ifdef ENABLE_OBSERVABLES
    procObsvSTMs <- observeSTM c tr
#endif
#endif
#ifdef LINUX
#ifdef RUN_ProcObseveDownload
    -- start thread endlessly which downloads sth in order to check the I/O usage
#ifdef ENABLE_OBSERVABLES
    procObsvDownload <- observeDownload c tr
#endif
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
#ifdef ENABLE_OBSERVABLES
    _ <- Async.waitCatch procObsvDownload
#endif
#endif
#endif
#ifdef RUN_ProcObseverSTM
    -- wait for observer thread to finish, ignoring any exception
#ifdef ENABLE_OBSERVABLES
    _ <- forM procObsvSTMs Async.waitCatch
#endif
#endif
#ifdef RUN_ProcObserveIO
    -- wait for observer thread to finish, ignoring any exception
#ifdef ENABLE_OBSERVABLES
    _ <- Async.waitCatch procObsvIO
#endif
#endif
#ifdef RUN_ProcRandom
    -- wait for random thread to finish, ignoring any exception
    _ <- Async.waitCatch procRandom
#endif
#ifdef RUN_ProcBufferDump
    _ <- Async.waitCatch procDump
#endif

    return ()

\end{code}
