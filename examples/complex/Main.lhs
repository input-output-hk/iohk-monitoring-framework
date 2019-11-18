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
#define RUN_ProcMonitoring
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
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text, pack)
import           System.Random

import           Cardano.BM.Backend.Aggregation
import           Cardano.BM.Backend.Editor
import           Cardano.BM.Backend.EKGView
import           Cardano.BM.Backend.Monitoring
import           Cardano.BM.Backend.Switchboard (Switchboard, readLogBuffer)
import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.Aggregated (Measurable (..))
import           Cardano.BM.Data.AggregatedKind
import           Cardano.BM.Data.BackendKind
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.MonitoringEval
import           Cardano.BM.Data.Output
import           Cardano.BM.Data.Rotation
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.SubTrace
#ifdef ENABLE_OBSERVABLES
import           Cardano.BM.Data.Observable
import           Cardano.BM.Observer.Monadic (bracketObserveIO)
import qualified Cardano.BM.Observer.STM as STM
#endif
import           Cardano.BM.Plugin
import           Cardano.BM.Setup
import           Cardano.BM.Trace

\end{code}

\subsubsection{Define configuration}
Selected values can be viewed in EKG on \url{http://localhost:12790}.
And, the \emph{Prometheus} interface is accessible at \url{http://localhost:12800/metrics}
\\
The configuration editor listens on \url{http://localhost:13790}.
\begin{code}
prepare_configuration :: IO CM.Configuration
prepare_configuration = do
    c <- CM.empty
    CM.setMinSeverity c Info
    CM.setSetupBackends c [ KatipBK
                          , AggregationBK
                          , MonitoringBK
                          -- , TraceForwarderBK -- testing for pipe
                          ]
    CM.setDefaultBackends c [KatipBK]
    CM.setSetupScribes c [ ScribeDefinition {
                              scName = "stdout"
                            , scKind = StdoutSK
                            , scFormat = ScText
                            , scPrivacy = ScPublic
                            , scRotation = Nothing
                            }
                         , ScribeDefinition {
                              scName = "logs/out.odd.json"
                            , scKind = FileSK
                            , scFormat = ScJson
                            , scPrivacy = ScPublic
                            , scRotation = Nothing
                            }
                         , ScribeDefinition {
                              scName = "logs/out.even.json"
                            , scKind = FileSK
                            , scFormat = ScJson
                            , scPrivacy = ScPublic
                            , scRotation = Nothing
                            }
                         , ScribeDefinition {
                              scName = "logs/downloading.json"
                            , scKind = FileSK
                            , scFormat = ScJson
                            , scPrivacy = ScPublic
                            , scRotation = Nothing
                            }
                         , ScribeDefinition {
                              scName = "logs/out.txt"
                            , scKind = FileSK
                            , scFormat = ScText
                            , scPrivacy = ScPublic
                            , scRotation = Just $ RotationParameters
                                              { rpLogLimitBytes = 5000 -- 5kB
                                              , rpMaxAgeHours   = 24
                                              , rpKeepFilesNum  = 3
                                              }
                            }
                         ]

    CM.setDefaultScribes c ["StdoutSK::stdout"]
    CM.setScribes c "complex.random" (Just ["StdoutSK::stdout", "FileSK::logs/out.txt"])
    forM_ [(1::Int)..10] $ \x ->
      if odd x
      then
        CM.setScribes c ("complex.#aggregation.complex.observeSTM." <> (pack $ show x)) $ Just [ "FileSK::logs/out.odd.json" ]
      else
        CM.setScribes c ("complex.#aggregation.complex.observeSTM." <> (pack $ show x)) $ Just [ "FileSK::logs/out.even.json" ]

#ifdef LINUX
#ifdef ENABLE_OBSERVABLES
    CM.setSubTrace c "complex.observeDownload" (Just $ ObservableTraceSelf [IOStats,NetStats])
#endif
    CM.setBackends c "complex.observeDownload" (Just [KatipBK])
    CM.setScribes c "complex.observeDownload" (Just ["FileSK::logs/downloading.json"])
#endif
    CM.setSubTrace c "#messagecounters.switchboard" $ Just NoTrace
    CM.setSubTrace c "#messagecounters.katip"       $ Just NoTrace
    CM.setSubTrace c "#messagecounters.aggregation" $ Just NoTrace
    CM.setSubTrace c "#messagecounters.ekgview"     $ Just NoTrace
    CM.setSubTrace c "#messagecounters.monitoring"  $ Just NoTrace

    CM.setSubTrace c "complex.random" (Just $ TeeTrace "ewma")
    CM.setSubTrace c "#ekgview"
      (Just $ FilterTrace [ (Drop (StartsWith "#ekgview.complex.#aggregation.complex.random"),
                             Unhide [(EndsWith ".count"),
                                     (EndsWith ".avg"),
                                     (EndsWith ".mean")]),
                            (Drop (StartsWith "#ekgview.complex.#aggregation.complex.observeIO"),
                             Unhide [(Contains "diff.RTS.cpuNs.timed.")]),
                            (Drop (StartsWith "#ekgview.complex.#aggregation.complex.observeSTM"),
                             Unhide [(Contains "diff.RTS.gcNum.timed.")]),
                            (Drop (StartsWith "#ekgview.complex.#aggregation.complex.message"),
                             Unhide [(Contains ".timed.m")])
                          ])
#ifdef ENABLE_OBSERVABLES
    CM.setSubTrace c "complex.observeIO" (Just $ ObservableTraceSelf [GhcRtsStats,MemoryStats])
    forM_ [(1::Int)..10] $ \x ->
      CM.setSubTrace
        c
        ("complex.observeSTM." <> (pack $ show x))
        (Just $ ObservableTraceSelf [GhcRtsStats,MemoryStats])
#endif

    CM.setBackends c "complex.message" (Just [AggregationBK, KatipBK, TraceForwarderBK])
    CM.setBackends c "complex.random" (Just [KatipBK, EKGViewBK])
    CM.setBackends c "complex.random.ewma" (Just [AggregationBK])
    CM.setBackends c "complex.observeIO" (Just [AggregationBK, MonitoringBK])

    forM_ [(1::Int)..10] $ \x -> do
      CM.setBackends c
        ("complex.observeSTM." <> (pack $ show x))
        (Just [AggregationBK])
      CM.setBackends c
        ("complex.#aggregation.complex.observeSTM." <> (pack $ show x))
        (Just [KatipBK])

    CM.setAggregatedKind c "complex.random.rr" (Just StatsAK)
    CM.setAggregatedKind c "complex.random.ewma.rr" (Just (EwmaAK 0.22))

    CM.setBackends c "complex.#aggregation.complex.random" (Just [EditorBK])
    CM.setBackends c "complex.#aggregation.complex.random.ewma" (Just [EKGViewBK, EditorBK])
    CM.setBackends c "complex.#aggregation.complex.message" (Just [EKGViewBK, MonitoringBK])
    CM.setBackends c "complex.#aggregation.complex.monitoring" (Just [MonitoringBK])
    CM.setBackends c "complex.#aggregation.complex.observeIO" (Just [EKGViewBK])
    CM.setEKGport c 12790

    -- if the TraceForwarderBK backend is started
    -- then it will try to open this pipe to forward messages to another process
    CM.setLogOutput c "logs/log-pipe"

    CM.setPrometheusPort c 12800
    CM.setGUIport c 13790
    CM.setMonitors c $ HM.fromList
        [ ( "complex.monitoring"
          , ( Just (Compare "monitMe" (GE, (OpMeasurable 10)))
            , Compare "monitMe" (GE, (OpMeasurable 42))
            , [CreateMessage Warning "MonitMe is greater than 42!"]
            )
          )
        , ( "complex.#aggregation.complex.monitoring"
          , ( Just (Compare "monitMe.fcount" (GE, (OpMeasurable 8)))
            , Compare "monitMe.mean" (GE, (OpMeasurable 41))
            , [CreateMessage Warning "MonitMe.mean is greater than 41!"]
            )
          )
        , ( "complex.observeIO.close"
          , ( Nothing
            , Compare "complex.observeIO.close.Mem.size" (GE, (OpMeasurable 25))
            , [CreateMessage Warning "closing mem size is greater than 25!"]
            )
          )
        ]
    CM.setBackends c "complex.monitoring" (Just [AggregationBK, KatipBK, MonitoringBK])
    return c

\end{code}

\subsubsection{Dump the log buffer periodically}
\begin{code}
dumpBuffer :: Switchboard Text -> Trace IO Text -> IO (Async.Async ())
dumpBuffer sb trace = do
  logInfo trace "starting buffer dump"
  proc <- Async.async (loop trace)
  return proc
  where
    loop tr = do
        threadDelay 25000000  -- 25 seconds
        buf <- readLogBuffer sb
        forM_ buf $ \(logname, LogObject _ lometa locontent) -> do
            let tr' = modifyName (\n -> ["#buffer"] <> n <> [logname]) tr
            traceNamedObject tr' (lometa, locontent)
        loop tr
\end{code}

\subsubsection{Thread that outputs a random number to a |Trace|}
\begin{code}
randomThr :: Trace IO Text -> IO (Async.Async ())
randomThr trace = do
  logInfo trace "starting random generator"
  let trace' = appendName "random" trace
  proc <- Async.async (loop trace')
  return proc
  where
    loop tr = do
        threadDelay 500000  -- 0.5 second
        num <- randomRIO (42-42, 42+42) :: IO Double
        lo <- (,) <$> (mkLOMeta Info Public) <*> pure (LogValue "rr" (PureD num))
        traceNamedObject tr lo
        loop tr

\end{code}

\subsubsection{Thread that outputs a random number to monitoring |Trace|}
\begin{code}
#ifdef RUN_ProcMonitoring
monitoringThr :: Trace IO Text -> IO (Async.Async ())
monitoringThr trace = do
  logInfo trace "starting numbers for monitoring..."
  let trace' = appendName "monitoring" trace
  proc <- Async.async (loop trace')
  return proc
  where
    loop tr = do
        threadDelay 500000  -- 0.5 second
        num <- randomRIO (42-42, 42+42) :: IO Double
        lo <- (,) <$> (mkLOMeta Warning Public) <*> pure (LogValue "monitMe" (PureD num))
        traceNamedObject tr lo
        loop tr
#endif
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
        let tr' = appendName "observeIO" tr
        _ <- bracketObserveIO config tr' Warning "complex.observeIO" $ do
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
        STM.bracketObserveIO config tr Warning ("observeSTM." <> name) (stmAction tvarlist)
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
        let tr' = appendName "observeDownload" tr
        bracketObserveIO config tr' Warning "complex.observeDownload" $ do
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
#ifdef RUN_ProcMessageOutput
msgThr :: Trace IO Text -> IO (Async.Async ())
msgThr trace = do
  logInfo trace "start messaging .."
  let trace' = appendName "message" trace
  Async.async (loop trace')
  where
    loop tr = do
        threadDelay 3000000  -- 3 seconds
        logNotice tr "N O T I F I C A T I O N ! ! !"
        logDebug tr "a detailed debug message."
        logError tr "Boooommm .."
        loop tr
#endif

\end{code}

\subsubsection{Main entry point}
\begin{code}
main :: IO ()
main = do
    -- create configuration
    c <- prepare_configuration

    -- create initial top-level Trace
    (tr :: Trace IO Text, sb) <- setupTrace_ c "complex"

    -- load plugins
    Cardano.BM.Backend.Editor.plugin c tr sb
      >>= loadPlugin sb
    Cardano.BM.Backend.EKGView.plugin c tr sb
      >>= loadPlugin sb
    Cardano.BM.Backend.Aggregation.plugin c tr sb
      >>= loadPlugin sb
    Cardano.BM.Backend.Monitoring.plugin c tr sb
      >>= loadPlugin sb

    logNotice tr "starting program; hit CTRL-C to terminate"
-- user can watch the progress only if EKG is enabled.
    logInfo tr "watch its progress on http://localhost:12790"

#ifdef RUN_ProcBufferDump
    procDump <- dumpBuffer sb tr
#endif

#ifdef RUN_ProcRandom
    {- start thread sending unbounded sequence of random numbers
       to a trace which aggregates them into a statistics (sent to EKG) -}
    procRandom <- randomThr tr
#endif
#ifdef RUN_ProcMonitoring
    procMonitoring <- monitoringThr tr
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
#ifdef RUN_ProcMonitoring
    _ <- Async.waitCatch procMonitoring
#endif
#ifdef RUN_ProcBufferDump
    _ <- Async.waitCatch procDump
#endif

    return ()

\end{code}
