
\subsection{Cardano.BM.Backend.EKGView}
\label{code:Cardano.BM.Backend.EKGView}

%if style == newcode
\begin{code}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.BM.Backend.EKGView
    (
      EKGView
    , effectuate
    , realizefrom
    , unrealize
    ) where

import           Control.Concurrent (killThread)
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar,
                     putMVar, readMVar, withMVar, modifyMVar_)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import           Control.Monad.IO.Class (liftIO)
import qualified Data.HashMap.Strict as HM
import           Data.Int (Int64)
import           Data.Text (Text, pack, stripPrefix)
import qualified Data.Text.IO as TIO
import           Data.Time (getCurrentTime)
import           Data.Version (showVersion)

import           System.IO (stderr)
import qualified System.Metrics.Gauge as Gauge
import qualified System.Metrics.Label as Label
import           System.Remote.Monitoring (Server, forkServer,
                     getGauge, getLabel, serverThreadId)

import           Paths_iohk_monitoring (version)

import           Cardano.BM.Configuration (Configuration, getEKGport,
                     testSubTrace)
import           Cardano.BM.Data.Aggregated
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.MessageCounter (MessageCounter, resetCounters,
                     sendAndResetAfter, updateMessageCounters)
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.Trace
import           Cardano.BM.Data.Tracer (Tracer (..), ToObject (..))
#ifdef ENABLE_PROMETHEUS
import           Cardano.BM.Configuration (getPrometheusPort)
import           Cardano.BM.Backend.Prometheus (spawnPrometheus)
#endif
import qualified Cardano.BM.Trace as Trace

\end{code}
%endif

\subsubsection{Structure of EKGView}\label{code:EKGView}\index{EKGView}
\begin{code}
type EKGViewMVar a = MVar (EKGViewInternal a)
newtype EKGView a = EKGView
    { getEV :: EKGViewMVar a }

data EKGViewInternal a = EKGViewInternal
    { evQueue   :: TBQ.TBQueue (Maybe (LogObject a))
    , evLabels  :: EKGViewMap Label.Label
    , evGauges  :: EKGViewMap Gauge.Gauge
    , evServer  :: Server
    }

\end{code}

\subsubsection{Relation from variable name to label handler}
We keep the label handlers for later update in a |HashMap|.
\begin{code}
type EKGViewMap a = HM.HashMap Text a

\end{code}

\subsubsection{Internal |Trace|}
This is an internal |Trace|, named "\#ekgview", which can be used to control
the messages that are being displayed by EKG.
\begin{code}
ekgTrace :: ToObject a => EKGView a -> Configuration -> IO (Trace IO a)
ekgTrace ekg _c = do
    let basetrace = ekgTrace' ekg
    Trace.appendName "#ekgview" basetrace
  where
    ekgTrace' :: ToObject a => EKGView a -> Tracer IO (LogObject a)
    ekgTrace' ekgview = Tracer $ \lo@(LogObject loname _ _) -> do
        let setLabel :: Text -> Text -> EKGViewInternal a -> IO (Maybe (EKGViewInternal a))
            setLabel name label ekg_i@(EKGViewInternal _ labels _ server) =
                case HM.lookup name labels of
                    Nothing -> do
                        ekghdl <- getLabel name server
                        Label.set ekghdl label
                        return $ Just $ ekg_i { evLabels = HM.insert name ekghdl labels}
                    Just ekghdl -> do
                        Label.set ekghdl label
                        return Nothing
            setGauge :: Text -> Int64 -> EKGViewInternal a -> IO (Maybe (EKGViewInternal a))
            setGauge name value ekg_i@(EKGViewInternal _ _ gauges server) =
                case HM.lookup name gauges of
                    Nothing -> do
                        ekghdl <- getGauge name server
                        Gauge.set ekghdl value
                        return $ Just $ ekg_i { evGauges = HM.insert name ekghdl gauges}
                    Just ekghdl -> do
                        Gauge.set ekghdl value
                        return Nothing

            update :: ToObject a => LogObject a -> EKGViewInternal a -> IO (Maybe (EKGViewInternal a))
            update (LogObject logname _ (LogMessage logitem)) ekg_i =
                setLabel logname (pack $ show $ toObject logitem) ekg_i
            update (LogObject logname _ (LogValue iname value)) ekg_i =
                let logname' = logname <> "." <> iname
                in
                case value of
                    (Microseconds x) -> setGauge ("us:"   <> logname') (fromIntegral x) ekg_i
                    (Nanoseconds  x) -> setGauge ("ns:"   <> logname') (fromIntegral x) ekg_i
                    (Seconds      x) -> setGauge ("s:"    <> logname')  (fromIntegral x) ekg_i
                    (Bytes        x) -> setGauge ("B:"    <> logname')  (fromIntegral x) ekg_i
                    (PureI        x) -> setGauge ("int:"  <> logname') (fromIntegral x) ekg_i
                    (PureD        _) -> setLabel ("real:" <> logname') (pack $ show value) ekg_i
                    (Severity     _) -> setLabel ("sev:"  <> logname') (pack $ show value) ekg_i

            update _ _ = return Nothing

        modifyMVar_ (getEV ekgview) $ \ekgup -> do
            let -- strip off some prefixes not necessary for display
                lognam1 = case stripPrefix "#ekgview.#aggregation." loname of
                        Nothing -> loname
                        Just ln' -> ln'
                logname = case stripPrefix "#ekgview." lognam1 of
                        Nothing -> lognam1
                        Just ln' -> ln'
            upd <- update lo{ loName = logname } ekgup
            case upd of
                Nothing     -> return ekgup
                Just ekgup' -> return ekgup'

\end{code}

\subsubsection{EKG view is an effectuator}\index{EKGView!instance of IsEffectuator}
Function |effectuate| is called to pass in a |LogObject| for display in EKG.
If the log item is an |AggregatedStats| message, then all its constituents are
put into the queue. In case the queue is full, all new items are dropped.
\begin{code}
instance IsEffectuator EKGView a where
    effectuate ekgview item = do
        ekg <- readMVar (getEV ekgview)
        let enqueue a = do
                        nocapacity <- atomically $ TBQ.isFullTBQueue (evQueue ekg)
                        if nocapacity
                        then handleOverflow ekgview
                        else atomically $ TBQ.writeTBQueue (evQueue ekg) (Just a)
        case item of
            (LogObject logname lometa (AggregatedMessage ags)) -> liftIO $ do
                let traceAgg :: [(Text,Aggregated)] -> IO ()
                    traceAgg [] = return ()
                    traceAgg ((n,AggregatedEWMA ewma):r) = do
                        enqueue $ LogObject (logname <> "." <> n) lometa (LogValue "avg" $ avg ewma)
                        traceAgg r
                    traceAgg ((n,AggregatedStats stats):r) = do
                        let statsname = logname <> "." <> n
                            qbasestats s' nm = do
                                enqueue $ LogObject nm lometa (LogValue "mean" (PureD $ meanOfStats s'))
                                enqueue $ LogObject nm lometa (LogValue "min" $ fmin s')
                                enqueue $ LogObject nm lometa (LogValue "max" $ fmax s')
                                enqueue $ LogObject nm lometa (LogValue "count" $ PureI $ fromIntegral $ fcount s')
                                enqueue $ LogObject nm lometa (LogValue "stdev" (PureD $ stdevOfStats s'))
                        enqueue $ LogObject statsname lometa (LogValue "last" $ flast stats)
                        qbasestats (fbasic stats) $ statsname <> ".basic"
                        qbasestats (fdelta stats) $ statsname <> ".delta"
                        qbasestats (ftimed stats) $ statsname <> ".timed"
                        traceAgg r
                traceAgg ags
            (LogObject _ _ (LogMessage _)) -> enqueue item
            (LogObject _ _ (LogValue _ _)) -> enqueue item
            _                              -> return ()

    handleOverflow _ = TIO.hPutStrLn stderr "Notice: EKGViews's queue full, dropping log items!"

\end{code}

\subsubsection{|EKGView| implements |Backend| functions}\index{EKGView!instance of IsBackend}

|EKGView| is an |IsBackend|
\begin{code}
instance ToObject a => IsBackend EKGView a where
    typeof _ = EKGViewBK

    realize _ = fail "EKGView cannot be instantiated by 'realize'"

    realizefrom config sbtrace _ = do
        evref <- newEmptyMVar
        let ekgview = EKGView evref
        evport <- getEKGport config
        ehdl <- forkServer "127.0.0.1" evport
        ekghdl <- getLabel "iohk-monitoring version" ehdl
        Label.set ekghdl $ pack (showVersion version)
        ekgtrace <- ekgTrace ekgview config
        queue <- atomically $ TBQ.newTBQueue 5120
        dispatcher <- spawnDispatcher config queue sbtrace ekgtrace
        -- link the given Async to the current thread, such that if the Async
        -- raises an exception, that exception will be re-thrown in the current
        -- thread, wrapped in ExceptionInLinkedThread.
        Async.link dispatcher
#ifdef ENABLE_PROMETHEUS
        prometheusPort <- getPrometheusPort config
        prometheusDispatcher <- spawnPrometheus ehdl prometheusPort
        Async.link prometheusDispatcher
#endif
        putMVar evref $ EKGViewInternal
                        { evLabels = HM.empty
                        , evGauges = HM.empty
                        , evServer = ehdl
                        , evQueue = queue
                        }
        return ekgview

    unrealize ekgview =
        withMVar (getEV ekgview) $ \ekg ->
            killThread $ serverThreadId $ evServer ekg

\end{code}

\subsubsection{Asynchronously reading log items from the queue and their processing}
\begin{code}
spawnDispatcher :: Configuration
                -> TBQ.TBQueue (Maybe (LogObject a))
                -> Trace.Trace IO a
                -> Trace.Trace IO a
                -> IO (Async.Async ())
spawnDispatcher config evqueue sbtrace ekgtrace = do
    now <- getCurrentTime
    let messageCounters = resetCounters now
    countersMVar <- newMVar messageCounters
    _timer <- Async.async $ sendAndResetAfter
                                sbtrace
                                "#messagecounters.ekgview"
                                countersMVar
                                60000   -- 60000 ms = 1 min
                                Warning -- Debug

    Async.async $ qProc countersMVar
  where
    {-@ lazy qProc @-}
    qProc :: MVar MessageCounter -> IO ()
    qProc counters = do
        maybeItem <- atomically $ TBQ.readTBQueue evqueue
        case maybeItem of
            Just obj@(LogObject logname _ _) -> do
                obj' <- testSubTrace config ("#ekgview." <> logname) obj
                case obj' of
                    Just lo@(LogObject logname' meta content) -> do
                        trace <- Trace.appendName logname' ekgtrace
                        Trace.traceNamedObject trace (meta, content)
                        -- increase the counter for the type of message
                        modifyMVar_ counters $ \cnt -> return $ updateMessageCounters cnt lo
                    Nothing -> pure ()
                qProc counters
            Nothing -> return ()  -- stop here

\end{code}
