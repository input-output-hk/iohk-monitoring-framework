
\subsection{Cardano.BM.Backend.EKGView}
\label{code:Cardano.BM.Backend.EKGView}

%if style == newcode
\begin{code}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Cardano.BM.Backend.EKGView
    (
      EKGView
    -- * Plugin
    , plugin
    ) where

import           Control.Concurrent (killThread, threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar,
                     readMVar, withMVar, modifyMVar_, tryTakeMVar)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import           Control.Exception (Exception, SomeException, catch, throwIO)
import           Control.Exception.Safe (throwM)
import           Control.Monad (void, forM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.HashMap.Strict as HM
import           Data.Int (Int64)
import           Data.Maybe (fromMaybe)
import           Data.String (fromString)
import           Data.Text (Text, pack, stripPrefix)
import qualified Data.Text.IO as TIO
import           Data.Version (showVersion)

import           System.IO (stderr)
import qualified System.Metrics.Gauge as Gauge
import qualified System.Metrics.Label as Label
import           System.Remote.Monitoring (Server, forkServer,
                     getGauge, getLabel, serverThreadId)

import           Paths_iohk_monitoring (version)

import           Cardano.BM.Backend.ProcessQueue (processQueue)
import           Cardano.BM.Backend.Prometheus (spawnPrometheus)
import           Cardano.BM.Configuration (Configuration, getEKGport,
                     getPrometheusBindAddr, testSubTrace)
import           Cardano.BM.Data.Aggregated
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.Trace
import           Cardano.BM.Data.Tracer (Tracer (..), traceWith)
import           Cardano.BM.Plugin
import qualified Cardano.BM.Trace as Trace

\end{code}
%endif

\subsubsection{Plugin definition}
\begin{code}
plugin :: (IsEffectuator s a, ToJSON a, FromJSON a)
       => Configuration -> Trace.Trace IO a -> s a -> IO (Plugin a)
plugin config trace sb = do
    be :: Cardano.BM.Backend.EKGView.EKGView a <- realizefrom config trace sb
    return $ BackendPlugin
               (MkBackend { bEffectuate = effectuate be, bUnrealize = unrealize be })
               (bekind be)
\end{code}

\subsubsection{Structure of EKGView}\label{code:EKGView}\index{EKGView}
\begin{code}
type EKGViewMVar a = MVar (EKGViewInternal a)
newtype EKGView a = EKGView
    { getEV :: EKGViewMVar a }

data EKGViewInternal a = EKGViewInternal
    { evQueue              :: Maybe (TBQ.TBQueue (Maybe (LogObject a)))
    , evLabels             :: EKGViewMap Label.Label
    , evGauges             :: EKGViewMap Gauge.Gauge
    , evServer             :: Maybe Server
    , evDispatch           :: Maybe (Async.Async ())
    , evPrometheusDispatch :: Maybe (Async.Async ())
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
ekgTrace :: ToJSON a => EKGView a -> Configuration -> Trace IO a
ekgTrace ekg _c =
    Trace.appendName "#ekgview" $ ekgTrace' ekg
  where
    ekgTrace' :: ToJSON a => EKGView a -> Trace IO a
    ekgTrace' ekgview = Tracer $ \(_ctx,lo@(LogObject outerloname _ _)) -> do
        let setLabel :: Text -> Text -> EKGViewInternal a -> IO (Maybe (EKGViewInternal a))
            setLabel name label ekg_i@(EKGViewInternal _ labels _ mserver _ _) =
                case (HM.lookup name labels, mserver) of
                    (Nothing, Just server) -> do
                        ekghdl <- getLabel name server
                        Label.set ekghdl label
                        return $ Just $ ekg_i { evLabels = HM.insert name ekghdl labels}
                    (Just ekghdl, _) -> do
                        Label.set ekghdl label
                        return Nothing
                    (Nothing, Nothing) ->
                        pure Nothing
            setGauge :: Text -> Int64 -> EKGViewInternal a -> IO (Maybe (EKGViewInternal a))
            setGauge name value ekg_i@(EKGViewInternal _ _ gauges mserver _ _) =
                case (HM.lookup name gauges, mserver) of
                    (Nothing, Just server) -> do
                        ekghdl <- getGauge name server
                        Gauge.set ekghdl value
                        return $ Just $ ekg_i { evGauges = HM.insert name ekghdl gauges}
                    (Just ekghdl, _) -> do
                        Gauge.set ekghdl value
                        return Nothing
                    (Nothing, Nothing) ->
                        pure Nothing

            update :: ToJSON a => LogObject a -> EKGViewInternal a -> IO (Maybe (EKGViewInternal a))
            update (LogObject loname _ (LogMessage logitem)) ekg_i =
                setLabel loname (pack $ show $ encode logitem) ekg_i
            update (LogObject loname _ (LogValue iname value)) ekg_i =
                let logname = loname <> "." <> iname
                in
                case value of
                    (Microseconds x) -> setGauge (logname <> ".us") (fromIntegral x) ekg_i
                    (Nanoseconds  x) -> setGauge (logname <> ".ns") (fromIntegral x) ekg_i
                    (Seconds      x) -> setGauge (logname <> ".s") (fromIntegral x) ekg_i
                    (Bytes        x) -> setGauge (logname <> ".B") (fromIntegral x) ekg_i
                    (PureI        x) -> setGauge (logname <> ".int") (fromIntegral x) ekg_i
                    (PureD        _) -> setLabel (logname <> ".real") (pack $ show value) ekg_i
                    (Severity     _) -> setLabel (logname <> ".sev") (pack $ show value) ekg_i

            update _ _ = return Nothing

        modifyMVar_ (getEV ekgview) $ \ekgup -> do
            let -- strip off some prefixes not necessary for display
                loname1 = fromMaybe outerloname $ stripPrefix "#ekgview" outerloname
                loname = fromMaybe loname1 $ stripPrefix "#aggregation" loname1
            upd <- update lo{ loName = loname } ekgup
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
        case evQueue ekg of
          Nothing -> pure ()
          Just queue -> doEnqueue ekg queue
     where
       doEnqueue :: EKGViewInternal a -> TBQ.TBQueue (Maybe (LogObject a)) -> IO ()
       doEnqueue _ekg queue = do
         let enqueue a = do
                         nocapacity <- atomically $ TBQ.isFullTBQueue queue
                         if nocapacity
                         then handleOverflow ekgview
                         else atomically $ TBQ.writeTBQueue queue (Just a)
         case item of
             (LogObject loname lometa (AggregatedMessage ags)) -> liftIO $ do
                 let traceAgg :: [(Text,Aggregated)] -> IO ()
                     traceAgg [] = return ()
                     traceAgg ((n,AggregatedEWMA agewma):r) = do
                         enqueue $ LogObject (loname <> "." <> n) lometa (LogValue "avg" $ avg agewma)
                         traceAgg r
                     traceAgg ((n,AggregatedStats stats):r) = do
                         let statsname = loname <> "." <> n
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
instance (ToJSON a, FromJSON a) => IsBackend EKGView a where
    type BackendFailure EKGView = EKGBackendFailure

    bekind _ = EKGViewBK

    realize _ = fail "EKGView cannot be instantiated by 'realize'"

    realizefrom config sbtrace _ = do
        evref <- newEmptyMVar
        let ekgview = EKGView evref
        evport <- getEKGport config
        ehdl <- (forkServer "127.0.0.1" evport
                 -- This unfortunate delay is to catch the async exception.
                 <* threadDelay 300000)
            `catch` mkHandler EKGServerStartupError
        ekghdl <- getLabel "iohk-monitoring version" ehdl
        Label.set ekghdl $ pack (showVersion version)
        let ekgtrace = ekgTrace ekgview config
#ifdef PERFORMANCE_TEST_QUEUE
        let qSize = 1000000
#else
        let qSize = 5120
#endif
        queue <- atomically $ TBQ.newTBQueue qSize
        dispatcher <- spawnDispatcher config queue sbtrace ekgtrace
          `catch` mkHandler EKGDispatcherStartupError
        -- link the given Async to the current thread, such that if the Async
        -- raises an exception, that exception will be re-thrown in the current
        -- thread, wrapped in ExceptionInLinkedThread.
        Async.link dispatcher
        prometheusBindAddr <- getPrometheusBindAddr config
        prometheusDispatcher <-
                case prometheusBindAddr of
                  Just (host, port) -> do
                    pd <- spawnPrometheus ehdl (fromString host) port
                      `catch` mkHandler EKGPrometheusStartupError
                    Async.link pd
                    return (Just pd)
                  Nothing ->
                    return Nothing
        putMVar evref $ EKGViewInternal
                        { evLabels = HM.empty
                        , evGauges = HM.empty
                        , evServer = Just ehdl
                        , evQueue = Just queue
                        , evDispatch = Just dispatcher
                        , evPrometheusDispatch = prometheusDispatcher
                        }
        return ekgview
      `catch` -- Try to catch specific errors first.
      nullSetup sbtrace
      `catch` -- ..if that fails, catch everything.
      (nullSetup sbtrace . EKGUnknownStartupError . (show :: SomeException -> String))
     where
       mkHandler
         :: (String -> EKGBackendFailure)
         -> SomeException
         -> IO b
       mkHandler ctor = throwIO . ctor . show

       nullSetup
         :: Trace IO a
         -> EKGBackendFailure
         -> IO (EKGView a)
       nullSetup trace e = do
         meta <- mkLOMeta Error Public
         traceWith trace $ ("#ekgview.realizeFrom", LogObject "#ekgview.realizeFrom" meta $
           LogError $ "EKGView backend disabled due to initialisation error: " <> (pack $ show e))
         _ <- atomically $ TBQ.newTBQueue 0
         ref <- newEmptyMVar
         putMVar ref $ EKGViewInternal
           { evLabels = HM.empty
           , evGauges = HM.empty
           , evServer = Nothing
           , evQueue = Nothing
           , evDispatch = Nothing
           , evPrometheusDispatch = Nothing
           }
         pure $ EKGView ref

    unrealize ekgview = do
        let clearMVar :: MVar b -> IO ()
            clearMVar = void . tryTakeMVar

        withMVar (getEV ekgview) $ \ev -> do

            forM_ (evQueue ev) $
                -- send terminating item to the queue
                \queue ->
                    atomically $ TBQ.writeTBQueue queue Nothing

            forM_ (evDispatch ev) $
                -- wait for the dispatcher to exit
                \dispatcher -> do
                    res <- Async.waitCatch dispatcher
                    either throwM return res

            forM_ (evPrometheusDispatch ev) $
                Async.cancel

        withMVar (getEV ekgview) $ \ekg ->
            forM_ (evServer ekg) $
                \server -> killThread $ serverThreadId server

        clearMVar $ getEV ekgview

data EKGBackendFailure
  = EKGUnknownStartupError String
  | EKGServerStartupError String
  | EKGDispatcherStartupError String
  | EKGPrometheusStartupError String
  deriving Show

instance Exception EKGBackendFailure

\end{code}

\subsubsection{Asynchronously reading log items from the queue and their processing}
\begin{code}
spawnDispatcher :: Configuration
                -> TBQ.TBQueue (Maybe (LogObject a))
                -> Trace.Trace IO a
                -> Trace.Trace IO a
                -> IO (Async.Async ())
spawnDispatcher config evqueue _sbtrace ekgtrace =
    Async.async $ qProc
  where
    {-@ lazy qProc @-}
    qProc :: IO ()
    qProc =
        processQueue
            evqueue
            processEKGView
            ()
            (\_ -> pure ())

    processEKGView obj@(LogObject loname0 _ _) _ = do
        obj' <- testSubTrace config ("#ekgview." <> loname0) obj
        case obj' of
            Just (LogObject loname meta content) ->
                let trace = Trace.appendName loname ekgtrace
                in
                Trace.traceNamedObject trace (meta, content)
            Nothing -> pure ()
        pure ()

\end{code}
