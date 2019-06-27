
\subsection{Cardano.BM.Backend.Monitoring}
\label{module:Cardano.BM.Backend.Monitoring}



%if style == newcode
\begin{code}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-@ embed Ratio * as int             @-}
{-@ embed GHC.Natural.Natural as int @-}

module Cardano.BM.Backend.Monitoring
    (
      Monitor
    , effectuate
    , realizefrom
    , unrealize
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar,
                     modifyMVar_, readMVar, tryReadMVar)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import           Data.Aeson (FromJSON)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (catMaybes)
import           Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import           Data.Time.Clock (getCurrentTime)
import           GHC.Clock (getMonotonicTimeNSec)
import           System.IO (stderr)

#ifdef QUEUE_FLUSH
import           Cardano.BM.Backend.ProcessQueue (processQueue)
#endif
import           Cardano.BM.Configuration.Model (Configuration, getMonitors)
import           Cardano.BM.Data.Aggregated
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.MessageCounter (resetCounters, sendAndResetAfter,
                     updateMessageCounters)
import           Cardano.BM.Data.MonitoringEval
import           Cardano.BM.Data.Severity (Severity (..))
import           Cardano.BM.Backend.LogBuffer
import qualified Cardano.BM.Trace as Trace

\end{code}
%endif

\subsubsection{Structure of Monitoring}\label{code:Monitor}\index{Monitor}
\begin{code}
type MonitorMVar a = MVar (MonitorInternal a)
newtype Monitor a = Monitor
    { getMon :: MonitorMVar a }

data MonitorInternal a = MonitorInternal
    { monQueue   :: TBQ.TBQueue (Maybe (LogObject a))
    , monBuffer  :: LogBuffer a
    }

\end{code}

\subsubsection{Relation from context name to monitoring state}\label{code:MonitorMap}\label{code:MonitorState}
We remember the state of each monitored context name.
\begin{code}
data MonitorState = MonitorState {
      _preCondition :: MEvPreCond
    , _expression   :: MEvExpr
    , _actions      :: [MEvAction]
    , _environment  :: Environment
    } deriving Show
type MonitorMap = HM.HashMap LoggerName MonitorState

\end{code}

\subsubsection{Monitor view is an effectuator}\index{Monitor!instance of IsEffectuator}
Function |effectuate| is called to pass in a |LogObject| for monitoring.
\begin{code}
instance IsEffectuator Monitor a where
    effectuate monitor item = do
        mon <- readMVar (getMon monitor)
        effectuate (monBuffer mon) item
        nocapacity <- atomically $ TBQ.isFullTBQueue (monQueue mon)
        if nocapacity
        then handleOverflow monitor
        else atomically $ TBQ.writeTBQueue (monQueue mon) $ Just item

    handleOverflow _ = TIO.hPutStrLn stderr "Notice: Monitor's queue full, dropping log items!\n"

\end{code}

\subsubsection{|Monitor| implements |Backend| functions}\index{Monitor!instance of IsBackend}

|Monitor| is an |IsBackend|
\begin{code}
instance FromJSON a => IsBackend Monitor a where
    typeof _ = MonitoringBK

    realize _ = fail "Monitoring cannot be instantiated by 'realize'"

    realizefrom config sbtrace _ = do
        monref <- newEmptyMVar
        let monitor = Monitor monref
        queue <- atomically $ TBQ.newTBQueue 512
        dispatcher <- spawnDispatcher queue config sbtrace monitor
        monbuf :: Cardano.BM.Backend.LogBuffer.LogBuffer a <- Cardano.BM.Backend.LogBuffer.realize config
        -- link the given Async to the current thread, such that if the Async
        -- raises an exception, that exception will be re-thrown in the current
        -- thread, wrapped in ExceptionInLinkedThread.
        Async.link dispatcher
        putMVar monref $ MonitorInternal
                        { monQueue = queue
                        , monBuffer = monbuf
                        }
        return monitor

    unrealize _ = return ()

\end{code}

\subsubsection{Asynchronously reading log items from the queue and their processing}
\begin{code}
spawnDispatcher :: TBQ.TBQueue (Maybe (LogObject a))
                -> Configuration
                -> Trace.Trace IO a
                -> Monitor a
                -> IO (Async.Async ())
spawnDispatcher mqueue config sbtrace monitor = do
    now <- getCurrentTime
    let messageCounters = resetCounters now
    countersMVar <- newMVar messageCounters
    _timer <- Async.async $ sendAndResetAfter
                                sbtrace
                                "#messagecounters.monitoring"
                                countersMVar
                                60000   -- 60000 ms = 1 min
                                Debug
    Async.async (initMap >>= qProc countersMVar)
  where
    {-@ lazy qProc @-}
    qProc counters state = do
#ifdef QUEUE_FLUSH
        processQueue
            mqueue
            processMonitoring
            (counters, state)
            (\_ -> pure ())
#else
        maybeItem <- atomically $ TBQ.readTBQueue mqueue
        case maybeItem of
            Just (logvalue@(LogObject _ _ _)) -> do
                let accessBufferMap = do
                        mon <- tryReadMVar (getMon monitor)
                        case mon of
                            Nothing        -> return []
                            Just actualMon -> readBuffer $ monBuffer actualMon
                mbuf <- accessBufferMap
                sbtraceWithMonitoring <- Trace.appendName "#monitoring" sbtrace
                valuesForMonitoring <- getVarValuesForMonitoring config mbuf
                state' <- evalMonitoringAction sbtraceWithMonitoring
                                               state
                                               logvalue
                                               valuesForMonitoring
                -- increase the counter for the type of message
                modifyMVar_ counters $ \cnt -> return $ updateMessageCounters cnt logvalue
                qProc counters state'
            Nothing -> return ()  -- stop here
#endif

#ifdef QUEUE_FLUSH
    processMonitoring lo@(LogObject _ _ _) (counters, state) = do
        let accessBufferMap = do
                mon <- tryReadMVar (getMon monitor)
                case mon of
                    Nothing        -> return []
                    Just actualMon -> readBuffer $ monBuffer actualMon
        mbuf <- accessBufferMap
        sbtraceWithMonitoring <- Trace.appendName "#monitoring" sbtrace
        valuesForMonitoring <- getVarValuesForMonitoring config mbuf
        state' <- evalMonitoringAction sbtraceWithMonitoring
                                        state
                                        lo
                                        valuesForMonitoring
        -- increase the counter for the type of message
        modifyMVar_ counters $ \cnt -> return $ updateMessageCounters cnt lo
        return (counters, state')
#endif

    initMap = do
        ls <- getMonitors config
        return $ HM.fromList $ map (\(n, (precond,e,as)) -> (n, MonitorState precond e as HM.empty))
                                   $ HM.toList ls
\end{code}

\begin{code}
getVarValuesForMonitoring :: Configuration
                          -> [(LoggerName, LogObject a)]
                          -> IO [(VarName, Measurable)]
getVarValuesForMonitoring config mbuf = do
    -- Here we take all var names for all monitors, just in case.
    monitorsInfo <- HM.elems <$> getMonitors config
    let varNames = concat [extractVarNames mEvExpr | (_, mEvExpr, _) <- monitorsInfo]
    return . catMaybes . concat $ map (getVNnVal varNames) mbuf
  where
    extractVarNames expr = case expr of
        Compare vn _  -> [vn]
        AND     e1 e2 -> extractVarNames e1 ++ extractVarNames e2
        OR      e1 e2 -> extractVarNames e1 ++ extractVarNames e2
        NOT     e     -> extractVarNames e

    getVNnVal varNames logObj = case logObj of
        (_, LogObject _ _ (LogValue vn val))       -> if vn `elem` varNames
                                                      then [Just (vn, val)]
                                                      else []
        (_, LogObject _ _ (AggregatedMessage agg)) -> concat $ map getMeasurable agg
        (_, _)                                     -> []
      where
        getMeasurable :: (Text, Aggregated) -> [Maybe (VarName, Measurable)]
        getMeasurable agg = case agg of
            (vn, AggregatedEWMA (EWMA _ val)) -> if vn `elem` varNames
                                                 then [Just (vn <> ".ewma.avg", val)]
                                                 else []
            (vn, AggregatedStats st)          -> if vn `elem` varNames
                                                 then stValues vn st
                                                 else []
            _                                 -> []
          where
            stValues vn st =
                [ Just (vn <> ".flast", flast st)
                , Just (vn <> ".fold",  fold st)

                , Just (vn <> ".fbasic.fmin",   fmin  . fbasic $ st)
                , Just (vn <> ".fbasic.fmax",   fmax  . fbasic $ st)
                , Just (vn <> ".fbasic.mean",   PureD . meanOfStats . fbasic $ st)
                , Just (vn <> ".fbasic.stdev",  PureD . stdevOfStats . fbasic $ st)
                , Just (vn <> ".fbasic.fcount", PureI . fromIntegral . fcount . fbasic $ st)

                , Just (vn <> ".fdelta.fmin",   fmin  . fdelta $ st)
                , Just (vn <> ".fdelta.fmax",   fmax  . fdelta $ st)
                , Just (vn <> ".fdelta.mean",   PureD . meanOfStats . fdelta $ st)
                , Just (vn <> ".fdelta.stdev",  PureD . stdevOfStats . fdelta $ st)
                , Just (vn <> ".fdelta.fcount", PureI . fromIntegral . fcount . fdelta $ st)

                , Just (vn <> ".ftimed.fmin",   fmin  . ftimed $ st)
                , Just (vn <> ".ftimed.fmax",   fmax  . ftimed $ st)
                , Just (vn <> ".ftimed.mean",   PureD . meanOfStats . ftimed $ st)
                , Just (vn <> ".ftimed.stdev",  PureD . stdevOfStats . ftimed $ st)
                , Just (vn <> ".ftimed.fcount", PureI . fromIntegral . fcount . ftimed $ st)
                ]

\end{code}

\subsubsection{Evaluation of monitoring action}\label{code:evalMonitoringAction}
Inspect the log message and match it against configured thresholds. If positive,
then run the action on the current state and return the updated state.
\begin{code}
evalMonitoringAction :: Trace.Trace IO a
                     -> MonitorMap
                     -> LogObject a
                     -> [(VarName, Measurable)]
                     -> IO MonitorMap
evalMonitoringAction sbtrace mmap logObj@(LogObject logname _ _) variables = do
    sbtrace' <- Trace.appendName logname sbtrace
    case HM.lookup logname mmap of
        Nothing -> return mmap
        Just mon@(MonitorState precond expr acts env0) -> do
            let env1 = updateEnv env0 logObj
            let env' = HM.union env1 $ HM.fromList variables
            let doMonitor = case precond of
                    -- There's no precondition, do monitor as usual.
                    Nothing -> True
                    -- Precondition is defined, do monitor only if it is True.
                    Just preCondExpr -> evaluate env' preCondExpr
            -- In this place env' already must contain opvn..
            let thresholdIsReached = evaluate env' expr
            if doMonitor && thresholdIsReached then do
                now <- getMonotonicTimeNSec
                let env'' = HM.insert "lastalert" (Nanoseconds now) env'
                TIO.putStrLn $ "alert! " <> logname <> " " <> (pack $ show acts) <> " " <> (pack $ show env'')
                mapM_ (evaluateAction sbtrace' env' expr) acts
                return $ HM.insert logname mon{_environment=env''} mmap
            else return mmap
  where
    updateEnv env (LogObject _ _ (ObserveOpen _)) = env
    updateEnv env (LogObject _ _ (ObserveDiff _)) = env
    updateEnv env (LogObject _ _ (ObserveClose _)) = env
    updateEnv env (LogObject _ lometa (LogValue vn val)) =
        let addenv = HM.fromList $ [ (vn, val)
                                   , ("timestamp", Nanoseconds $ utc2ns (tstamp lometa))
                                   ]
        in
        HM.union addenv env
    updateEnv env (LogObject _ lometa (LogMessage _logitem)) =
        let addenv = HM.fromList [ ("severity", (Severity (severity lometa)))
                                --  , ("selection", (liSelection logitem))
                                --  , ("message", (liPayload logitem))
                                 , ("timestamp", Nanoseconds $ utc2ns (tstamp lometa))
                                 ]
        in
        HM.union addenv env
    updateEnv env (LogObject _ lometa (AggregatedMessage vals)) =
        let addenv = ("timestamp", Nanoseconds $ utc2ns (tstamp lometa)) : aggs2measurables vals []
        in
        HM.union (HM.fromList addenv) env
      where
        aggs2measurables [] acc = acc
        aggs2measurables ((n, AggregatedEWMA ewma):r) acc = aggs2measurables r $ (n <> ".avg", avg ewma) : acc
        aggs2measurables ((n, AggregatedStats s):r) acc = aggs2measurables r $
              (n <> ".mean", PureD . meanOfStats $ fbasic s)
            : (n <> ".flast", flast s)
            : (n <> ".fcount", PureI . fromIntegral . fcount $ fbasic s)
            : acc
    -- catch all
    updateEnv env _ = env

    evaluateAction sbtrace' env expr (CreateMessage sev alertMessage) = do
        lometa <- mkLOMeta sev Public
        let fullMessage = alertMessage
                          <> "; environment is: " <> pack (show env)
                          <> "; threshold expression is: " <> pack (show expr)
        Trace.traceNamedObject sbtrace' (lometa, MonitoringEffect (MonitorAlert fullMessage))
    evaluateAction sbtrace' _ _ (SetGlobalMinimalSeverity sev) = do
        lometa <- mkLOMeta sev Public
        Trace.traceNamedObject sbtrace' (lometa, MonitoringEffect (MonitorAlterGlobalSeverity sev))
    evaluateAction sbtrace' _ _ (AlterSeverity loggerName sev) = do
        lometa <- mkLOMeta sev Public
        Trace.traceNamedObject sbtrace' (lometa, MonitoringEffect (MonitorAlterSeverity loggerName sev))

\end{code}
