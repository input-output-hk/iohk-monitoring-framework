
\subsection{Cardano.BM.Backend.Monitoring}
\label{module:Cardano.BM.Backend.Monitoring}



%if style == newcode
\begin{code}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-@ embed Ratio * as int             @-}
{-@ embed GHC.Natural.Natural as int @-}

module Cardano.BM.Backend.Monitoring
    (
      Monitor
    -- * Plugin
    , plugin
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar,
                     readMVar, tryReadMVar, tryTakeMVar, withMVar)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import           Control.Exception.Safe (throwM)
import           Control.Monad (void)
import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (catMaybes)
import           Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import           GHC.Clock (getMonotonicTimeNSec)
import           GHC.Conc (labelThread, myThreadId)
import           System.IO (stderr)

import           Cardano.BM.Backend.LogBuffer
import           Cardano.BM.Backend.ProcessQueue (processQueue)
import           Cardano.BM.Configuration.Model (Configuration, getMonitors)
import           Cardano.BM.Data.Aggregated
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.Counter (Counter (..), CounterState (..),
                     nameCounter)
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.MonitoringEval
import           Cardano.BM.Plugin (Plugin (..))
import qualified Cardano.BM.Trace as Trace

\end{code}
%endif

\subsubsection{Plugin definition}
\begin{code}
plugin :: (IsEffectuator s a, ToJSON a, FromJSON a)
       => Configuration -> Trace.Trace IO a -> s a -> IO (Plugin a)
plugin config trace sb = do
    be :: Cardano.BM.Backend.Monitoring.Monitor a <- realizefrom config trace sb
    return $ BackendPlugin
               (MkBackend { bEffectuate = effectuate be, bUnrealize = unrealize be })
               (bekind be)
\end{code}

\subsubsection{Structure of Monitoring}\label{code:Monitor}\index{Monitor}
\begin{code}
type MonitorMVar a = MVar (MonitorInternal a)
newtype Monitor a = Monitor
    { getMon :: MonitorMVar a }

data MonitorInternal a = MonitorInternal
    { monQueue    :: TBQ.TBQueue (Maybe (LogObject a))
    , monDispatch :: Async.Async ()
    , monBuffer   :: !(LogBuffer a)
    }

\end{code}

\subsubsection{Relation from context name to monitoring state}\label{code:MonitorMap}\label{code:MonitorState}
We remember the state of each monitored context name.
\begin{code}
data MonitorState = MonitorState {
      _preCondition :: !MEvPreCond
    , _expression   :: !MEvExpr
    , _actions      :: [MEvAction]
    , _environment  :: !Environment
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
    bekind _ = MonitoringBK

    realize _ = fail "Monitoring cannot be instantiated by 'realize'"

    realizefrom config sbtrace _ = do
        monref <- newEmptyMVar
        let monitor = Monitor monref
#ifdef PERFORMANCE_TEST_QUEUE
        let qSize = 1000000
#else
        let qSize = 512
#endif
        queue <- atomically $ TBQ.newTBQueue qSize
        dispatcher <- spawnDispatcher queue config sbtrace monitor
        monbuf :: Cardano.BM.Backend.LogBuffer.LogBuffer a <- Cardano.BM.Backend.LogBuffer.realize config
        -- link the given Async to the current thread, such that if the Async
        -- raises an exception, that exception will be re-thrown in the current
        -- thread, wrapped in ExceptionInLinkedThread.
        Async.link dispatcher
        putMVar monref $ MonitorInternal
                        { monQueue = queue
                        , monDispatch = dispatcher
                        , monBuffer = monbuf
                        }
        return monitor

    unrealize monitoring = do
        let clearMVar :: MVar b -> IO ()
            clearMVar = void . tryTakeMVar

        (dispatcher, queue) <- withMVar (getMon monitoring) (\mon ->
                                return (monDispatch mon, monQueue mon))
        -- send terminating item to the queue
        atomically $ TBQ.writeTBQueue queue Nothing
        -- wait for the dispatcher to exit
        res <- Async.waitCatch dispatcher
        either throwM return res
        clearMVar $ getMon monitoring

\end{code}

\subsubsection{Asynchronously reading log items from the queue and their processing}
\begin{code}
spawnDispatcher :: TBQ.TBQueue (Maybe (LogObject a))
                -> Configuration
                -> Trace.Trace IO a
                -> Monitor a
                -> IO (Async.Async ())
spawnDispatcher mqueue config sbtrace monitor =
    Async.async (do
                    myThreadId >>= flip labelThread "Monitoring dispatcher (lobemo-backend-monitoring)"
                    initMap >>= qProc)
  where
    {-@ lazy qProc @-}
    qProc state =
        processQueue
            mqueue
            processMonitoring
            state
            (\_ -> pure ())

    processMonitoring lo@LogObject{} state = do
        let accessBufferMap = do
                mon <- tryReadMVar (getMon monitor)
                case mon of
                    Nothing        -> return []
                    Just actualMon -> readBuffer $ monBuffer actualMon
        mbuf <- accessBufferMap
        let sbtraceWithMonitoring = Trace.appendName "#monitoring" sbtrace
        valuesForMonitoring <- getVarValuesForMonitoring config mbuf
        state' <- evalMonitoringAction sbtraceWithMonitoring
                                        state
                                        lo
                                        valuesForMonitoring
        return state'

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
        (_, LogObject _ _ (LogValue vn val))       -> [Just (vn, val) | vn `elem` varNames]
        (_, LogObject _ _ (AggregatedMessage agg)) -> concatMap getMeasurable agg
        (_, _)                                     -> []
      where
        getMeasurable :: (Text, Aggregated) -> [Maybe (VarName, Measurable)]
        getMeasurable agg = case agg of
            (vn, AggregatedEWMA (EWMA _ val)) -> [Just (vn <> ".ewma.avg", val) | vn `elem` varNames]
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
evalMonitoringAction sbtrace mmap logObj@(LogObject logname1 _ content) variables = do
    let logname = case content of
                    ObserveOpen  _ -> logname1 <> ".open"
                    ObserveDiff  _ -> logname1 <> ".diff"
                    ObserveClose _ -> logname1 <> ".close"
                    _              -> logname1
    let sbtrace' = Trace.appendName logname sbtrace
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
                mapM_ (evaluateAction sbtrace' env' expr) acts
                return $ HM.insert logname mon{_environment=env''} mmap
            else return mmap
  where
    updateEnv :: Environment -> LogObject a -> Environment
    updateEnv env (LogObject loname lometa (ObserveOpen (CounterState counters))) =
        let addenv = HM.fromList $ ("timestamp", Nanoseconds $ utc2ns (tstamp lometa))
                                 : countersEnvPairs (loname <> ".open") counters
        in
        HM.union addenv env
    updateEnv env (LogObject loname lometa (ObserveDiff (CounterState counters))) =
        let addenv = HM.fromList $ ("timestamp", Nanoseconds $ utc2ns (tstamp lometa))
                                 : countersEnvPairs (loname <> ".diff") counters
        in
        HM.union addenv env
    updateEnv env (LogObject loname lometa (ObserveClose (CounterState counters))) =
        let addenv = HM.fromList $ ("timestamp", Nanoseconds $ utc2ns (tstamp lometa))
                                 : countersEnvPairs (loname <> ".close") counters
        in
        HM.union addenv env
    updateEnv env (LogObject _ lometa (LogValue vn val)) =
        let addenv = HM.fromList [ (vn, val)
                                 , ("timestamp", Nanoseconds $ utc2ns (tstamp lometa))
                                 ]
        in
        HM.union addenv env
    updateEnv env (LogObject _ lometa (LogMessage _logitem)) =
        let addenv = HM.fromList [ ("severity", Severity (severity lometa))
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
        aggs2measurables ((n, AggregatedEWMA vewma):r) acc = aggs2measurables r $ (n <> ".avg", avg vewma) : acc
        aggs2measurables ((n, AggregatedStats s):r) acc = aggs2measurables r $
              (n <> ".mean", PureD . meanOfStats $ fbasic s)
            : (n <> ".flast", flast s)
            : (n <> ".fcount", PureI . fromIntegral . fcount $ fbasic s)
            : acc
    -- catch all
    updateEnv env _ = env

    countersEnvPairs loggerName = map $ \counter ->
        let name = loggerName <> "." <> nameCounter counter <> "." <> cName counter
            value = cValue counter
        in
            (name, value)

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
