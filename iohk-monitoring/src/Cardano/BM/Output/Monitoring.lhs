
\subsection{Cardano.BM.Output.Monitoring}
\label{module:Cardano.BM.Output.Monitoring}



%if style == newcode
\begin{code}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.BM.Output.Monitoring
    (
      Monitor
    , effectuate
    , realizefrom
    , unrealize
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar,
                     modifyMVar_, readMVar)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import qualified Data.HashMap.Strict as HM
import           Data.Text (pack)
import qualified Data.Text.IO as TIO
import           Data.Time.Calendar (toModifiedJulianDay)
import           Data.Time.Clock (UTCTime (..), getCurrentTime)
import           GHC.Clock (getMonotonicTimeNSec)
import           System.IO (stderr)

import           Cardano.BM.Configuration.Model (Configuration, getMonitors)
import           Cardano.BM.Data.Aggregated
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.MessageCounter (resetCounters, sendAndResetAfter,
                     updateMessageCounters)
import           Cardano.BM.Data.MonitoringEval
import           Cardano.BM.Data.Severity (Severity (..))
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
        nocapacity <- atomically $ TBQ.isFullTBQueue (monQueue mon)
        if nocapacity
        then handleOverflow monitor
        else atomically $ TBQ.writeTBQueue (monQueue mon) $ Just item

    handleOverflow _ = TIO.hPutStrLn stderr "Notice: Monitor's queue full, dropping log items!\n"

\end{code}

\subsubsection{|Monitor| implements |Backend| functions}\index{Monitor!instance of IsBackend}

|Monitor| is an |IsBackend|
\begin{code}
instance IsBackend Monitor a where
    typeof _ = MonitoringBK

    realize _ = error "Monitoring cannot be instantiated by 'realize'"

    realizefrom config sbtrace _ = do
        monref <- newEmptyMVar
        let monitor = Monitor monref
        queue <- atomically $ TBQ.newTBQueue 512
        dispatcher <- spawnDispatcher queue config sbtrace
        -- link the given Async to the current thread, such that if the Async
        -- raises an exception, that exception will be re-thrown in the current
        -- thread, wrapped in ExceptionInLinkedThread.
        Async.link dispatcher
        putMVar monref $ MonitorInternal
                        { monQueue = queue
                        -- , monState = mempty
                        }
        return monitor

    unrealize _ = return ()

\end{code}

\subsubsection{Asynchrouniously reading log items from the queue and their processing}
\begin{code}
spawnDispatcher :: TBQ.TBQueue (Maybe (LogObject a))
                -> Configuration
                -> Trace.Trace IO a
                -> IO (Async.Async ())
spawnDispatcher mqueue config sbtrace = do
    now <- getCurrentTime
    let messageCounters = resetCounters now
    countersMVar <- newMVar messageCounters
    _timer <- Async.async $ sendAndResetAfter
                                sbtrace
                                "#messagecounters.monitoring"
                                countersMVar
                                60000   -- 60000 ms = 1 min
                                Warning -- Debug

    Async.async (initMap >>= qProc countersMVar)
  where
    qProc counters state = do
        maybeItem <- atomically $ TBQ.readTBQueue mqueue
        case maybeItem of
            Just (logvalue@(LogObject _ _ _)) -> do
                state' <- evalMonitoringAction sbtrace state logvalue
                -- increase the counter for the type of message
                modifyMVar_ counters $ \cnt -> return $ updateMessageCounters cnt logvalue
                qProc counters state'
            Nothing -> return ()  -- stop here
    initMap = do
        ls <- getMonitors config
        return $ HM.fromList $ map (\(n, (precond,e,as)) -> (n, MonitorState precond e as HM.empty))
                                   $ HM.toList ls
\end{code}

\subsubsection{Evaluation of monitoring action}\label{code:evalMonitoringAction}
Inspect the log message and match it against configured thresholds. If positive,
then run the action on the current state and return the updated state.
\begin{code}
evalMonitoringAction :: Trace.Trace IO a
                     -> MonitorMap
                     -> LogObject a
                     -> IO MonitorMap
evalMonitoringAction sbtrace mmap logObj@(LogObject logname _ _) =
    case HM.lookup logname mmap of
        Nothing -> return mmap
        Just mon@(MonitorState precond expr acts env0) -> do
            let env' = updateEnv env0 logObj
            let doMonitor = case precond of
                    -- There's no precondition, do monitor as usual.
                    Nothing -> True
                    -- Precondition is defined, do monitor only if it is True.
                    Just preCondExpr -> evaluate env' preCondExpr
            let thresholdIsReached = evaluate env' expr
            if doMonitor && thresholdIsReached then do
                now <- getMonotonicTimeNSec
                let env'' = HM.insert "lastalert" (Nanoseconds now) env'
                TIO.putStrLn $ "alert! " <> logname <> " " <> (pack $ show acts) <> " " <> (pack $ show env'')
                mapM_ performAction acts
                return $ HM.insert logname mon{_environment=env''} mmap
            else return mmap
  where
    utc2ns (UTCTime days secs) =
        let yearsecs :: Rational
            yearsecs = 365 * 24 * 3600
            rdays,rsecs :: Rational
            rdays = toRational $ toModifiedJulianDay days
            rsecs = toRational secs
            s2ns = 1000000000
        in
        Nanoseconds $ round $ (fromRational $ s2ns * rsecs + rdays * yearsecs :: Double)
    updateEnv env (LogObject _ _ (ObserveOpen _)) = env
    updateEnv env (LogObject _ _ (ObserveDiff _)) = env
    updateEnv env (LogObject _ _ (ObserveClose _)) = env
    updateEnv env (LogObject _ lometa (LogValue vn val)) =
        let addenv = HM.fromList [ (vn, val)
                                 , ("timestamp", utc2ns (tstamp lometa))
                                 ]
        in
        HM.union addenv env
    updateEnv env (LogObject _ lometa (LogMessage _logitem)) =
        let addenv = HM.fromList [ ("severity", (Severity (severity lometa)))
                                --  , ("selection", (liSelection logitem))
                                --  , ("message", (liPayload logitem))
                                 , ("timestamp", utc2ns (tstamp lometa))
                                 ]
        in
        HM.union addenv env
    updateEnv env (LogObject _ lometa (AggregatedMessage vals)) =
        let addenv = ("timestamp", utc2ns (tstamp lometa)) : aggs2measurables vals []
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

    performAction (CreateMessage sev alertMessage) = do
        lometa <- mkLOMeta sev Public
        Trace.traceNamedObject sbtrace (lometa, MonitoringEffect (MonitorAlert alertMessage))

\end{code}
