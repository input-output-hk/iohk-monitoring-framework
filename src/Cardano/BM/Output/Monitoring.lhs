
\subsection{Cardano.BM.Output.Monitoring}
\label{module:Cardano.BM.Output.Monitoring}



%if style == newcode
\begin{code}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.Output.Monitoring
    (
      Monitor
    , effectuate
    , realize
    , unrealize
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar,
                     readMVar)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import qualified Data.HashMap.Strict as HM
import           Data.Text (pack)
import qualified Data.Text.IO as TIO
import           Data.Time.Calendar (toModifiedJulianDay)
import           Data.Time.Clock (UTCTime (..))
import           GHC.Clock (getMonotonicTimeNSec)

import           Cardano.BM.Configuration.Model (Configuration, getMonitors)
import           Cardano.BM.Data.Aggregated
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.MonitoringEval
-- import           Cardano.BM.Data.Severity
-- import           Cardano.BM.Data.SubTrace
-- import           Cardano.BM.Data.Trace

\end{code}
%endif

\subsubsection{Structure of Monitoring}\label{code:Monitor}\index{Monitor}
\begin{code}
type MonitorMVar = MVar MonitorInternal
newtype Monitor = Monitor
    { getMon :: MonitorMVar }

data MonitorInternal = MonitorInternal
    { monQueue   :: TBQ.TBQueue (Maybe NamedLogItem)
    }

\end{code}

\subsubsection{Relation from context name to monitoring state}\label{code:MonitorMap}\label{code:MonitorState}
We remember the state of each monitored context name.
\begin{code}
data MonitorState = MonitorState {
      _expression  :: MEvExpr
    , _actions     :: [MEvAction]
    , _environment :: Environment
    }
type MonitorMap = HM.HashMap LoggerName MonitorState

\end{code}

\subsubsection{Monitor view is an effectuator}\index{Monitor!instance of IsEffectuator}
Function |effectuate| is called to pass in a |NamedLogItem| for monitoring.
\begin{code}
instance IsEffectuator Monitor where
    effectuate monitor item = do
        mon <- readMVar (getMon monitor)
        nocapacity <- atomically $ TBQ.isFullTBQueue (monQueue mon)
        if nocapacity
        then handleOverflow monitor
        else atomically $ TBQ.writeTBQueue (monQueue mon) $ Just item

    handleOverflow _ = putStrLn "Notice: Monitor's queue full, dropping log items!"

\end{code}

\subsubsection{|Monitor| implements |Backend| functions}\index{Monitor!instance of IsBackend}

|Monitor| is an |IsBackend|
\begin{code}
instance IsBackend Monitor where
    typeof _ = MonitoringBK

    realize config = do
        monref <- newEmptyMVar
        let monitor = Monitor monref
        queue <- atomically $ TBQ.newTBQueue 512
        dispatcher <- spawnDispatcher queue config
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
spawnDispatcher :: TBQ.TBQueue (Maybe NamedLogItem)
                -> Configuration
                -> IO (Async.Async ())
spawnDispatcher mqueue config =
    Async.async (initMap >>= qProc)
  where
    qProc state = do
        maybeItem <- atomically $ TBQ.readTBQueue mqueue
        case maybeItem of
            Just (LogNamed logname logvalue) -> do
                state' <- evalMonitoringAction state logname logvalue
                qProc state'
            Nothing -> return ()  -- stop here
    initMap = do
        ls <- getMonitors config
        return $ HM.fromList $ map (\(n, (e,as)) -> (n, MonitorState e as HM.empty)) $ HM.toList ls
\end{code}

\subsubsection{Evaluation of monitoring action}\label{code:evalMonitoringAction}
Inspect the log message and match it against configured thresholds. If positive,
then run the action on the current state and return the updated state.
\begin{code}
evalMonitoringAction :: MonitorMap -> LoggerName -> LogObject -> IO MonitorMap
evalMonitoringAction mmap logname logvalue =
    case HM.lookup logname mmap of
        Nothing -> return mmap
        Just mon@(MonitorState expr acts env0) -> do
            let env' = updateEnv env0 logvalue
            if evaluate env' expr
            then do
                now <- getMonotonicTimeNSec
                let env'' = HM.insert "lastalert" (Nanoseconds now) env'
                TIO.putStrLn $ "alert! " <> logname <> " " <> (pack $ show acts) <> " " <> (pack $ show env'')
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
    updateEnv env (LogObject _ (ObserveOpen _)) = env
    updateEnv env (LogObject _ (ObserveDiff _)) = env
    updateEnv env (LogObject _ (ObserveClose _)) = env
    updateEnv env (LogObject lometa (LogValue vn val)) =
        let addenv = HM.fromList [ (vn, val)
                                 , ("timestamp", utc2ns (tstamp lometa))
                                 ]
        in
        HM.union addenv env
    updateEnv env (LogObject lometa (LogMessage logitem)) =
        let addenv = HM.fromList [ ("severity", (Severity (liSeverity logitem)))
                                --  , ("selection", (liSelection logitem))
                                --  , ("message", (liPayload logitem))
                                 , ("timestamp", utc2ns (tstamp lometa))
                                 ]
        in
        HM.union addenv env
    updateEnv env (LogObject lometa (AggregatedMessage vals)) =
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
\end{code}
