
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

import           Cardano.BM.Configuration (Configuration)
-- import           Cardano.BM.Data.Aggregated
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.LogItem
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
    -- , monState   :: MonitorMap
    }

\end{code}

\subsubsection{Relation from context name to monitoring state}\label{code:MonitorMap}\label{code:MonitorState}
We remember the state of each monitored context name.
\begin{code}
data MonitorState
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

    handleOverflow _ = putStrLn "Notice: Aggregation's queue full, dropping log items!"

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
    Async.async $ qProc mempty
  where
    qProc state = do
        maybeItem <- atomically $ TBQ.readTBQueue mqueue
        case maybeItem of
            Just (LogNamed logname logvalue) -> do
                state' <- evalMonitoringAction state config logname logvalue
                qProc state'
            Nothing -> return ()  -- stop here

\end{code}

\subsubsection{Evaluation of monitoring action}\label{code:evalMonitoringAction}
Inspect the log message and match it against configured thresholds. If positive,
then run the action on the current state and return the updated state.
\begin{code}
evalMonitoringAction :: MonitorMap -> Configuration -> LoggerName -> LogObject -> IO MonitorMap
evalMonitoringAction mmap _config _logname _logvalue = return mmap

\end{code}
