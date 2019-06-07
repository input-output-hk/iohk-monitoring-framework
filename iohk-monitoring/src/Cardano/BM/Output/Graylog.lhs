
\subsection{Cardano.BM.Output.Graylog}
\label{code:Cardano.BM.Output.Graylog}

%if style == newcode
\begin{code}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.BM.Output.Graylog
    (
      Graylog
    , effectuate
    , realizefrom
    , unrealize
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar,
                     putMVar, readMVar, withMVar, modifyMVar_)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import           Control.Monad.IO.Class (liftIO)
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Data.Time (getCurrentTime)
--import           Data.Version (showVersion)
import           System.IO (stderr)

--import           Paths_iohk_monitoring (version)

import           Cardano.BM.Configuration (Configuration, getGraylogPort)
import           Cardano.BM.Data.Aggregated
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.MessageCounter (MessageCounter, resetCounters,
                     sendAndResetAfter, updateMessageCounters)
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.Trace
import           Cardano.BM.Data.Tracer (Tracer (..), ToObject (..))
import           Cardano.BM.Configuration (getGraylogPort)
import qualified Cardano.BM.Trace as Trace

\end{code}
%endif

\subsubsection{Structure of Graylog}\label{code:Graylog}\index{Graylog}
\begin{code}
type GraylogMVar a = MVar (GraylogInternal a)
newtype Graylog a = Graylog
    { getGL :: GraylogMVar a }

data GraylogInternal a = GraylogInternal
    { glQueue :: TBQ.TBQueue (Maybe (LogObject a))
    }

\end{code}

\subsubsection{Graylog is an effectuator}\index{Graylog!instance of IsEffectuator}
Function |effectuate| is called to pass in a |LogObject| to forward to Graylog.
In case the queue is full, all new items are dropped.
\begin{code}
instance IsEffectuator Graylog a where
    effectuate graylog item = do
        gelf <- readMVar (getGL graylog)
        let enqueue a = do
                        nocapacity <- atomically $ TBQ.isFullTBQueue (glQueue gelf)
                        if nocapacity
                        then handleOverflow graylog
                        else atomically $ TBQ.writeTBQueue (glQueue gelf) (Just a)
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

    handleOverflow _ = TIO.hPutStrLn stderr "Notice: Graylogs's queue full, dropping log items!"

\end{code}

\subsubsection{|Graylog| implements |Backend| functions}\index{Graylog!instance of IsBackend}

|Graylog| is an |IsBackend|
\begin{code}
instance ToObject a => IsBackend Graylog a where
    typeof _ = GraylogBK

    realize _ = fail "Graylog cannot be instantiated by 'realize'"

    realizefrom config sbtrace _ = do
        glref <- newEmptyMVar
        let graylog = Graylog glref
        queue <- atomically $ TBQ.newTBQueue 1024
        dispatcher <- spawnDispatcher config queue sbtrace
        -- link the given Async to the current thread, such that if the Async
        -- raises an exception, that exception will be re-thrown in the current
        -- thread, wrapped in ExceptionInLinkedThread.
        Async.link dispatcher
        putMVar glref $ GraylogInternal
                        { glQueue = queue
                        }
        return graylog

    unrealize graylog =
        withMVar (getGL graylog) $ \gelf ->
            atomically $ TBQ.writeTBQueue (glQueue gelf) Nothing

\end{code}

\subsubsection{Asynchronously reading log items from the queue and their processing}
\begin{code}
spawnDispatcher :: Configuration
                -> TBQ.TBQueue (Maybe (LogObject a))
                -> Trace.Trace IO a
                -> IO (Async.Async ())
spawnDispatcher config evqueue sbtrace = do
    now <- getCurrentTime
    let messageCounters = resetCounters now
    countersMVar <- newMVar messageCounters
    _timer <- Async.async $ sendAndResetAfter
                                sbtrace
                                "#messagecounters.graylog"
                                countersMVar
                                60000   -- 60000 ms = 1 min
                                Warning -- Debug

    Async.async $ qProc countersMVar
  where
    {-@ lazy qProc @-}
    qProc :: MVar MessageCounter -> IO ()
    qProc counters = do
        -- TODO read all items and process list at once
        maybeItem <- atomically $ TBQ.readTBQueue evqueue
        case maybeItem of
            Just obj@(LogObject logname _ _) -> do
                -- increase the counter for the type of message
                modifyMVar_ counters $ \cnt -> return $ updateMessageCounters cnt obj
                qProc counters
            Nothing -> return ()  -- stop here

\end{code}
