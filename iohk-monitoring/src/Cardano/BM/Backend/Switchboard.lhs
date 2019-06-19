
\subsection{Cardano.BM.Backend.Switchboard}
\label{code:Cardano.BM.Backend.Switchboard}

%if style == newcode
\begin{code}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

#if !defined(mingw32_HOST_OS)
#define POSIX
#endif

module Cardano.BM.Backend.Switchboard
    (
      Switchboard (..)
    , MockSwitchboard (..)
    , mainTraceConditionally
    , traceMock
    , readLogBuffer
    , effectuate
    , realize
    , unrealize
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar,
                     modifyMVar_, putMVar, readMVar, tryTakeMVar, withMVar)
import           Control.Concurrent.STM (TVar, atomically, modifyTVar, retry)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import           Control.Exception.Safe (throwM)
import           Control.Monad (forM_, when, void)
import           Data.Aeson (FromJSON)
import           Data.Maybe (fromMaybe)
import qualified Data.Text.IO as TIO
import           Data.Time.Clock (getCurrentTime)
import           System.IO (stderr)

import           Cardano.BM.Configuration (Configuration)
import qualified Cardano.BM.Configuration as Config
import           Cardano.BM.Configuration.Model (getBackends,
                     getSetupBackends, setSeverity, setMinSeverity)
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.MessageCounter (resetCounters, sendAndResetAfter,
                     updateMessageCounters)
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.SubTrace (SubTrace (..))
import           Cardano.BM.Data.Tracer (Tracer (..), ToObject, traceWith)
import qualified Cardano.BM.Backend.TraceAcceptor
import qualified Cardano.BM.Backend.Log
import qualified Cardano.BM.Backend.LogBuffer
import qualified Cardano.BM.Backend.TraceForwarder

#ifdef POSIX
import           Cardano.BM.Backend.ExternalAbstraction (UnixNamedPipe)
#else
import           Cardano.BM.Backend.ExternalAbstraction (NoPipe)
#endif

#ifdef ENABLE_AGGREGATION
import qualified Cardano.BM.Backend.Aggregation
#endif

#ifdef ENABLE_EKG
import qualified Cardano.BM.Backend.EKGView
#endif

#ifdef ENABLE_GRAYLOG
import qualified Cardano.BM.Backend.Graylog
#endif

#ifdef ENABLE_MONITORING
import qualified Cardano.BM.Backend.Monitoring
#endif

#ifdef ENABLE_GUI
import qualified Cardano.BM.Backend.Editor
#endif

\end{code}
%endif

\subsubsection{Switchboard}\label{code:Switchboard}\index{Switchboard}

We are using an |MVar| because we spawn a set of backends that may try to send messages to
the switchboard before it is completely setup.

\begin{code}
type SwitchboardMVar a = MVar (SwitchboardInternal a)
newtype Switchboard a = Switchboard
    { getSB :: SwitchboardMVar a }

data SwitchboardInternal a = SwitchboardInternal
    { sbQueue     :: TBQ.TBQueue (LogObject a)
    , sbDispatch  :: Async.Async ()
    , sbLogBuffer :: Cardano.BM.Backend.LogBuffer.LogBuffer a
    }

\end{code}

\subsubsection{Trace that forwards to the |Switchboard|}
\label{code:mainTraceConditionally}\index{mainTraceConditionally}
Every |Trace| ends in the |Switchboard| which then takes care of
dispatching the messages to the selected backends.
\\
This |Tracer| will forward all messages unconditionally to the |Switchboard|.
(currently disabled)
\begin{spec}
mainTrace :: IsEffectuator eff a => eff a -> Tracer IO (LogObject a)
mainTrace = Tracer . effectuate

\end{spec}

This |Tracer| will apply to every message the severity filter as defined in the |Configuration|.
\begin{code}
mainTraceConditionally :: IsEffectuator eff a => Configuration -> eff a -> Tracer IO (LogObject a)
mainTraceConditionally config eff = Tracer $ \item -> do
    mayItem <- Config.testSubTrace config (loName item) item
    case mayItem of
        Just itemF@(LogObject loggername meta _) -> do
            passSevFilter <- Config.testSeverity config loggername meta
            when passSevFilter $
                effectuate eff itemF
        Nothing -> pure ()

\end{code}

\subsubsection{Process incoming messages}\index{Switchboard!instance of IsEffectuator}
Incoming messages are put into the queue, and then processed by the dispatcher.
The switchboard will never block when processing incoming messages
("eager receiver").
\newline
The queue is initialized and the message dispatcher launched.

\begin{code}
instance IsEffectuator Switchboard a where
    effectuate switchboard item = do
        let writequeue :: TBQ.TBQueue (LogObject a) -> LogObject a -> IO ()
            writequeue q i = do
                    nocapacity <- atomically $ TBQ.isFullTBQueue q
                    if nocapacity
                    then handleOverflow switchboard
                    else atomically $ TBQ.writeTBQueue q i

        sb <- readMVar (getSB switchboard)
        writequeue (sbQueue sb) item

    handleOverflow _ = TIO.hPutStrLn stderr "Error: Switchboard's queue full, dropping log items!"

\end{code}

\subsubsection{|Switchboard| implements |Backend| functions}\index{Switchboard!instance of IsBackend}

|Switchboard| is an |IsBackend|
\begin{code}
instance (FromJSON a, ToObject a) => IsBackend Switchboard a where
    typeof _ = SwitchboardBK

    realize cfg = do
        -- we setup |LogBuffer| explicitly so we can access it as a |Backend| and as |LogBuffer|
        logbuf :: Cardano.BM.Backend.LogBuffer.LogBuffer a <- Cardano.BM.Backend.LogBuffer.realize cfg
        let spawnDispatcher
                :: [(BackendKind, Backend a)]
                -> TBQ.TBQueue (LogObject a)
                -> IO (Async.Async ())
            spawnDispatcher backends queue = do
                now <- getCurrentTime
                let messageCounters = resetCounters now
                countersMVar <- newMVar messageCounters
                let traceInQueue q =
                        Tracer $ \lognamed -> do
                            item' <- Config.testSubTrace cfg (loName lognamed) lognamed
                            case item' of
                                Just obj@(LogObject loggername meta _) -> do
                                    passSevFilter <- Config.testSeverity cfg loggername meta
                                    when passSevFilter $ do
                                        nocapacity <- atomically $ TBQ.isFullTBQueue q
                                        if nocapacity
                                        then putStrLn "Error: Switchboard's queue full, dropping log items!"
                                        else atomically $ TBQ.writeTBQueue q obj
                                Nothing -> pure ()
                _timer <- Async.async $ sendAndResetAfter
                                            (traceInQueue queue)
                                            "#messagecounters.switchboard"
                                            countersMVar
                                            60000   -- 60000 ms = 1 min
                                            Warning -- Debug

                let sendMessage nli befilter = do
                        let name = case nli of
                                LogObject loname _ (LogValue valueName _) ->
                                    loname <> "." <> valueName
                                LogObject loname _ _ -> loname
                        selectedBackends <- getBackends cfg name
                        let selBEs = befilter selectedBackends
                        forM_ backends $ \(bek, be) ->
                            when (bek `elem` selBEs) (bEffectuate be nli)

                    qProc counters = do
                        -- read complete queue at once and process items
                        nlis <- atomically $ do
                                      r <- TBQ.flushTBQueue queue
                                      when (null r) retry
                                      return r

                        let processItem nli@(LogObject loname _ loitem) = do
                                when (loname /= "#messagecounters.switchboard") $
                                    modifyMVar_ counters $
                                        \cnt -> return $ updateMessageCounters cnt nli

                                Config.findSubTrace cfg loname >>= \case
                                    Just (TeeTrace sndName) ->
                                        atomically $ TBQ.writeTBQueue queue $ nli{ loName = loname <> "." <> sndName }
                                    _ -> return ()

                                case loitem of
                                    KillPill -> do
                                        forM_ backends ( \(_, be) -> bUnrealize be )
                                        return False
                                    (AggregatedMessage _) -> do
                                        sendMessage nli (filter (/= AggregationBK))
                                        return True
                                    (MonitoringEffect (MonitorAlert _)) -> do
                                        sendMessage nli (filter (/= MonitoringBK))
                                        return True
                                    (MonitoringEffect (MonitorAlterGlobalSeverity sev)) -> do
                                        setMinSeverity cfg sev
                                        return True
                                    (MonitoringEffect (MonitorAlterSeverity loggerName sev)) -> do
                                        setSeverity cfg loggerName (Just sev)
                                        return True
                                    (Command (DumpBufferedTo bk)) -> do
                                        msgs <- Cardano.BM.Backend.LogBuffer.readBuffer logbuf
                                        forM_ msgs (\(lonm, lobj) -> sendMessage (lobj {loName = lonm}) (const [bk]))
                                        return True
                                    _ -> do
                                        sendMessage nli id
                                        return True

                        res <- mapM processItem nlis
                        when (and res) $ qProc counters

                Async.async $ qProc countersMVar

        q <- atomically $ TBQ.newTBQueue 2048
        sbref <- newEmptyMVar
        let sb :: Switchboard a = Switchboard sbref

        backends <- getSetupBackends cfg
        bs0 <- setupBackends backends cfg sb
        bs1 <- return (LogBufferBK, MkBackend
                            { bEffectuate = Cardano.BM.Backend.LogBuffer.effectuate logbuf
                            , bUnrealize = Cardano.BM.Backend.LogBuffer.unrealize logbuf
                            })

        let bs = bs1 : bs0
        dispatcher <- spawnDispatcher bs q
        -- link the given Async to the current thread, such that if the Async
        -- raises an exception, that exception will be re-thrown in the current
        -- thread, wrapped in ExceptionInLinkedThread.
        Async.link dispatcher
        putMVar sbref $ SwitchboardInternal {sbQueue = q, sbDispatch = dispatcher, sbLogBuffer = logbuf}

        return sb

    unrealize switchboard = do
        let clearMVar :: MVar some -> IO ()
            clearMVar = void . tryTakeMVar

        (dispatcher, queue) <- withMVar (getSB switchboard) (\sb -> return (sbDispatch sb, sbQueue sb))
        -- send terminating item to the queue
        lo <- LogObject <$> pure "kill.switchboard"
                        <*> (mkLOMeta Warning Confidential)
                        <*> pure KillPill
        atomically $ TBQ.writeTBQueue queue lo
        -- wait for the dispatcher to exit
        res <- Async.waitCatch dispatcher
        either throwM return res
        (clearMVar . getSB) switchboard

\end{code}

\subsubsection{Reading the buffered log messages}\label{code:readLogBuffer}\index{readLogBuffer}
\begin{code}
readLogBuffer :: Switchboard a -> IO [(LoggerName, LogObject a)]
readLogBuffer switchboard = do
    sb <- readMVar (getSB switchboard)
    Cardano.BM.Backend.LogBuffer.readBuffer (sbLogBuffer sb)

\end{code}

\subsubsection{Realizing the backends according to configuration}\label{code:setupBackends}\index{Switchboard!setupBackends}
\begin{code}
setupBackends :: (FromJSON a, ToObject a)
              => [BackendKind]
              -> Configuration
              -> Switchboard a
              -> IO [(BackendKind, Backend a)]
setupBackends bes c sb = setupBackendsAcc bes []
  where
    setupBackendsAcc [] acc = return acc
    setupBackendsAcc (bk : r) acc = do
        setupBackend' bk c sb >>= \case
            Nothing -> setupBackendsAcc r acc
            Just be -> setupBackendsAcc r ((bk,be) : acc)

setupBackend' :: (FromJSON a, ToObject a) => BackendKind -> Configuration -> Switchboard a -> IO (Maybe (Backend a))
setupBackend' SwitchboardBK _ _ = fail "cannot instantiate a further Switchboard"
#ifdef ENABLE_MONITORING
setupBackend' MonitoringBK c sb = do
    let basetrace = mainTraceConditionally c sb

    be :: Cardano.BM.Backend.Monitoring.Monitor a <- Cardano.BM.Backend.Monitoring.realizefrom c basetrace sb
    return $ Just MkBackend
      { bEffectuate = Cardano.BM.Backend.Monitoring.effectuate be
      , bUnrealize = Cardano.BM.Backend.Monitoring.unrealize be
      }
#else
setupBackend' MonitoringBK _ _ = do
    TIO.hPutStrLn stderr "disabled! will not setup backend 'Monitoring'"
    return Nothing
#endif
#ifdef ENABLE_EKG
setupBackend' EKGViewBK c sb = do
    let basetrace = mainTraceConditionally c sb

    be :: Cardano.BM.Backend.EKGView.EKGView a <- Cardano.BM.Backend.EKGView.realizefrom c basetrace sb
    return $ Just MkBackend
      { bEffectuate = Cardano.BM.Backend.EKGView.effectuate be
      , bUnrealize = Cardano.BM.Backend.EKGView.unrealize be
      }
#else
setupBackend' EKGViewBK _ _ = do
    TIO.hPutStrLn stderr "disabled! will not setup backend 'EKGView'"
    return Nothing
#endif
#ifdef ENABLE_AGGREGATION
setupBackend' AggregationBK c sb = do
    let basetrace = mainTraceConditionally c sb

    be :: Cardano.BM.Backend.Aggregation.Aggregation a <- Cardano.BM.Backend.Aggregation.realizefrom c basetrace sb
    return $ Just MkBackend
      { bEffectuate = Cardano.BM.Backend.Aggregation.effectuate be
      , bUnrealize = Cardano.BM.Backend.Aggregation.unrealize be
      }
#else
setupBackend' AggregationBK _ _ = do
    TIO.hPutStrLn stderr "disabled! will not setup backend 'Aggregation'"
    return Nothing
#endif
#ifdef ENABLE_GUI
setupBackend' EditorBK c sb = do
    port <- Config.getGUIport c
    if port > 0
    then do
        let trace = mainTraceConditionally c sb
        be :: Cardano.BM.Backend.Editor.Editor a <- Cardano.BM.Backend.Editor.realizefrom c trace sb
        return $ Just MkBackend
            { bEffectuate = Cardano.BM.Backend.Editor.effectuate be
            , bUnrealize = Cardano.BM.Backend.Editor.unrealize be
            }
    else
        return Nothing
#else
setupBackend' EditorBK _ _ = do
    TIO.hPutStrLn stderr "disabled! will not setup backend 'Editor'"
    return Nothing
#endif
#ifdef ENABLE_GRAYLOG
setupBackend' GraylogBK c sb = do
    port <- Config.getGraylogPort c
    if port > 0
    then do
        let trace = mainTraceConditionally c sb
        be :: Cardano.BM.Backend.Graylog.Graylog a <- Cardano.BM.Backend.Graylog.realizefrom c trace sb
        return $ Just MkBackend
            { bEffectuate = Cardano.BM.Backend.Graylog.effectuate be
            , bUnrealize = Cardano.BM.Backend.Graylog.unrealize be
            }
    else
        return Nothing
#else
setupBackend' GraylogBK _ _ = do
    TIO.hPutStrLn stderr "disabled! will not setup backend 'Graylog'"
    return Nothing
#endif
setupBackend' KatipBK c _ = do
    be :: Cardano.BM.Backend.Log.Log a <- Cardano.BM.Backend.Log.realize c
    return $ Just MkBackend
        { bEffectuate = Cardano.BM.Backend.Log.effectuate be
        , bUnrealize = Cardano.BM.Backend.Log.unrealize be
        }
setupBackend' LogBufferBK _ _ = return Nothing
setupBackend' TraceAcceptorBK c sb = do
    let basetrace = mainTraceConditionally c sb

    be :: Cardano.BM.Backend.TraceAcceptor.TraceAcceptor PipeType a
            <- Cardano.BM.Backend.TraceAcceptor.realizefrom c basetrace sb
    return $ Just MkBackend
      { bEffectuate = Cardano.BM.Backend.TraceAcceptor.effectuate be
      , bUnrealize = Cardano.BM.Backend.TraceAcceptor.unrealize be
      }
setupBackend' TraceForwarderBK c sb = do
    let basetrace = mainTraceConditionally c sb

    be :: Cardano.BM.Backend.TraceForwarder.TraceForwarder PipeType a
            <- Cardano.BM.Backend.TraceForwarder.realizefrom c basetrace sb
    return $ Just MkBackend
      { bEffectuate = Cardano.BM.Backend.TraceForwarder.effectuate be
      , bUnrealize = Cardano.BM.Backend.TraceForwarder.unrealize be
      }

type PipeType =
#ifdef POSIX
    UnixNamedPipe
#else
    NoPipe
#endif

\end{code}

\subsubsection{MockSwitchboard}\label{code:MockSwitchboard}\index{MockSwitchboard}
|MockSwitchboard| is useful for tests since it keeps the |LogObject|s
to be output in a list.

\begin{code}
newtype MockSwitchboard a = MockSB (TVar [LogObject a])

instance IsEffectuator MockSwitchboard a where
    effectuate (MockSB tvar) item = atomically $ modifyTVar tvar ((:) item)
    handleOverflow _ = pure ()

\end{code}

\subsubsection{traceMock}\label{code:traceMock}\index{traceMock}
A |Tracer| which forwards |LogObject|s to |MockSwitchboard| simulating
functionality of |mainTraceConditionally|.

\begin{code}
traceMock :: MockSwitchboard a -> Config.Configuration -> Tracer IO (LogObject a)
traceMock ms config =
    Tracer $ \item@(LogObject loggername _ _) -> do
        traceWith mainTrace item
        subTrace <- fromMaybe Neutral <$> Config.findSubTrace config loggername
        case subTrace of
            TeeTrace secName ->
                traceWith mainTrace item{ loName = secName }
            _ -> return ()
  where
    mainTrace = mainTraceConditionally config ms

\end{code}
