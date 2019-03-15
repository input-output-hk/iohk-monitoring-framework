
\subsection{Cardano.BM.Output.Switchboard}
\label{code:Cardano.BM.Output.Switchboard}

%if style == newcode
\begin{code}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.BM.Output.Switchboard
    (
      Switchboard (..)
    , mainTrace
    , mainTraceConditionally
    , effectuate
    , realize
    , unrealize
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar,
                     putMVar, readMVar, tryTakeMVar, withMVar, modifyMVar_)
import           Control.Concurrent.STM (atomically, retry)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import           Control.Exception.Safe (throwM)
import           Control.Monad (forM_, when, void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (ToJSON)
import           Data.Maybe (fromMaybe)
import qualified Data.Text.IO as TIO
import           Data.Time.Clock (getCurrentTime)
import           Data.Functor.Contravariant (Op (..))
import           System.IO (stderr)

import qualified Cardano.BM.Tracer.Class as Tracer
import           Cardano.BM.Configuration (Configuration)
import qualified Cardano.BM.Configuration as Config
import           Cardano.BM.Configuration.Model (getBackends,
                     getSetupBackends)
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.MessageCounter (resetCounters, sendAndResetAfter,
                     updateMessageCounters)
import           Cardano.BM.Data.Trace (TraceNamed, TraceContext (..))
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.SubTrace (SubTrace (..))
import qualified Cardano.BM.Output.Log
import           Cardano.BM.Trace (evalFilters)

#ifdef ENABLE_AGGREGATION
import qualified Cardano.BM.Output.Aggregation
#endif

#ifdef ENABLE_EKG
import qualified Cardano.BM.Output.EKGView
#endif

#ifdef ENABLE_MONITORING
import qualified Cardano.BM.Output.Monitoring
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
    { sbQueue    :: TBQ.TBQueue (NamedLogItem a)
    , sbDispatch :: Async.Async ()
    }

\end{code}

\subsubsection{Trace that forwards to the \nameref{code:Switchboard}}\label{code:mainTrace}\index{mainTrace}

Every |Trace| ends in the \nameref{code:Switchboard} which then takes care of
dispatching the messages to outputs

\begin{code}
mainTrace :: Switchboard a -> TraceNamed IO a
mainTrace sb = Tracer.Tracer $ Op $ effectuate sb

mainTraceConditionally :: TraceContext -> Switchboard a -> TraceNamed IO a
mainTraceConditionally ctx sb = Tracer.Tracer $ Op $ \item@(LogObject loggername meta _) -> do
    globminsev  <- liftIO $ Config.minSeverity (configuration ctx)
    globnamesev <- liftIO $ Config.inspectSeverity (configuration ctx) loggername
    let minsev = max globminsev $ fromMaybe Debug globnamesev
    if (severity meta) >= minsev
    then effectuate sb item
    else return ()

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
        let writequeue :: TBQ.TBQueue (NamedLogItem a) -> NamedLogItem a -> IO ()
            writequeue q i = do
                    nocapacity <- atomically $ TBQ.isFullTBQueue q
                    if nocapacity
                    then handleOverflow switchboard
                    else atomically $ TBQ.writeTBQueue q i

        sb <- readMVar (getSB switchboard)
        writequeue (sbQueue sb) item

    -- TODO where to put this error message
    handleOverflow _ = TIO.hPutStrLn stderr "Error: Switchboard's queue full, dropping log items!"

\end{code}

\begin{spec}
instead of 'writequeue ...':
        evalMonitoringAction config item >>=
            mapM_ (writequeue (sbQueue sb))

evalMonitoringAction :: Configuration -> NamedLogItem a -> m [NamedLogItem a]
evalMonitoringAction c item = return [item]
    -- let action = LogNamed { lnName=(lnName item) <> ".action", lnItem=LogMessage ... }
    -- return (action : item)

\end{spec}

\subsubsection{|Switchboard| implements |Backend| functions}\index{Switchboard!instance of IsBackend}

|Switchboard| is an \nameref{code:IsBackend}
\begin{code}
instance (Show a, ToJSON a) => IsBackend Switchboard a where
    typeof _ = SwitchboardBK

    realize cfg =
        let spawnDispatcher
                :: (Show a)
                => Configuration
                -> [(BackendKind, Backend a)]
                -> TBQ.TBQueue (NamedLogItem a)
                -> IO (Async.Async ())
            spawnDispatcher config backends queue = do
                now <- getCurrentTime
                let messageCounters = resetCounters now
                countersMVar <- newMVar messageCounters
                let traceInQueue q =
                        Tracer.Tracer $ Op $ \lognamed -> do
                            nocapacity <- atomically $ TBQ.isFullTBQueue q
                            if nocapacity
                            then putStrLn "Error: Switchboard's queue full, dropping log items!"
                            else atomically $ TBQ.writeTBQueue q lognamed
                    ctx = TraceContext { configuration = cfg
                                       }
                _timer <- Async.async $ sendAndResetAfter
                                            (ctx, traceInQueue queue)
                                            "#messagecounters.switchboard"
                                            countersMVar
                                            60000   -- 60000 ms = 1 min
                                            Warning -- Debug

                let sendMessage nli befilter = do
                        selectedBackends <- getBackends config (loName nli)
                        let selBEs = befilter selectedBackends
                        forM_ backends $ \(bek, be) ->
                            when (bek `elem` selBEs) (bEffectuate be nli)

                    qProc counters = do
                        -- read complete queue at once and process items
                        nlis <- atomically $ do
                                      r <- TBQ.flushTBQueue queue
                                      when (null r) retry
                                      return r

                        let processItem nli@(LogObject loname lometa loitem) = do
                                let losev = severity lometa

                                -- evaluate minimum severity criteria
                                locsev <- fromMaybe Debug <$> Config.inspectSeverity cfg loname
                                globsev <- Config.minSeverity config
                                let sevGE = losev >= globsev && losev >= locsev

                                -- do not count again messages that contain the results of message counters
                                when (loname /= "#messagecounters.switchboard") $
                                    -- increase the counter for the specific severity
                                    modifyMVar_ counters $
                                        \cnt -> return $ updateMessageCounters cnt nli

                                subtrace <- fromMaybe Neutral <$> Config.findSubTrace (configuration ctx) loname
                                case subtrace of
                                    TeeTrace secName ->
                                        atomically $ TBQ.writeTBQueue queue $ nli{ loName = secName }
                                    _ -> return ()

                                let doOutput = case subtrace of
                                        FilterTrace filters ->
                                            case loitem of
                                                LogValue name _ ->
                                                    evalFilters filters (loname <> "." <> name)
                                                _ ->
                                                    evalFilters filters loname
                                        DropOpening -> case loitem of
                                                        ObserveOpen _ -> False
                                                        _             -> True
                                        NoTrace     -> False
                                        _           -> True

                                case loitem of
                                    KillPill -> do
                                        forM_ backends ( \(_, be) -> bUnrealize be )
                                        return False
#ifdef ENABLE_AGGREGATION
                                    (AggregatedMessage _) -> do
                                        when (sevGE && doOutput) $
                                            sendMessage nli (filter (/= AggregationBK))
                                        return True
#endif
#ifdef ENABLE_MONITORING
                                    (MonitoringEffect inner) -> do
                                        when (sevGE && doOutput) $
                                            sendMessage (inner {loName = loname}) (filter (/= MonitoringBK))
                                        return True
#endif
                                    _ -> do
                                        when (sevGE && doOutput) $
                                            sendMessage nli id
                                        return True

                        res <- mapM processItem nlis
                        when (and res) $ qProc counters

                Async.async $ qProc countersMVar
        in do
        q <- atomically $ TBQ.newTBQueue 2048
        sbref <- newEmptyMVar
        let sb :: Switchboard a = Switchboard sbref

        backends <- getSetupBackends cfg
        bs <- setupBackends backends cfg sb []
        dispatcher <- spawnDispatcher cfg bs q
        -- link the given Async to the current thread, such that if the Async
        -- raises an exception, that exception will be re-thrown in the current
        -- thread, wrapped in ExceptionInLinkedThread.
        Async.link dispatcher
        putMVar sbref $ SwitchboardInternal {sbQueue = q, sbDispatch = dispatcher}

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

\subsubsection{Realizing the backends according to configuration}\label{code:setupBackends}\index{Switchboard!setupBackends}
\begin{code}
setupBackends :: (Show a, ToJSON a)
              => [BackendKind]
              -> Configuration
              -> Switchboard a
              -> [(BackendKind, Backend a)]
              -> IO [(BackendKind, Backend a)]
setupBackends [] _ _ acc = return acc
setupBackends (bk : bes) c sb acc = do
    be' <- setupBackend' bk c sb
    setupBackends bes c sb ((bk,be') : acc)
setupBackend' :: (Show a, ToJSON a) => BackendKind -> Configuration -> Switchboard a -> IO (Backend a)
setupBackend' SwitchboardBK _ _ = error "cannot instantiate a further Switchboard"
#ifdef ENABLE_MONITORING
setupBackend' MonitoringBK c sb = do
    let ctx   = TraceContext { configuration = c
                             }
        trace = mainTraceConditionally ctx sb

    be :: Cardano.BM.Output.Monitoring.Monitor a <- Cardano.BM.Output.Monitoring.realizefrom (ctx, trace) sb
    return MkBackend
      { bEffectuate = Cardano.BM.Output.Monitoring.effectuate be
      , bUnrealize = Cardano.BM.Output.Monitoring.unrealize be
      }
#else
-- We need it anyway, to avoid "Non-exhaustive patterns" warning.
setupBackend' MonitoringBK _ _ =
    TIO.hPutStrLn stderr "disabled! will not setup backend 'Monitoring'"
#endif
#ifdef ENABLE_EKG
setupBackend' EKGViewBK c sb = do
    let ctx   = TraceContext { configuration = c
                             }
        trace = mainTraceConditionally ctx sb

    be :: Cardano.BM.Output.EKGView.EKGView a <- Cardano.BM.Output.EKGView.realizefrom (ctx, trace) sb
    return MkBackend
      { bEffectuate = Cardano.BM.Output.EKGView.effectuate be
      , bUnrealize = Cardano.BM.Output.EKGView.unrealize be
      }
#else
-- We need it anyway, to avoid "Non-exhaustive patterns" warning.
setupBackend' EKGViewBK _ _ =
    TIO.hPutStrLn stderr "disabled! will not setup backend 'EKGView'"
#endif
#ifdef ENABLE_AGGREGATION
setupBackend' AggregationBK c sb = do
    let ctx   = TraceContext { configuration = c
                             }
        trace = mainTraceConditionally ctx sb

    be :: Cardano.BM.Output.Aggregation.Aggregation a <- Cardano.BM.Output.Aggregation.realizefrom (ctx,trace) sb
    return MkBackend
      { bEffectuate = Cardano.BM.Output.Aggregation.effectuate be
      , bUnrealize = Cardano.BM.Output.Aggregation.unrealize be
      }
#else
-- We need it anyway, to avoid "Non-exhaustive patterns" warning.
setupBackend' AggregationBK _ _ =
    TIO.hPutStrLn stderr "disabled! will not setup backend 'Aggregation'"
#endif
setupBackend' KatipBK c _ = do
    be :: Cardano.BM.Output.Log.Log a <- Cardano.BM.Output.Log.realize c
    return MkBackend
      { bEffectuate = Cardano.BM.Output.Log.effectuate be
      , bUnrealize = Cardano.BM.Output.Log.unrealize be
      }

\end{code}
