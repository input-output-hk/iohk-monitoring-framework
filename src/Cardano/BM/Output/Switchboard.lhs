
\subsection{Cardano.BM.Output.Switchboard}
\label{code:Cardano.BM.Output.Switchboard}

%if style == newcode
\begin{code}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.BM.Output.Switchboard
    (
      Switchboard (..)
    , mainTrace
    , effectuate
    , realize
    , unrealize
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar,
                     putMVar, readMVar, tryTakeMVar, withMVar, modifyMVar_)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import           Control.Exception.Safe (throwM)
import           Control.Monad (forM_, when, void)
import           Data.Time.Clock (getCurrentTime)
import           Data.Functor.Contravariant (Op (..))

import qualified Cardano.BM.BaseTrace as BaseTrace
import           Cardano.BM.Configuration (Configuration)
import           Cardano.BM.Configuration.Model (getBackends,
                     getSetupBackends)
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.MessageCounter (resetCounters, sendAndResetAfter,
                     updateMessageCounters)
import           Cardano.BM.Data.Trace (TraceNamed, TraceContext (..))
import           Cardano.BM.Data.Severity
import qualified Cardano.BM.Output.Log

#ifdef ENABLE_AGGREGATION
import           Cardano.BM.Data.SubTrace
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
type SwitchboardMVar = MVar SwitchboardInternal
newtype Switchboard = Switchboard
    { getSB :: SwitchboardMVar }

data SwitchboardInternal = SwitchboardInternal
    { sbQueue    :: TBQ.TBQueue NamedLogItem
    , sbDispatch :: Async.Async ()
    }

\end{code}

\subsubsection{Trace that forwards to the \nameref{code:Switchboard}}\label{code:mainTrace}\index{mainTrace}

Every |Trace| ends in the \nameref{code:Switchboard} which then takes care of
dispatching the messages to outputs

\begin{code}
mainTrace :: Switchboard -> TraceNamed IO
mainTrace sb = BaseTrace.BaseTrace $ Op $ \lognamed -> do
    effectuate sb lognamed

\end{code}

\subsubsection{Process incoming messages}\index{Switchboard!instance of IsEffectuator}
Incoming messages are put into the queue, and
then processed by the dispatcher.
\newline
The queue is initialized and the message dispatcher launched.

\begin{code}
instance IsEffectuator Switchboard where
    effectuate switchboard item = do
        let writequeue :: TBQ.TBQueue NamedLogItem -> NamedLogItem -> IO ()
            writequeue q i = do
                    nocapacity <- atomically $ TBQ.isFullTBQueue q
                    if nocapacity
                    then handleOverflow switchboard
                    else atomically $ TBQ.writeTBQueue q i

        sb <- readMVar (getSB switchboard)
        writequeue (sbQueue sb) item

    handleOverflow _ = putStrLn "Error: Switchboard's queue full, dropping log items!"

\end{code}

\begin{spec}
instead of 'writequeue ...':
        evalMonitoringAction config item >>=
            mapM_ (writequeue (sbQueue sb))

evalMonitoringAction :: Configuration -> NamedLogItem -> m [NamedLogItem]
evalMonitoringAction c item = return [item]
    -- let action = LogNamed { lnName=(lnName item) <> ".action", lnItem=LogMessage ... }
    -- return (action : item)

\end{spec}

\subsubsection{|Switchboard| implements |Backend| functions}\index{Switchboard!instance of IsBackend}

|Switchboard| is an \nameref{code:IsBackend}
\begin{code}
instance IsBackend Switchboard where
    typeof _ = SwitchboardBK

    realize cfg =
        let spawnDispatcher
                :: Configuration
                -> [(BackendKind, Backend)]
                -> TBQ.TBQueue NamedLogItem
                -> IO (Async.Async ())
            spawnDispatcher config backends queue = do
                now <- getCurrentTime
                let messageCounters = resetCounters now
                countersMVar <- newMVar messageCounters
                let traceInQueue q =
                        BaseTrace.BaseTrace $ Op $ \lognamed -> do
                            nocapacity <- atomically $ TBQ.isFullTBQueue q
                            if nocapacity
                            then putStrLn "Error: Switchboard's queue full, dropping log items!"
                            else atomically $ TBQ.writeTBQueue q lognamed
                    ctx = TraceContext { loggerName = ""
                                       , configuration = cfg
                                       , minSeverity = Debug
                                       , tracetype = Neutral
                                       }
                _timer <- Async.async $ sendAndResetAfter
                                            (ctx, traceInQueue queue)
                                            "#messagecounters.switchboard"
                                            countersMVar
                                            60000   -- 60000 ms = 1 min
                                            Warning -- Debug

                let sendMessage nli befilter = do
                        selectedBackends <- getBackends config (lnName nli)
                        let selBEs = befilter selectedBackends
                        forM_ backends $ \(bek, be) ->
                            when (bek `elem` selBEs) (bEffectuate be $ nli)

                    qProc counters = do
                        nli <- atomically $ TBQ.readTBQueue queue
                        -- do not count again messages that contain the results of message counters
                        when (lnName nli /= "#messagecounters.switchboard") $
                            -- increase the counter for the specific severity
                            modifyMVar_ counters $ \cnt -> return $ updateMessageCounters cnt $ lnItem nli
                        case lnItem nli of
                            LogObject _ KillPill ->
                                forM_ backends ( \(_, be) -> bUnrealize be )
#ifdef ENABLE_AGGREGATION
                            LogObject _ (AggregatedMessage _) -> do
                                sendMessage nli (filter (/= AggregationBK))
                                qProc counters
#endif
#ifdef ENABLE_MONITORING
                            LogObject _ (MonitoringEffect inner) -> do
                                sendMessage (nli {lnItem = inner}) (filter (/= MonitoringBK))
                                qProc counters
#endif
                            _ -> sendMessage nli id >> qProc counters

                Async.async $ qProc countersMVar
        in do
        q <- atomically $ TBQ.newTBQueue 2048
        sbref <- newEmptyMVar
        let sb :: Switchboard = Switchboard sbref

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
        let clearMVar :: MVar a -> IO ()
            clearMVar = void . tryTakeMVar

        (dispatcher, queue) <- withMVar (getSB switchboard) (\sb -> return (sbDispatch sb, sbQueue sb))
        -- send terminating item to the queue
        lo <- LogObject <$> (mkLOMeta Warning) <*> pure KillPill
        atomically $ TBQ.writeTBQueue queue $ LogNamed "kill.switchboard" lo
        -- wait for the dispatcher to exit
        res <- Async.waitCatch dispatcher
        either throwM return res
        (clearMVar . getSB) switchboard

\end{code}

\subsubsection{Realizing the backends according to configuration}\label{code:setupBackends}\index{Switchboard!setupBackends}
\begin{code}
setupBackends :: [BackendKind]
              -> Configuration
              -> Switchboard
              -> [(BackendKind, Backend)]
              -> IO [(BackendKind, Backend)]
setupBackends [] _ _ acc = return acc
setupBackends (bk : bes) c sb acc = do
    be' <- setupBackend' bk c sb
    setupBackends bes c sb ((bk,be') : acc)
setupBackend' :: BackendKind -> Configuration -> Switchboard -> IO Backend
setupBackend' SwitchboardBK _ _ = error "cannot instantiate a further Switchboard"
#ifdef ENABLE_MONITORING
setupBackend' MonitoringBK c sb = do
    let trace = mainTrace sb
        ctx   = TraceContext { loggerName = ""
                             , configuration = c
                             , minSeverity = Debug
                             , tracetype = Neutral
                             }

    be :: Cardano.BM.Output.Monitoring.Monitor <- Cardano.BM.Output.Monitoring.realizefrom (ctx, trace) sb
    return MkBackend
      { bEffectuate = Cardano.BM.Output.Monitoring.effectuate be
      , bUnrealize = Cardano.BM.Output.Monitoring.unrealize be
      }
#else
-- We need it anyway, to avoid "Non-exhaustive patterns" warning.
setupBackend' MonitoringBK _ _ =
    error "Impossible happened: monitoring is disabled by Cabal-flag, we mustn't match this backend!"
#endif
#ifdef ENABLE_EKG
setupBackend' EKGViewBK c sb = do
    let trace = mainTrace sb
        ctx   = TraceContext { loggerName = ""
                             , configuration = c
                             , minSeverity = Debug
                             , tracetype = Neutral
                             }

    be :: Cardano.BM.Output.EKGView.EKGView <- Cardano.BM.Output.EKGView.realizefrom (ctx, trace) sb
    return MkBackend
      { bEffectuate = Cardano.BM.Output.EKGView.effectuate be
      , bUnrealize = Cardano.BM.Output.EKGView.unrealize be
      }
#else
-- We need it anyway, to avoid "Non-exhaustive patterns" warning.
setupBackend' EKGViewBK _ _ =
    error "Impossible happened: EKG is disabled by Cabal-flag, we mustn't match this backend!"
#endif
#ifdef ENABLE_AGGREGATION
setupBackend' AggregationBK c sb = do
    let trace = mainTrace sb
        ctx   = TraceContext { loggerName = ""
                             , configuration = c
                             , minSeverity = Debug
                             , tracetype = Neutral
                             }

    be :: Cardano.BM.Output.Aggregation.Aggregation <- Cardano.BM.Output.Aggregation.realizefrom (ctx,trace) sb
    return MkBackend
      { bEffectuate = Cardano.BM.Output.Aggregation.effectuate be
      , bUnrealize = Cardano.BM.Output.Aggregation.unrealize be
      }
#else
-- We need it anyway, to avoid "Non-exhaustive patterns" warning.
setupBackend' AggregationBK _ _ =
    error "Impossible happened: aggregation is disabled by Cabal-flag, we mustn't match this backend!"
#endif
setupBackend' KatipBK c _ = do
    be :: Cardano.BM.Output.Log.Log <- Cardano.BM.Output.Log.realize c
    return MkBackend
      { bEffectuate = Cardano.BM.Output.Log.effectuate be
      , bUnrealize = Cardano.BM.Output.Log.unrealize be
      }

\end{code}
