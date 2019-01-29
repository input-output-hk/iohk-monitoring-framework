
\subsection{Cardano.BM.Output.Switchboard}\label{sec:Switchboard}

%if style == newcode
\begin{code}
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
import           Control.Concurrent.MVar (MVar, modifyMVar_, newEmptyMVar,
                    putMVar, tryTakeMVar, withMVar)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import           Control.Exception.Safe (throwM)
import           Control.Monad (forM_, when, void)
import           Data.Functor.Contravariant (Op (..))

import qualified Cardano.BM.BaseTrace as BaseTrace
import           Cardano.BM.Configuration (Configuration)
import           Cardano.BM.Configuration.Model (getBackends,
                     getSetupBackends)
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Trace (TraceNamed, TraceContext (..))
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.SubTrace
import qualified Cardano.BM.Output.Aggregation
import qualified Cardano.BM.Output.EKGView
import qualified Cardano.BM.Output.Log

\end{code}
%endif

\subsubsection{Switchboard}\label{code:Switchboard}\index{Switchboard}

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
                then return ()
                else atomically $ TBQ.writeTBQueue q i
        withMVar (getSB switchboard) $ \sb ->
            writequeue (sbQueue sb) item
\end{code}

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
            spawnDispatcher config backends queue =
                let sendMessage nli befilter = do
                        selectedBackends <- getBackends config (lnName nli)
                        let selBEs = befilter selectedBackends
                        forM_ backends $ \(bek, be) ->
                            when (bek `elem` selBEs) (bEffectuate be $ nli)

                    qProc = do
                        nli <- atomically $ TBQ.readTBQueue queue
                        case lnItem nli of
                            LogObject _ KillPill ->
                                forM_ backends ( \(_, be) -> bUnrealize be )
                            LogObject _ (AggregatedMessage _) -> do
                                sendMessage nli (filter (/= AggregationBK))
                                qProc
                            _ -> sendMessage nli id >> qProc
                in
                Async.async qProc
        in do
        q <- atomically $ TBQ.newTBQueue 2048
        sbref <- newEmptyMVar
        putMVar sbref $ SwitchboardInternal q $ error "unitialized dispatcher"
        let sb :: Switchboard = Switchboard sbref

        backends <- getSetupBackends cfg
        bs <- setupBackends backends cfg sb []
        dispatcher <- spawnDispatcher cfg bs q
        -- link the given Async to the current thread, such that if the Async
        -- raises an exception, that exception will be re-thrown in the current
        -- thread, wrapped in ExceptionInLinkedThread.
        Async.link dispatcher
        modifyMVar_ sbref $ \sbInternal -> return $ sbInternal {sbDispatch = dispatcher}

        return sb

    unrealize switchboard = do
        let clearMVar :: MVar a -> IO ()
            clearMVar = void . tryTakeMVar

        (dispatcher, queue) <- withMVar (getSB switchboard) (\sb -> return (sbDispatch sb, sbQueue sb))
        -- send terminating item to the queue
        lo <- LogObject <$> mkLOMeta <*> pure KillPill
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
setupBackend' EKGViewBK c _ = do
    be :: Cardano.BM.Output.EKGView.EKGView <- Cardano.BM.Output.EKGView.realize c
    return MkBackend
      { bEffectuate = Cardano.BM.Output.EKGView.effectuate be
      , bUnrealize = Cardano.BM.Output.EKGView.unrealize be
      }
setupBackend' AggregationBK c sb = do
    let trace = mainTrace sb
        ctx   = TraceContext { loggerName = ""
                             , configuration = c
                             , minSeverity = Debug
                             , tracetype = Neutral
                             , shutdown = pure ()
                             }

    be :: Cardano.BM.Output.Aggregation.Aggregation <- Cardano.BM.Output.Aggregation.realizefrom (ctx,trace) sb
    return MkBackend
      { bEffectuate = Cardano.BM.Output.Aggregation.effectuate be
      , bUnrealize = Cardano.BM.Output.Aggregation.unrealize be
      }
setupBackend' KatipBK c _ = do
    be :: Cardano.BM.Output.Log.Log <- Cardano.BM.Output.Log.realize c
    return MkBackend
      { bEffectuate = Cardano.BM.Output.Log.effectuate be
      , bUnrealize = Cardano.BM.Output.Log.unrealize be
      }

\end{code}
