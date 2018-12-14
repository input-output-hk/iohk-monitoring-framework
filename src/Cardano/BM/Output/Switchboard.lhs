
\subsection{Cardano.BM.Output.Switchboard}\label{sec:Switchboard}

%if style == newcode
\begin{code}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.BM.Output.Switchboard
    (
      Switchboard
    , effectuate
    , realize
    , unrealize
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar,
                     withMVar)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import           Control.Monad (forM_, when)

import           Cardano.BM.Configuration (Configuration)
import           Cardano.BM.Configuration.Model (getBackends,
                     getDefaultBackends)
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.LogItem
import qualified Cardano.BM.Output.Aggregation
import qualified Cardano.BM.Output.EKGView
import qualified Cardano.BM.Output.Log

\end{code}
%endif

\subsubsection{Switchboard}\label{code:Switchboard}

\begin{code}
type SwitchboardMVar = MVar SwitchboardInternal
newtype Switchboard = Switchboard
    { getSB :: SwitchboardMVar }

data SwitchboardInternal = SwitchboardInternal
    { sbQueue       :: TBQ.TBQueue NamedLogItem
    }

\end{code}

\subsubsection{Process incoming messages}
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

\subsubsection{|Switchboard| implements |Backend| functions}

|Switchboard| is an \nameref{code:IsBackend}
\begin{code}
instance IsBackend Switchboard where
    typeof _ = SwitchboardBK

    realize cfg = do
        let spawnDispatcher :: IsBackend e => Configuration -> [(BackendKind, e)] -> TBQ.TBQueue NamedLogItem -> IO (Async.Async ())
            spawnDispatcher config backends queue =
                let qProc = do
                        nli <- atomically $ TBQ.readTBQueue queue
                        case lnItem nli of
                            AggregatedMessage _ _aggregated -> do
                                selectedBackends <- getBackends config (lnName nli)
                                let dropAggrBackends = filter (/= AggregationBK) selectedBackends
                                forM_ backends ( \(bek, be) ->
                                    when (bek `elem` dropAggrBackends) (dispatch nli be) )
                                qProc
                            KillPill ->
                                forM_ backends $ \(_, backend) ->
                                    unrealize backend
                            _ -> do
                                selectedBackends <- getBackends config (lnName nli)
                                forM_ backends $ \(bek, be) ->
                                    when (bek `elem` selectedBackends) (dispatch nli be)
                                qProc
                            -- if Nothing is in the queue then every backend needs to be terminated
                            -- and the dispatcher exits, the |Switchboard| stops.
                    dispatch :: IsEffectuator e => NamedLogItem -> e -> IO ()
                    dispatch nli backend = effectuate backend nli
                in
                Async.async qProc

        q <- atomically $ TBQ.newTBQueue 2048
        sbref <- newEmptyMVar
        putMVar sbref $ SwitchboardInternal q
        let sb = Switchboard sbref

        backends <- getDefaultBackends cfg
        bs <- setupBackends backends cfg sb []
 
        _ <- spawnDispatcher cfg bs q
        return sb

    unrealize _ = pure ()
\end{code}

\subsubsection{Realizing the backends according to configuration}
\begin{code}
setupBackends :: IsBackend e
            => [BackendKind]
            -> Configuration
            -> e
            -> [(BackendKind, e)]
            -> IO [(BackendKind, e)]
setupBackends [] _ _ acc = return acc
setupBackends (bk : bes) c sb acc = do
    be' <- setupBackend' bk c sb
    setupBackends bes c sb ((bk,be') : acc) 
setupBackend' :: IsBackend e => BackendKind -> Configuration -> e -> IO e
setupBackend' EKGViewBK c _ = Cardano.BM.Output.EKGView.realize c
setupBackend' AggregationBK c sb = Cardano.BM.Output.Aggregation.realizefrom c sb
setupBackend' KatipBK c _ = Cardano.BM.Output.Log.realize c
setupBackend' SwitchboardBK _ _ = error "cannot instantiate a further Switchboard"

\end{code}
