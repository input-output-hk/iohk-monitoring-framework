
\subsection{Cardano.BM.Output.Switchboard}\label{sec:Switchboard}

%if style == newcode
\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.Output.Switchboard
    (
      Switchboard
    , setup
    , pass
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar,
                     withMVar)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import           Control.Monad (forM_, when)
import           Control.Monad.Catch (throwM)

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
The switchboard is a singleton.

\begin{code}
type SwitchboardMVar = MVar SwitchboardInternal
newtype Switchboard = Switchboard
    { getSB :: SwitchboardMVar }

-- Our internal state
data SwitchboardInternal = SwitchboardInternal
    { sbQueue       :: TBQ.TBQueue (Maybe NamedLogItem)
    , sbDispatch    :: Async.Async ()
    }

\end{code}

\subsubsection{Starting the switchboard from configuration}
The queue is initialized and the message dispatcher launched.
TODO: the backends should be connected according to configuration.

\begin{code}
setup :: Configuration -> IO Switchboard
setup cfg = do
    q <- atomically $ TBQ.newTBQueue 2048

    backends <- getDefaultBackends cfg
    bs <- setupBackends cfg [] backends q

    sbref <- newEmptyMVar
    d <- spawnDispatcher cfg bs q
    putMVar sbref $ SwitchboardInternal q d
    return $ Switchboard sbref
  where
    spawnDispatcher :: Configuration -> [(BackendKind, Backend)] -> TBQ.TBQueue (Maybe NamedLogItem) -> IO (Async.Async ())
    spawnDispatcher config backends queue = Async.async qProc
      where
        qProc = do
            nli' <- atomically $ TBQ.readTBQueue queue
            case nli' of
                Just nli ->
                    case lnItem nli of
                        AggregatedMessage _ _aggregated -> do
                            selectedBackends <- getBackends config (lnName nli)
                            let dropAggrBackends = filter (/= AggregationBK) selectedBackends
                            forM_ backends ( \(bek, be) ->
                                when (bek `elem` dropAggrBackends) (dispatch nli be) )
                            qProc
                        _ -> do
                            selectedBackends <- getBackends config (lnName nli)
                            forM_ backends ( \(bek, be) ->
                                when (bek `elem` selectedBackends) (dispatch nli be) )
                            qProc
                -- if Nothing is in the queue then every backend needs to be terminated
                -- and the dispatcher exits, the |Switchboard| stops.
                Nothing ->
                    forM_ backends $ \(_, backend) ->
                        bTerminate backend
        dispatch :: NamedLogItem -> Backend -> IO ()
        dispatch nli backend = (bPass backend) nli

    setupBackends :: Configuration
                  -> [(BackendKind,Backend)]
                  -> [BackendKind]
                  -> TBQ.TBQueue (Maybe NamedLogItem)
                  -> IO [(BackendKind,Backend)]
    setupBackends _ acc [] _ = return acc
    setupBackends c acc (bk : bes) q = do
        be' <- setupBackend' bk c q
        setupBackends c ((bk,be') : acc) bes q
    setupBackend' :: BackendKind -> Configuration -> TBQ.TBQueue (Maybe NamedLogItem) -> IO Backend
    setupBackend' EKGViewBK c _ = do
        be <- Cardano.BM.Output.EKGView.setup c
        return $ MkBackend
                    { bPass = Cardano.BM.Output.EKGView.pass be
                    , bTerminate = Cardano.BM.Output.EKGView.takedown be
                    }
    setupBackend' AggregationBK c q = do
        be <- Cardano.BM.Output.Aggregation.setup c q
        return $ MkBackend
                    { bPass = Cardano.BM.Output.Aggregation.pass be
                    , bTerminate = Cardano.BM.Output.Aggregation.takedown be
                    }
    setupBackend' KatipBK c _ = do
        be <- Cardano.BM.Output.Log.setup c
        return $ MkBackend
                    { bPass = Cardano.BM.Output.Log.pass be
                    , bTerminate = Cardano.BM.Output.Log.takedown be
                    }

\end{code}

\subsubsection{Process incoming messages}
Incoming messages are put into the queue, and
then processed by the dispatcher.

\begin{code}
instance HasPass Switchboard where
    pass switchboard item = do
        let writequeue :: TBQ.TBQueue (Maybe NamedLogItem) -> NamedLogItem -> IO ()
            writequeue q i =
                case lnItem i of
                    KillPill -> do
                        -- if KillPill received then kill all backends and
                        -- switchboard terminates.
                        d <- withMVar (getSB switchboard) (\sb ->
                                return (sbDispatch sb))
                        -- send terminating item to the queue
                        atomically $ TBQ.writeTBQueue q Nothing
                        -- wait for the dispatcher to exit
                        res <- Async.waitCatch d
                        either throwM return res
                    _  -> do
                        nocapacity <- atomically $ TBQ.isFullTBQueue q
                        if not nocapacity
                        then atomically $ TBQ.writeTBQueue q (Just i)
                        else return ()
        withMVar (getSB switchboard) $ \sb ->
            writequeue (sbQueue sb) item

\end{code}
