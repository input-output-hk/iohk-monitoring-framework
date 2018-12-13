
\subsection{Cardano.BM.Output.Aggregation}
\label{module:Cardano.BM.Output.Aggregation}

%if style == newcode
\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.Output.Aggregation
    (
      setup
    , pass
    , inspect
    , takedown
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, modifyMVar_, newEmptyMVar,
                     putMVar, readMVar, takeMVar, tryTakeMVar, withMVar)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import           Control.Monad (void)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)

import           Cardano.BM.Aggregated (Aggregated (..), updateAggregation)
import           Cardano.BM.Configuration (Configuration)
import           Cardano.BM.Data.LogItem

\end{code}
%endif

The aggregation is a singleton.
\begin{code}
type AggregationMVar = MVar AggregationInternal
newtype Aggregation = Aggregation
    { getAg :: AggregationMVar }

-- Our internal state
data AggregationInternal = AggregationInternal
    { agMap      :: HM.HashMap Text Aggregated
    , agQueue    :: TBQ.TBQueue (Maybe NamedLogItem)
    , agDispatch :: Async.Async ()
    }

\end{code}

\begin{code}
inspect :: Aggregation -> Text -> IO (Maybe Aggregated)
inspect agg name =
    withMVar (getAg agg) $ \ag ->
        return $ HM.lookup name (agMap ag)
\end{code}

\begin{code}
setup :: Configuration -> TBQ.TBQueue (Maybe NamedLogItem) -> IO Aggregation
setup _ switchboardQueue = do
    aggref <- newEmptyMVar
    aggregationQueue <- atomically $ TBQ.newTBQueue 2048
    dispatcher <- spawnDispatcher aggref aggregationQueue switchboardQueue
    putMVar aggref $ AggregationInternal HM.empty aggregationQueue dispatcher
    return $ Aggregation aggref

\end{code}

Pass the item to the Aggregation queue.
\begin{code}
pass :: Aggregation -> NamedLogItem -> IO ()
pass agg item = do
    ag <- readMVar (getAg agg) -- this will block. would it be better if the Queue was setup in the Switchboard
    -- so as to have it here as an argument?
    let aggregationQueue = agQueue ag
    atomically $ TBQ.writeTBQueue aggregationQueue $ Just item

spawnDispatcher :: AggregationMVar
                -> TBQ.TBQueue (Maybe NamedLogItem)
                -> TBQ.TBQueue (Maybe NamedLogItem)
                -> IO (Async.Async ())
spawnDispatcher agg aggregationQueue switchboardQueue = Async.async qProc
  where
    qProc = do
        maybeItem <- atomically $ TBQ.readTBQueue aggregationQueue
        case maybeItem of
            Just item -> do
                aggregation <- takeMVar agg
                let (updatedMap, maybeAggregatedMsg) =
                        update (lnItem item) (lnName item) (agMap aggregation)
                case maybeAggregatedMsg of
                    Just aggregatedMsg@(AggregatedMessage _ _) ->
                        -- forward the aggregated message to Switchboard
                        atomically $ TBQ.writeTBQueue switchboardQueue $
                            Just $ LogNamed
                                        { lnName = (lnName item) <> ".aggregated"
                                        , lnItem = aggregatedMsg
                                        }
                    _ -> return ()
                putMVar agg $ aggregation { agMap = updatedMap }
                qProc
            -- if Nothing is in the queue then every backend is terminated
            -- and Aggregation stops.
            Nothing -> return () -- maybe check the Queue for messages came after Nothing

    update :: LogObject
           -> LoggerName
           -> HM.HashMap Text Aggregated
           -> (HM.HashMap Text Aggregated, Maybe LogObject)
    update (LP (LogValue iname value)) logname agmap =
        let name = logname <> "." <> iname
            maybeAggregated = updateAggregation value $ HM.lookup name agmap
            aggregatedMessage = case maybeAggregated of
                                    Nothing ->
                                        Nothing
                                    Just aggregated ->
                                        Just $ AggregatedMessage iname aggregated
        in
        -- use of HM.alter so that in future we can clear the Agrregated
        -- by using as alter's arg a function which returns Nothing.
        (HM.alter (const $ maybeAggregated) name agmap, aggregatedMessage)
    -- TODO for text messages aggregate on delta of timestamps
    -- update (ObserveDiff counterState) logname agmap = undefined -- forM_ (csCounters counterState)
    update _ _ agmap = (agmap, Nothing)

\end{code}

\begin{code}
takedown :: Aggregation -> IO ()
takedown = clearMVar . getAg --TODO waitCatch dispatcher

clearMVar :: MVar a -> IO ()
clearMVar = void . tryTakeMVar

\end{code}
