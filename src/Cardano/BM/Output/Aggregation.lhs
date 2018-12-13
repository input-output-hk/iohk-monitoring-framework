
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
import           Control.Concurrent.MVar (MVar, newEmptyMVar,
                     putMVar, readMVar, takeMVar, tryTakeMVar, withMVar)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import           Control.Monad (void)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)

import           Cardano.BM.Aggregated (Aggregated (..), updateAggregation)
import           Cardano.BM.Configuration (Configuration)
import           Cardano.BM.Data.Counter (Counter (..), CounterState (..))
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
spawnDispatcher agg {-map-} aggregationQueue switchboardQueue = Async.async qProc
  where
    qProc {-map-} = do
        maybeItem <- atomically $ TBQ.readTBQueue aggregationQueue
        case maybeItem of
            Just item -> do
                aggregation <- takeMVar agg
                let (updatedMap, msgs) =
                        update (lnItem item) (lnName item) (agMap aggregation)
                putMVar agg $ aggregation { agMap = updatedMap }
                -- send Aggregated messages back to Switchboard
                sendAggregated msgs switchboardQueue (lnName item)
                qProc
            -- if Nothing is in the queue then every backend is terminated
            -- and Aggregation stops.
            Nothing -> return () -- maybe check the Queue for messages came after Nothing

    update :: LogObject
           -> LoggerName
           -> HM.HashMap Text Aggregated
           -> (HM.HashMap Text Aggregated, [LogObject])
    update (LP (LogValue iname value)) logname agmap =
        let name = logname <> "." <> iname
            maybeAggregated = updateAggregation value $ HM.lookup name agmap
            aggregatedMessage = case maybeAggregated of
                                    Nothing ->
                                        []
                                    Just aggregated ->
                                        [AggregatedMessage iname aggregated]
        in
        -- use of HM.alter so that in future we can clear the Agrregated
        -- by using as alter's arg a function which returns Nothing.
        (HM.alter (const $ maybeAggregated) name agmap, aggregatedMessage)
    update (ObserveDiff counterState) logname agmap =
        let
            counters = csCounters counterState
            (mapNew, msgs) = updateCounter counters logname agmap []
        in
            (mapNew, reverse msgs)
            -- (agmap, [])
    -- TODO for text messages aggregate on delta of timestamps
    update _ _ agmap = (agmap, [])

    updateCounter :: [Counter]
                  -> LoggerName
                  -> HM.HashMap Text Aggregated
                  -> [LogObject]
                  -> (HM.HashMap Text Aggregated, [LogObject])
    updateCounter [] _ aggrMap msgs = (aggrMap, msgs)
    updateCounter ((MonotonicClockTime name value) :cs) logname aggrMap msgs =
        let
            fullname = logname <> "." <> name
            maybeAggregated = updateAggregation (toInteger value) $ HM.lookup fullname aggrMap
            aggregatedMessage = case maybeAggregated of
                                    Nothing ->
                                        error "This should not have happened!"
                                    Just aggregated ->
                                        AggregatedMessage name aggregated
            updatedMap = HM.alter (const $ maybeAggregated) fullname aggrMap
        in
            updateCounter cs logname updatedMap (aggregatedMessage :msgs)
        -- updateCounter cs logname aggrMap msgs
    updateCounter ((MemoryCounter      name value) :cs) logname aggrMap msgs =
        let
            fullname = logname <> "." <> name
            maybeAggregated = updateAggregation value $ HM.lookup fullname aggrMap
            aggregatedMessage = case maybeAggregated of
                                    Nothing ->
                                        error "This should not have happened!"
                                    Just aggregated ->
                                        AggregatedMessage name aggregated
            updatedMap = HM.alter (const $ maybeAggregated) fullname aggrMap
        in
            updateCounter cs logname updatedMap (aggregatedMessage :msgs)
    updateCounter ((StatInfo           name value) :cs) logname aggrMap msgs =
        let
            fullname = logname <> "." <> name
            maybeAggregated = updateAggregation value $ HM.lookup fullname aggrMap
            aggregatedMessage = case maybeAggregated of
                                    Nothing ->
                                        error "This should not have happened!"
                                    Just aggregated ->
                                        AggregatedMessage name aggregated
            updatedMap = HM.alter (const $ maybeAggregated) fullname aggrMap
        in
            updateCounter cs logname updatedMap (aggregatedMessage :msgs)
    updateCounter ((IOCounter          name value) :cs) logname aggrMap msgs =
        let
            fullname = logname <> "." <> name
            maybeAggregated = updateAggregation value $ HM.lookup fullname aggrMap
            aggregatedMessage = case maybeAggregated of
                                    Nothing ->
                                        error "This should not have happened!"
                                    Just aggregated ->
                                        AggregatedMessage name aggregated
            updatedMap = HM.alter (const $ maybeAggregated) fullname aggrMap
        in
            updateCounter cs logname updatedMap (aggregatedMessage :msgs)
    updateCounter ((CpuCounter         name value) :cs) logname aggrMap msgs =
        let
            fullname = logname <> "." <> name
            maybeAggregated = updateAggregation value $ HM.lookup fullname aggrMap
            aggregatedMessage = case maybeAggregated of
                                    Nothing ->
                                        error "This should not have happened!"
                                    Just aggregated ->
                                        AggregatedMessage name aggregated
            updatedMap = HM.alter (const $ maybeAggregated) fullname aggrMap
        in
            updateCounter cs logname updatedMap (aggregatedMessage :msgs)

    sendAggregated :: [LogObject] -> TBQ.TBQueue (Maybe NamedLogItem) -> Text -> IO ()
    sendAggregated [] _ _ = return ()
    sendAggregated (aggregatedMsg@(AggregatedMessage _ _) : ms) sbQueue logname = do
        -- forward the aggregated message to Switchboard
        atomically $ TBQ.writeTBQueue sbQueue $
            Just $ LogNamed
                        { lnName = logname <> ".aggregated"
                        , lnItem = aggregatedMsg
                        }
        sendAggregated ms sbQueue logname
    -- ingnore messages that are not of type AggregatedMessage
    sendAggregated (_ : ms) sbQueue logname =
        sendAggregated ms sbQueue logname


\end{code}

\begin{code}
takedown :: Aggregation -> IO ()
takedown = clearMVar . getAg --TODO waitCatch dispatcher

clearMVar :: MVar a -> IO ()
clearMVar = void . tryTakeMVar

\end{code}
