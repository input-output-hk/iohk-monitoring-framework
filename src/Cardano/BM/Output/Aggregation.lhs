
\subsection{Cardano.BM.Output.Aggregation}
\label{module:Cardano.BM.Output.Aggregation}

%if style == newcode
\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.Output.Aggregation
    (
      Aggregation
    , effectuate
    , realizefrom
    , unrealize
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, newEmptyMVar,
                     putMVar, readMVar, tryTakeMVar, withMVar)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import           Control.Monad (void)
import           Control.Monad.Catch (throwM)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)

import           Cardano.BM.Data.Aggregated (Aggregated (..), updateAggregation)
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.Counter (Counter (..), CounterState (..), nameCounter)
import           Cardano.BM.Data.LogItem

\end{code}
%endif

\subsubsection{Internal representation}\label{code:Aggregation}
\begin{code}
type AggregationMVar = MVar AggregationInternal
newtype Aggregation = Aggregation
    { getAg :: AggregationMVar }

data AggregationInternal = AggregationInternal
    { agQueue    :: TBQ.TBQueue (Maybe NamedLogItem)
    , agDispatch :: Async.Async ()
    }

\end{code}

\subsubsection{Relation from context name to aggregated statistics}
We keep the aggregated values (\nameref{code:Aggregated}) for a named context in a |HashMap|.
\begin{code}
type AggregationMap = HM.HashMap Text Aggregated

\end{code}

\subsubsection{|Aggregation| implements |effectuate|}

|Aggregation| is an \nameref{code:IsEffectuator}
Enter the log item into the |Aggregation| queue.
\begin{code}
instance IsEffectuator Aggregation where
    effectuate agg item = do
        ag <- readMVar (getAg agg)
        atomically $ TBQ.writeTBQueue (agQueue ag) $ Just item

\end{code}

\subsubsection{|Aggregation| implements |Backend| functions}

|Aggregation| is an \nameref{code:IsBackend}
\begin{code}
instance IsBackend Aggregation where
    typeof _ = AggregationBK

    realize _ = error "Aggregation cannot be instantiated by 'realize'"

    realizefrom _ switchboard = do
        aggref <- newEmptyMVar
        aggregationQueue <- atomically $ TBQ.newTBQueue 2048
        dispatcher <- spawnDispatcher HM.empty aggregationQueue switchboard
        putMVar aggref $ AggregationInternal aggregationQueue dispatcher
        return $ Aggregation aggref

    unrealize aggregation = do
        let clearMVar :: MVar a -> IO ()
            clearMVar = void . tryTakeMVar

        (dispatcher, queue) <- withMVar (getAg aggregation) (\ag ->
                            return (agDispatch ag, agQueue ag))
        -- send terminating item to the queue
        atomically $ TBQ.writeTBQueue queue Nothing
        -- wait for the dispatcher to exit
        res <- Async.waitCatch dispatcher
        either throwM return res
        (clearMVar . getAg) aggregation

\end{code}

\subsubsection{Asynchrouniously reading log items from the queue and their processing}
\begin{code}
spawnDispatcher :: IsEffectuator e
                => AggregationMap
                -> TBQ.TBQueue (Maybe NamedLogItem)
                -> e
                -> IO (Async.Async ())
spawnDispatcher aggMap aggregationQueue switchboard = Async.async $ qProc aggMap
  where
    qProc aggregatedMap = do
        maybeItem <- atomically $ TBQ.readTBQueue aggregationQueue
        case maybeItem of
            Just item -> do
                let (updatedMap, msgs) =
                        update (lnItem item) (lnName item) aggregatedMap
                sendAggregated msgs switchboard (lnName item)
                qProc updatedMap
            Nothing -> return ()

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
    -- TODO for text messages aggregate on delta of timestamps
    update _ _ agmap = (agmap, [])

    updateCounter :: [Counter]
                  -> LoggerName
                  -> HM.HashMap Text Aggregated
                  -> [LogObject]
                  -> (HM.HashMap Text Aggregated, [LogObject])
    updateCounter [] _ aggrMap msgs = (aggrMap, msgs)
    updateCounter (counter : cs) logname aggrMap msgs =
        let
            name = cName counter
            fullname = logname <> "." <> name
            maybeAggregated = updateAggregation (cValue counter) $ HM.lookup fullname aggrMap
            aggregatedMessage = case maybeAggregated of
                                    Nothing ->
                                        error "This should not have happened!"
                                    Just aggregated ->
                                        AggregatedMessage ((nameCounter counter) <> "." <> name) aggregated
            updatedMap = HM.alter (const $ maybeAggregated) fullname aggrMap
        in
            updateCounter cs logname updatedMap (aggregatedMessage :msgs)

    sendAggregated :: IsEffectuator e => [LogObject] -> e -> Text -> IO ()
    sendAggregated [] _ _ = return ()
    sendAggregated (aggregatedMsg@(AggregatedMessage _ _) : ms) sb logname = do
        -- forward the aggregated message to Switchboard
        effectuate sb $
                    LogNamed
                        { lnName = logname <> ".aggregated"
                        , lnItem = aggregatedMsg
                        }
        sendAggregated ms sb logname
    -- ingnore all other messages that are not of type AggregatedMessage
    sendAggregated (_ : ms) sb logname =
        sendAggregated ms sb logname


\end{code}
