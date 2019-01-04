
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
import           Control.Monad (unless, void)
import           Control.Monad.Catch (throwM)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text, stripSuffix)

import           Cardano.BM.Data.Aggregated (Aggregated (..), Measurable(..), updateAggregation, EWMA (..))
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
                let (updatedMap, aggregations) =
                        update (lnItem item) (lnName item) aggregatedMap
                unless (null aggregations) $
                    sendAggregated (AggregatedMessage aggregations) switchboard (lnName item)
                qProc updatedMap
            Nothing -> return ()

    update :: LogObject
           -> LoggerName
           -> HM.HashMap Text Aggregated
           -> (HM.HashMap Text Aggregated, [(Text, Aggregated)])
    update (LP (LogValue iname value)) logname agmap =
        let name = logname <> "." <> iname
            maybeAggregated = updateAggregation (Pure value) $ HM.lookup name agmap
            aggregatedMessage = case maybeAggregated of
                                    Nothing ->
                                        []
                                    Just aggregated ->
                                        [(iname, aggregated)] -- Is there a need for list??
        in
        -- use of HM.alter so that in future we can clear the Agrregated
        -- by using as alter's arg a function which returns Nothing.
        (HM.alter (const $ maybeAggregated) name agmap, aggregatedMessage)
    update (ObserveDiff counterState) logname agmap =
        let
            counters = csCounters counterState
            (mapNew, aggs) = updateCounter counters logname agmap []
        in
            (mapNew, reverse aggs)
    -- remove |Aggregated| of Time for the name given
    update (ResetTimeAggregation name) _ agmap =
            let k = case stripSuffix "aggregated" name of
                        Just n  -> n
                        Nothing -> ""
            in
            (HM.delete (k <> "monoclock") agmap, [])

    -- TODO for text messages aggregate on delta of timestamps
    update _ _ agmap = (agmap, [])

    updateCounter :: [Counter]
                  -> LoggerName
                  -> HM.HashMap Text Aggregated
                  -> [(Text, Aggregated)]
                  -> (HM.HashMap Text Aggregated, [(Text, Aggregated)])
    updateCounter [] _ aggrMap aggs = (aggrMap, aggs)
    updateCounter (counter : cs) logname aggrMap aggs =
        let
            name = cName counter
            fullname = logname <> "." <> name
            maybeAggregated = updateAggregation (cValue counter) $ HM.lookup fullname aggrMap
            namedAggregated = case maybeAggregated of
                                    Nothing ->
                                        error "This should not have happened!"
                                    Just aggregated ->
                                        (((nameCounter counter) <> "." <> name), aggregated)
            updatedMap = HM.alter (const $ maybeAggregated) fullname aggrMap
            -- ewma
            maybeAggregatedEWMA =
                case HM.lookup (fullname <> ".ewma") updatedMap of
                    Nothing ->
                        Just $ AggregatedEWMA $ EWMA 0.75 (cValue counter)
                    agg@(Just (AggregatedEWMA _)) ->
                        updateAggregation (cValue counter) agg
                    _ -> Nothing
            namedAggregatedEWMA =
                case maybeAggregatedEWMA of
                    Nothing ->
                        error "This should not have happened!"
                    Just aggregatedEWMA ->
                        (((nameCounter counter) <> "." <> name <> ".ewma"), aggregatedEWMA)
            updatedMap' = HM.alter (const $ maybeAggregatedEWMA) (fullname <> ".ewma") updatedMap
        in
            updateCounter cs logname updatedMap' (namedAggregated: namedAggregatedEWMA :aggs)

    sendAggregated :: IsEffectuator e => LogObject -> e -> Text -> IO ()
    sendAggregated (aggregatedMsg@(AggregatedMessage _)) sb logname =
        -- forward the aggregated message to Switchboard
        effectuate sb $
                    LogNamed
                        { lnName = logname <> ".aggregated"
                        , lnItem = aggregatedMsg
                        }
    -- ingnore every other message that is not of type AggregatedMessage
    sendAggregated _ _ _ = return ()

\end{code}
