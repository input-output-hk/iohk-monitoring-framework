
\subsection{Cardano.BM.Backend.Aggregation}
\label{code:Cardano.BM.Backend.Aggregation}

%if style == newcode
\begin{code}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-@ embed GHC.Natural.Natural as int @-}

module Cardano.BM.Backend.Aggregation
    (
      Aggregation
    , effectuate
    , realizefrom
    , unrealize
    -- * Plugin
    , plugin
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar,
                     modifyMVar_, putMVar, readMVar, tryTakeMVar, withMVar)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import           Control.Exception.Safe (throwM)
import           Control.Monad (unless, void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import           Data.Time.Clock (getCurrentTime)
import           Data.Word (Word64)
import           GHC.Clock (getMonotonicTimeNSec)
import           System.IO (stderr)

import           Cardano.BM.Backend.ProcessQueue (processQueue)
import           Cardano.BM.Configuration.Model (Configuration, getAggregatedKind)
import           Cardano.BM.Data.Aggregated (Aggregated (..), BaseStats (..),
                     EWMA (..), Measurable (..), Stats (..), ewma, getDouble,
                     getInteger, singletonStats, subtractMeasurable,
                     updateAggregation)
import           Cardano.BM.Data.AggregatedKind (AggregatedKind (..))
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.Counter (Counter (..), CounterState (..),
                     nameCounter)
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.MessageCounter (resetCounters, sendAndResetAfter,
                     updateMessageCounters)
import           Cardano.BM.Data.Severity (Severity (..))
import           Cardano.BM.Plugin
import qualified Cardano.BM.Trace as Trace

\end{code}
%endif

\subsubsection{Plugin definition}
\begin{code}
plugin :: (IsEffectuator s a, ToJSON a, FromJSON a)
       => Configuration -> Trace.Trace IO a -> s a -> IO (Plugin a)
plugin config trace sb = do
    be :: Cardano.BM.Backend.Aggregation.Aggregation a <- realizefrom config trace sb
    return $ BackendPlugin
               (MkBackend { bEffectuate = effectuate be, bUnrealize = unrealize be })
               (bekind be)
\end{code}

\subsubsection{Internal representation}\label{code:Aggregation}\index{Aggregation}
\begin{code}
type AggregationMVar a = MVar (AggregationInternal a)
newtype Aggregation a = Aggregation
    { getAg :: AggregationMVar a }

data AggregationInternal a = AggregationInternal
    { agQueue    :: TBQ.TBQueue (Maybe (LogObject a))
    , agDispatch :: Async.Async ()
    }

\end{code}

\subsubsection{Relation from context name to aggregated statistics}
We keep the aggregated values (|Aggregated|) for a named context in a |HashMap|.
\begin{code}
type AggregationMap = HM.HashMap Text AggregatedExpanded

\end{code}

\subsubsection{Info for Aggregated operations}\label{code:AggregatedExpanded}\index{AggregatedExpanded}
Apart from the |Aggregated| we keep some valuable info regarding to them; such as when
was the last time it was sent.
\begin{code}
type Timestamp = Word64

data AggregatedExpanded = AggregatedExpanded
                            { aeAggregated :: !Aggregated
                            , aeResetAfter :: !(Maybe Word64)
                            , aeLastSent   :: {-# UNPACK #-} !Timestamp
                            }

\end{code}

\subsubsection{|Aggregation| implements |effectuate|}\index{Aggregation!instance of IsEffectuator}

|Aggregation| is an |IsEffectuator|
Enter the log item into the |Aggregation| queue.
\begin{code}
instance IsEffectuator Aggregation a where
    effectuate agg item = do
        ag <- readMVar (getAg agg)
        nocapacity <- atomically $ TBQ.isFullTBQueue (agQueue ag)
        if nocapacity
        then handleOverflow agg
        else atomically $ TBQ.writeTBQueue (agQueue ag) $! Just item

    handleOverflow _ = TIO.hPutStrLn stderr "Notice: Aggregation's queue full, dropping log items!"
\end{code}

\subsubsection{|Aggregation| implements |Backend| functions}\index{Aggregation!instance of IsBackend}

|Aggregation| is an |IsBackend|
\begin{code}
instance FromJSON a => IsBackend Aggregation a where
    bekind _ = AggregationBK

    realize _ = fail "Aggregation cannot be instantiated by 'realize'"

    realizefrom config trace _ = do
        aggref <- newEmptyMVar
#ifdef PERFORMANCE_TEST_QUEUE
        let qSize = 1000000
#else
        let qSize = 2048
#endif
        aggregationQueue <- atomically $ TBQ.newTBQueue qSize
        dispatcher <- spawnDispatcher config HM.empty aggregationQueue trace
        -- link the given Async to the current thread, such that if the Async
        -- raises an exception, that exception will be re-thrown in the current
        -- thread, wrapped in ExceptionInLinkedThread.
        Async.link dispatcher
        putMVar aggref $ AggregationInternal aggregationQueue dispatcher
        return $ Aggregation aggref

    unrealize aggregation = do
        let clearMVar = void . tryTakeMVar

        (dispatcher, queue) <- withMVar (getAg aggregation) (\ag ->
                            return (agDispatch ag, agQueue ag))
        -- send terminating item to the queue
        atomically $ TBQ.writeTBQueue queue Nothing
        -- wait for the dispatcher to exit
        -- TODO add a timeout to waitCatch in order
        -- to be sure that it will finish
        res <- Async.waitCatch dispatcher
        either throwM return res
        (clearMVar . getAg) aggregation

\end{code}

\subsubsection{Asynchronously reading log items from the queue and their processing}
\begin{code}
spawnDispatcher :: Configuration
                -> AggregationMap
                -> TBQ.TBQueue (Maybe (LogObject a))
                -> Trace.Trace IO a
                -> IO (Async.Async ())
spawnDispatcher conf aggMap aggregationQueue basetrace = do
    now <- getCurrentTime
    let trace = Trace.appendName "#aggregation" basetrace
    let messageCounters = resetCounters now
    countersMVar <- newMVar messageCounters
    _timer <- Async.async $ sendAndResetAfter
                                basetrace
                                "#messagecounters.aggregation"
                                countersMVar
                                60000   -- 60000 ms = 1 min
                                Debug

    Async.async $ qProc trace countersMVar aggMap
  where
    {-@ lazy qProc @-}
    qProc trace counters aggregatedMap = do
        processQueue
            aggregationQueue
            processAggregated
            (trace, counters, aggregatedMap)
            (\_ -> pure ())

    processAggregated lo@(LogObject logname lm _) (trace, counters, aggregatedMap) = do
        (updatedMap, aggregations) <- update lo aggregatedMap trace
        unless (null aggregations) $
            sendAggregated trace (LogObject logname lm (AggregatedMessage aggregations))
        -- increase the counter for the specific severity and message type
        modifyMVar_ counters $ \cnt -> return $ updateMessageCounters cnt lo
        return (trace, counters, updatedMap)

    createNupdate :: Text -> Measurable -> LOMeta -> AggregationMap -> IO (Either Text Aggregated)
    createNupdate name value lme agmap = do
        case HM.lookup name agmap of
            Nothing -> do
                -- if Aggregated does not exist; initialize it.
                aggregatedKind <- getAggregatedKind conf name
                case aggregatedKind of
                    StatsAK      -> return $ Right $ singletonStats value
                    EwmaAK aEWMA -> do
                        let initEWMA = EmptyEWMA aEWMA
                        return $ AggregatedEWMA <$> ewma initEWMA value
            Just a -> return $ updateAggregation value (aeAggregated a) (utc2ns $ tstamp lme) (aeResetAfter a)

    update :: LogObject a
           -> AggregationMap
            -> Trace.Trace IO a
           -> IO (AggregationMap, [(Text, Aggregated)])
    update (LogObject logname lme (LogValue iname value)) agmap trace = do
        let fullname = logname <> "." <> iname
        eitherAggregated <- createNupdate fullname value lme agmap
        case eitherAggregated of
            Right aggregated -> do
                now <- getMonotonicTimeNSec
                let aggregatedX = AggregatedExpanded {
                                    aeAggregated = aggregated
                                  , aeResetAfter = Nothing
                                  , aeLastSent = now
                                  }
                    namedAggregated = [(iname, aeAggregated aggregatedX)]
                    updatedMap = HM.alter (const $ Just $ aggregatedX) fullname agmap
                return (updatedMap, namedAggregated)
            Left w -> do
                let trace' = Trace.appendName "update" trace
                Trace.traceNamedObject trace' =<<
                    (,) <$> liftIO (mkLOMeta Warning Public)
                        <*> pure (LogError w)
                return (agmap, [])

    update (LogObject logname lme (ObserveDiff counterState)) agmap trace =
        updateCounters (csCounters counterState) lme (logname, "diff") agmap [] trace
    update (LogObject logname lme (ObserveOpen counterState)) agmap trace =
        updateCounters (csCounters counterState) lme (logname, "open") agmap [] trace
    update (LogObject logname lme (ObserveClose counterState)) agmap trace =
        updateCounters (csCounters counterState) lme (logname, "close") agmap [] trace

    update (LogObject logname lme (LogMessage _)) agmap trace = do
        let iname  = pack $ show (severity lme)
        let fullname = logname <> "." <> iname
        eitherAggregated <- createNupdate fullname (PureI 0) lme agmap
        case eitherAggregated of
            Right aggregated -> do
                now <- getMonotonicTimeNSec
                let aggregatedX = AggregatedExpanded {
                                    aeAggregated = aggregated
                                  , aeResetAfter = Nothing
                                  , aeLastSent = now
                                  }
                    namedAggregated = [(iname, aeAggregated aggregatedX)]
                    updatedMap = HM.alter (const $ Just $ aggregatedX) fullname agmap
                return (updatedMap, namedAggregated)
            Left w -> do
                let trace' = Trace.appendName "update" trace
                Trace.traceNamedObject trace' =<<
                    (,) <$> liftIO (mkLOMeta Warning Public)
                        <*> pure (LogError w)
                return (agmap, [])

    -- everything else
    update _ agmap _ = return (agmap, [])

    updateCounters :: [Counter]
                   -> LOMeta
                   -> (LoggerName,LoggerName)
                   -> AggregationMap
                   -> [(Text, Aggregated)]
                   -> Trace.Trace IO a
                   -> IO (AggregationMap, [(Text, Aggregated)])
    updateCounters [] _ _ aggrMap aggs _ = return $ (aggrMap, aggs)
    updateCounters (counter : cs) lme (logname, msgname) aggrMap aggs trace = do
        let name = cName counter
            subname = msgname <> "." <> (nameCounter counter) <> "." <> name
            fullname = logname <> "." <> subname
            value = cValue counter
        eitherAggregated <- createNupdate fullname value lme aggrMap
        case eitherAggregated of
            Right aggregated -> do
                now <- getMonotonicTimeNSec
                let aggregatedX = AggregatedExpanded {
                                    aeAggregated = aggregated
                                  , aeResetAfter = Nothing
                                  , aeLastSent = now
                                  }
                    namedAggregated = (subname, aggregated)
                    updatedMap = HM.alter (const $ Just $ aggregatedX) fullname aggrMap
                updateCounters cs lme (logname, msgname) updatedMap (namedAggregated : aggs) trace
            Left w -> do
                let trace' = Trace.appendName "updateCounters" trace
                Trace.traceNamedObject trace' =<<
                    (,) <$> liftIO (mkLOMeta Warning Public)
                        <*> pure (LogError w)
                updateCounters cs lme (logname, msgname) aggrMap aggs trace

    sendAggregated :: Trace.Trace IO a -> LogObject a -> IO ()
    sendAggregated trace (LogObject logname meta v@(AggregatedMessage _)) = do
        -- enter the aggregated message into the |Trace|
        let trace' = Trace.appendName logname trace
        liftIO $ Trace.traceNamedObject trace' (meta, v)
    -- ingnore every other message
    sendAggregated _ _ = return ()

\end{code}
