
\subsection{Cardano.BM.Backend.Aggregation}
\label{code:Cardano.BM.Backend.Aggregation}

%if style == newcode
\begin{code}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-@ embed GHC.Natural.Natural as int @-}

module Cardano.BM.Backend.Aggregation
    (
      Aggregation
    , effectuate
    , realizefrom
    , updateAggregation
    , unrealize
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar,
                     modifyMVar_, putMVar, readMVar, tryTakeMVar, withMVar)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import           Control.Exception.Safe (throwM)
import           Control.Monad (unless, void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (FromJSON)
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
                     EWMA (..), Measurable (..), Stats (..), getDouble,
                     getInteger, singletonStats, subtractMeasurable)
import           Cardano.BM.Data.AggregatedKind (AggregatedKind (..))
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.Counter (Counter (..), CounterState (..),
                     nameCounter)
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.MessageCounter (resetCounters, sendAndResetAfter,
                     updateMessageCounters)
import           Cardano.BM.Data.Severity (Severity (..))
import qualified Cardano.BM.Trace as Trace

\end{code}
%endif

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
    typeof _ = AggregationBK

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
        let clearMVar :: MVar a -> IO ()
            clearMVar = void . tryTakeMVar

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
            Just a -> return $ updateAggregation value (aeAggregated a) lme (aeResetAfter a)

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

\subsubsection{Update aggregation}\label{code:updateAggregation}\index{updateAggregation}
We distinguish an unitialized from an already initialized aggregation. The latter is properly initialized.
\\
We use Welford's online algorithm to update the estimation of mean and variance of the sample statistics.
(see \url{https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Welford's_Online_algorithm})

\begin{code}
updateAggregation :: Measurable -> Aggregated -> LOMeta -> Maybe Word64 -> Either Text Aggregated
updateAggregation v (AggregatedStats s) lme resetAfter =
    let count = fcount (fbasic s)
        reset = maybe False (count >=) resetAfter
    in
    if reset
    then
        Right $ singletonStats v
    else
        Right $ AggregatedStats $! Stats { flast  = v
                                         , fold = mkTimestamp
                                         , fbasic = updateBaseStats 1 v (fbasic s)
                                         , fdelta = updateBaseStats 2 deltav (fdelta s)
                                         , ftimed = updateBaseStats 2 timediff (ftimed s)
                                         }
  where
    deltav = subtractMeasurable v (flast s)
    mkTimestamp = Nanoseconds $ utc2ns (tstamp lme)
    timediff = Nanoseconds $ fromInteger $ (getInteger mkTimestamp) - (getInteger $ fold s)

updateAggregation v (AggregatedEWMA e) _ _ =
    let !eitherAvg = ewma e v
    in
        AggregatedEWMA <$> eitherAvg

updateBaseStats :: Word64 -> Measurable -> BaseStats -> BaseStats
updateBaseStats startAt v s =
    let newcount = fcount s + 1 in
    if (startAt > newcount)
    then s {fcount = fcount s + 1}
    else
        let newcountRel = newcount - startAt + 1
            newvalue = getDouble v
            delta = newvalue - fsum_A s
            dincr = (delta / fromIntegral newcountRel)
            delta2 = newvalue - fsum_A s - dincr
            (minim, maxim) =
                if startAt == newcount
                then (v, v)
                else (min v (fmin s), max v (fmax s))
        in
        BaseStats { fmin   = minim
                  , fmax   = maxim
                  , fcount = newcount
                  , fsum_A = fsum_A s + dincr
                  , fsum_B = fsum_B s + (delta * delta2)
                  }

\end{code}

\subsubsection{Calculation of EWMA}\label{code:ewma}\index{ewma}
Following \url{https://en.wikipedia.org/wiki/Moving_average#Exponential_moving_average} we calculate
the exponential moving average for a series of values $ Y_t $ according to:

$$
S_t =
\begin{cases}
  Y_1,       & t = 1 \\
  \alpha \cdot Y_t + (1 - \alpha) \cdot S_{t-1},    & t > 1
\end{cases}
$$
\\
The pattern matching below ensures that the |EWMA| will start with the first value passed in,
and will not change type, once determined.
\begin{code}
ewma :: EWMA -> Measurable -> Either Text EWMA
ewma (EmptyEWMA a) v = Right $ EWMA a v
ewma (EWMA a s@(Microseconds _)) y@(Microseconds _) =
    Right $ EWMA a $ Microseconds $ round $ a * (getDouble y) + (1 - a) * (getDouble s)
ewma (EWMA a s@(Seconds _)) y@(Seconds _) =
    Right $ EWMA a $ Seconds $ round $ a * (getDouble y) + (1 - a) * (getDouble s)
ewma (EWMA a s@(Bytes _)) y@(Bytes _) =
    Right $ EWMA a $ Bytes $ round $ a * (getDouble y) + (1 - a) * (getDouble s)
ewma (EWMA a (PureI s)) (PureI y) =
    Right $ EWMA a $ PureI $ round $ a * (fromInteger y) + (1 - a) * (fromInteger s)
ewma (EWMA a (PureD s)) (PureD y) =
    Right $ EWMA a $ PureD $ a * y + (1 - a) * s
ewma _ _ = Left "EWMA: Cannot compute average on values of different types"

\end{code}
