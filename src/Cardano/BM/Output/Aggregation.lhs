
\subsection{Cardano.BM.Output.Aggregation}
\label{code:Cardano.BM.Output.Aggregation}

%if style == newcode
\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.Output.Aggregation
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
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text, pack)
import           Data.Time.Calendar (toModifiedJulianDay)
import           Data.Time.Clock (UTCTime (..), getCurrentTime)
import           Data.Word (Word64)
import           GHC.Clock (getMonotonicTimeNSec)

import           Cardano.BM.Configuration.Model (Configuration, getAggregatedKind)
import           Cardano.BM.Data.Aggregated (Aggregated (..), BaseStats (..),
                     EWMA (..), Measurable (..), Stats (..), getDouble,
                     singletonStats)
import           Cardano.BM.Data.AggregatedKind (AggregatedKind (..))
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.Counter (Counter (..), CounterState (..),
                     nameCounter)
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.MessageCounter (resetCounters, sendAndResetAfter,
                     updateMessageCounters)
import           Cardano.BM.Data.Severity (Severity (..))
import           Cardano.BM.Data.Trace
import qualified Cardano.BM.Trace as Trace

\end{code}
%endif

\subsubsection{Internal representation}\label{code:Aggregation}\index{Aggregation}
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
type AggregationMap = HM.HashMap Text AggregatedExpanded

\end{code}

\subsubsection{Info for Aggregated operations}\label{code:AggregatedExpanded}\index{AggregatedExpanded}
Apart from the |Aggregated| we keep some valuable info regarding to them; such as when
was the last time it was sent.
\begin{code}
type Timestamp = Word64

data AggregatedExpanded = AggregatedExpanded
                            { aeAggregated :: !Aggregated
                            , aeResetAfter :: !(Maybe Int)
                            , aeLastSent   :: {-# UNPACK #-} !Timestamp
                            }

\end{code}

\subsubsection{|Aggregation| implements |effectuate|}\index{Aggregation!instance of IsEffectuator}

|Aggregation| is an |IsEffectuator|
Enter the log item into the |Aggregation| queue.
\begin{code}
instance IsEffectuator Aggregation where
    effectuate agg item = do
        ag <- readMVar (getAg agg)
        nocapacity <- atomically $ TBQ.isFullTBQueue (agQueue ag)
        if nocapacity
        then handleOverflow agg
        else atomically $ TBQ.writeTBQueue (agQueue ag) $! Just item

    handleOverflow _ = putStrLn "Notice: Aggregation's queue full, dropping log items!"
\end{code}

\subsubsection{|Aggregation| implements |Backend| functions}\index{Aggregation!instance of IsBackend}

|Aggregation| is an |IsBackend|
\begin{code}
instance IsBackend Aggregation where
    typeof _ = AggregationBK

    realize _ = error "Aggregation cannot be instantiated by 'realize'"

    realizefrom trace0@(ctx,_) _ = do
        trace <- Trace.subTrace "#aggregation" trace0
        aggref <- newEmptyMVar
        aggregationQueue <- atomically $ TBQ.newTBQueue 2048
        dispatcher <- spawnDispatcher (configuration ctx) HM.empty aggregationQueue trace trace0
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
                -> TBQ.TBQueue (Maybe NamedLogItem)
                -> Trace.Trace IO
                -> Trace.Trace IO
                -> IO (Async.Async ())
spawnDispatcher conf aggMap aggregationQueue trace trace0 = do
    now <- getCurrentTime
    let messageCounters = resetCounters now
    countersMVar <- newMVar messageCounters
    _timer <- Async.async $ sendAndResetAfter
                                trace0
                                "#messagecounters.aggregation"
                                countersMVar
                                60000   -- 60000 ms = 1 min
                                Warning -- Debug

    Async.async $ qProc countersMVar aggMap
  where
    qProc counters aggregatedMap = do
        maybeItem <- atomically $ TBQ.readTBQueue aggregationQueue
        case maybeItem of
            Just (LogNamed logname lo@(LogObject lm _)) -> do
                (updatedMap, aggregations) <- update lo logname aggregatedMap
                unless (null aggregations) $
                    sendAggregated (LogObject lm (AggregatedMessage aggregations)) logname
                -- increase the counter for the specific severity and message type
                modifyMVar_ counters $ \cnt -> return $ updateMessageCounters cnt lo
                qProc counters updatedMap
            Nothing -> return ()

    createNupdate name value lme agmap = do
        case HM.lookup name agmap of
            Nothing -> do
                -- if Aggregated does not exist; initialize it.
                aggregatedKind <- getAggregatedKind conf name
                case aggregatedKind of
                    StatsAK      -> return $ singletonStats value
                    EwmaAK aEWMA -> do
                        let initEWMA = EmptyEWMA aEWMA
                        return $ AggregatedEWMA $ ewma initEWMA value
            Just a -> return $ updateAggregation value (aeAggregated a) lme (aeResetAfter a)

    update :: LogObject
           -> LoggerName
           -> AggregationMap
           -> IO (AggregationMap, [(Text, Aggregated)])
    update (LogObject lme (LogValue iname value)) logname agmap = do
        let fullname = logname <> "." <> iname
        aggregated <- createNupdate fullname value lme agmap
        now <- getMonotonicTimeNSec
        let aggregatedX = AggregatedExpanded {
                            aeAggregated = aggregated
                          , aeResetAfter = Nothing
                          , aeLastSent = now
                          }
            namedAggregated = [(iname, aeAggregated aggregatedX)]
            updatedMap = HM.alter (const $ Just $ aggregatedX) fullname agmap
        return (updatedMap, namedAggregated)

    update (LogObject lme (ObserveDiff counterState)) logname agmap =
        updateCounters (csCounters counterState) lme (logname, "diff") agmap []
    update (LogObject lme (ObserveOpen counterState)) logname agmap =
        updateCounters (csCounters counterState) lme (logname, "open") agmap []
    update (LogObject lme (ObserveClose counterState)) logname agmap =
        updateCounters (csCounters counterState) lme (logname, "close") agmap []

    update (LogObject lme (LogMessage _)) logname agmap = do
        let iname  = pack $ show (severity lme)
        let fullname = logname <> "." <> iname
        aggregated <- createNupdate fullname (PureI 0) lme agmap
        now <- getMonotonicTimeNSec
        let aggregatedX = AggregatedExpanded {
                            aeAggregated = aggregated
                          , aeResetAfter = Nothing
                          , aeLastSent = now
                          }
            namedAggregated = [(iname, aeAggregated aggregatedX)]
            updatedMap = HM.alter (const $ Just $ aggregatedX) fullname agmap
        return (updatedMap, namedAggregated)

    -- everything else
    update _ _ agmap = return (agmap, [])

    updateCounters :: [Counter]
                   -> LOMeta
                   -> (LoggerName,LoggerName)
                   -> AggregationMap
                   -> [(Text, Aggregated)]
                   -> IO (AggregationMap, [(Text, Aggregated)])
    updateCounters [] _ _ aggrMap aggs = return $ (aggrMap, aggs)
    updateCounters (counter : cs) lme (logname, msgname) aggrMap aggs = do
        let name = cName counter
            subname = msgname <> "." <> (nameCounter counter) <> "." <> name
            fullname = logname <> "." <> subname
            value = cValue counter
        aggregated <- createNupdate fullname value lme aggrMap
        now <- getMonotonicTimeNSec
        let aggregatedX = AggregatedExpanded {
                            aeAggregated = aggregated
                          , aeResetAfter = Nothing
                          , aeLastSent = now
                          }
            namedAggregated = (subname, aggregated)
            updatedMap = HM.alter (const $ Just $ aggregatedX) fullname aggrMap

        updateCounters cs lme (logname, msgname) updatedMap (namedAggregated : aggs)

    sendAggregated :: LogObject -> Text -> IO ()
    sendAggregated aggregatedMsg@(LogObject _ (AggregatedMessage _)) logname = do
        -- enter the aggregated message into the |Trace|
        trace' <- Trace.appendName logname trace
        liftIO $ Trace.traceConditionally trace' aggregatedMsg
    -- ingnore every other message
    sendAggregated _ _ = return ()

\end{code}

\subsubsection{Update aggregation}\label{code:updateAggregation}\index{updateAggregation}
We distinguish an unitialized from an already initialized aggregation. The latter is properly initialized.
\\
We use Welford's online algorithm to update the estimation of mean and variance of the sample statistics.
(see \url{https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Welford's_Online_algorithm})

\begin{code}
updateAggregation :: Measurable -> Aggregated -> LOMeta -> Maybe Int -> Aggregated
updateAggregation v (AggregatedStats s) lme resetAfter =
    let count = fcount (fbasic s)
        reset = maybe False (count >=) resetAfter
    in
    if reset
    then
        singletonStats v
    else
        AggregatedStats $! Stats { flast  = v
                                 , fold = mkTimestamp
                                 , fbasic = updateBaseStats (count >= 1) v (fbasic s)
                                 , fdelta = updateBaseStats (count >= 2) (v - flast s) (fdelta s)
                                 , ftimed = updateBaseStats (count >= 2) (mkTimestamp - fold s) (ftimed s)
                                 }
  where
    mkTimestamp = utc2ns (tstamp lme)
    utc2ns (UTCTime days secs) =
        let yearsecs :: Rational
            yearsecs = 365 * 24 * 3600
            rdays,rsecs :: Rational
            rdays = toRational $ toModifiedJulianDay days
            rsecs = toRational secs
            s2ns = 1000000000
        in
        Nanoseconds $ round $ (fromRational $ s2ns * rsecs + rdays * yearsecs :: Double)

updateAggregation v (AggregatedEWMA e) _ _ = AggregatedEWMA $! ewma e v

updateBaseStats :: Bool -> Measurable -> BaseStats -> BaseStats
updateBaseStats False _ s = s {fcount = fcount s + 1}
updateBaseStats True v s =
    let newcount = fcount s + 1
        newvalue = getDouble v
        delta = newvalue - fsum_A s
        dincr = (delta / fromIntegral newcount)
        delta2 = newvalue - fsum_A s - dincr
    in
    BaseStats { fmin   = min (fmin s) v
              , fmax   = max v (fmax s)
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
ewma :: EWMA -> Measurable -> EWMA
ewma (EmptyEWMA a) v = EWMA a v
ewma (EWMA a s@(Microseconds _)) y@(Microseconds _) =
    EWMA a $ Microseconds $ round $ a * (getDouble y) + (1 - a) * (getDouble s)
ewma (EWMA a s@(Seconds _)) y@(Seconds _) =
    EWMA a $ Seconds $ round $ a * (getDouble y) + (1 - a) * (getDouble s)
ewma (EWMA a s@(Bytes _)) y@(Bytes _) =
    EWMA a $ Bytes $ round $ a * (getDouble y) + (1 - a) * (getDouble s)
ewma (EWMA a (PureI s)) (PureI y) =
    EWMA a $ PureI $ round $ a * (fromInteger y) + (1 - a) * (fromInteger s)
ewma (EWMA a (PureD s)) (PureD y) =
    EWMA a $ PureD $ a * y + (1 - a) * s
ewma _ _ = error "Cannot average on values of different type"

\end{code}
