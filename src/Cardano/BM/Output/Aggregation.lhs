
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
    , updateAggregation
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
import           Data.Text (Text)
import           Data.Word (Word64)
import           GHC.Clock (getMonotonicTimeNSec)

import           Cardano.BM.Data.Aggregated (Aggregated (..), EWMA (..),
                     Measurable (..), Stats (..), getDouble, singleton)
import           Cardano.BM.Data.AggregatedKind (AggregatedKind (..))
import           Cardano.BM.Data.Backend
import           Cardano.BM.Configuration.Model (Configuration, getAggregatedKind)
import           Cardano.BM.Data.Counter (Counter (..), CounterState (..),
                     nameCounter)
import           Cardano.BM.Data.LogItem

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
                            { aeAggregated :: Aggregated
                            , aeResetAfter :: Maybe Integer
                            , aeLastSent   :: Timestamp
                            }

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

    realizefrom conf switchboard = do
        aggref <- newEmptyMVar
        aggregationQueue <- atomically $ TBQ.newTBQueue 2048
        dispatcher <- spawnDispatcher conf HM.empty aggregationQueue switchboard
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
                => Configuration
                -> AggregationMap
                -> TBQ.TBQueue (Maybe NamedLogItem)
                -> e
                -> IO (Async.Async ())
spawnDispatcher conf aggMap aggregationQueue switchboard = Async.async $ qProc aggMap
  where
    qProc aggregatedMap = do
        maybeItem <- atomically $ TBQ.readTBQueue aggregationQueue
        case maybeItem of
            Just item -> do
                (updatedMap, aggregations) <- update (lnItem item) (lnName item) aggregatedMap
                unless (null aggregations) $
                    sendAggregated (AggregatedMessage aggregations) switchboard (lnName item)
                qProc updatedMap
            Nothing -> return ()

    update :: LogObject
           -> LoggerName
           -> HM.HashMap Text AggregatedExpanded
           -> IO (HM.HashMap Text AggregatedExpanded, [(Text, Aggregated)])
    update (LogValue iname value) logname agmap = do
        let name = logname <> "." <> iname
        aggregated <-
            case HM.lookup name agmap of
                Nothing -> do
                    -- if Aggregated does not exist; initialize it.
                    aggregatedKind <- getAggregatedKind conf name
                    case aggregatedKind of
                        StatsAK      -> return $ singleton value
                        EwmaAK aEWMA -> do
                            let initEWMA = EmptyEWMA aEWMA
                            return $ AggregatedEWMA $ ewma initEWMA value
                Just a -> return $ updateAggregation value (aeAggregated a) (aeResetAfter a)
        now <- getMonotonicTimeNSec
        let aggregatedX = AggregatedExpanded {
                            aeAggregated = aggregated
                          , aeResetAfter = Nothing
                          , aeLastSent = now
                          }
            namedAggregated = [(iname, aeAggregated aggregatedX)]
            updatedMap = HM.alter (const $ Just $ aggregatedX) name agmap
        -- use of HM.alter so that in future we can clear the Agrregated
        -- by using as alter's arg a function which returns Nothing.
        return (updatedMap, namedAggregated)
    update (ObserveDiff counterState) logname agmap = do
        let counters = csCounters counterState
        (mapNew, aggs) <- updateCounters counters logname agmap []

        return (mapNew, reverse aggs)
    -- TODO for text messages aggregate on delta of timestamps
    update _ _ agmap = return (agmap, [])

    updateCounters :: [Counter]
                  -> LoggerName
                  -> HM.HashMap Text AggregatedExpanded
                  -> [(Text, Aggregated)]
                  -> IO (HM.HashMap Text AggregatedExpanded, [(Text, Aggregated)])
    updateCounters [] _ aggrMap aggs = return $ (aggrMap, aggs)
    updateCounters (counter : cs) logname aggrMap aggs = do
        let name = cName counter
            fullname = logname <> "." <> name
            value = cValue counter
        aggregated <-
            case HM.lookup fullname aggrMap of
                    -- if Aggregated does not exist; initialize it.
                    Nothing -> do
                        aggregatedKind <- getAggregatedKind conf fullname
                        case aggregatedKind of
                            StatsAK      -> return $ singleton value
                            EwmaAK aEWMA -> do
                                let initEWMA = EmptyEWMA aEWMA
                                return $ AggregatedEWMA $ ewma initEWMA value
                    Just a -> return $ updateAggregation value (aeAggregated a) (aeResetAfter a)
        now <- getMonotonicTimeNSec
        let aggregatedX = AggregatedExpanded {
                            aeAggregated = aggregated
                          , aeResetAfter = Nothing
                          , aeLastSent = now
                          }
            namedAggregated = (((nameCounter counter) <> "." <> name), aggregated)
            updatedMap = HM.alter (const $ Just $ aggregatedX) fullname aggrMap

        updateCounters cs logname updatedMap (namedAggregated :aggs)

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

\subsubsection{Update aggregation}\label{code:updateAggregation}\index{updateAggregation}
We distinguish an unitialized from an already initialized aggregation. The latter is properly initialized.
\\
We use Welford's online algorithm to update the estimation of mean and variance of the sample statistics.
(see \url{https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Welford's_Online_algorithm})

\begin{code}
updateAggregation :: Measurable -> Aggregated -> Maybe Integer -> Aggregated
updateAggregation v (AggregatedStats s) resetAfter =
    let count = fcount s
        reset = maybe False (count >=) resetAfter
    in
    if reset
    then
        singleton v
    else
        let newcount = count + 1
            newvalue = getDouble v
            delta = newvalue - fsum_A s
            dincr = (delta / fromInteger newcount)
            delta2 = newvalue - fsum_A s - dincr
        in
        AggregatedStats Stats { flast  = v
                                    , fmin   = min (fmin s) v
                                    , fmax   = max (fmax s) v
                                    , fcount = newcount
                                    , fsum_A = fsum_A s + dincr
                                    , fsum_B = fsum_B s + (delta * delta2)
                                    }
updateAggregation v (AggregatedEWMA e) _ =
    AggregatedEWMA $ ewma e v

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
ewma (EWMA a (Microseconds s)) (Microseconds y) =
    EWMA a $ Microseconds $ round $ a * (fromInteger y) + (1 - a) * (fromInteger s)
ewma (EWMA a (Seconds s)) (Seconds y) =
    EWMA a $ Seconds $ round $ a * (fromInteger y) + (1 - a) * (fromInteger s)
ewma (EWMA a (Bytes s)) (Bytes y) =
    EWMA a $ Bytes $ round $ a * (fromInteger y) + (1 - a) * (fromInteger s)
ewma (EWMA a (PureI s)) (PureI y) =
    EWMA a $ PureI $ round $ a * (fromInteger y) + (1 - a) * (fromInteger s)
ewma (EWMA a (PureD s)) (PureD y) =
    EWMA a $ PureD $ a * y + (1 - a) * s
ewma _ _ = error "Cannot average on values of different type"

\end{code}
