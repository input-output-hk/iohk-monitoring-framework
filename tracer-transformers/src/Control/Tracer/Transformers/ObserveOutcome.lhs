\label{code:Control.Tracer.Transformers.ObserveOutcome}

%if style == newcode
\begin{code}
{-|
Module: ObserveOutcome

Observing events with annotations of thread id and time.
-}
{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Tracer.Transformers.ObserveOutcome
    (
    -- * transformer
      Outcome (..)
    , OutcomeProgressionStatus (..)
    , mkOutcomeExtractor
    ) where

import           Control.Monad.IO.Class (MonadIO (..))
-- We really need to use Cardano.Prelude here and gain access to more
-- advanced concurrency primitives.
import           Control.Concurrent.MVar (MVar, newMVar, readMVar, putMVar)

import           Control.Tracer (Tracer (..), traceWith)

\end{code}
%endif

\begin{code}
-- transformer of traces that have the structure of an 'Outcome'
-- (beginning and possible end)

-- the distinct stages
data OutcomeProgressionStatus
  = OutcomeStarts
  | OutcomeOther
  | OutcomeEnds
  deriving (Eq)

-- constructing an outcome from a sequence of observables
class (Monad m) => Outcome m a where
  type IntermediateValue a
  type OutcomeMetric a

  classifyObservable     :: a
                         -> m OutcomeProgressionStatus
  captureObservableValue :: a
                         -> m (IntermediateValue a)
  computeOutcomeMetric   :: a
                         -> IntermediateValue a
                         -> IntermediateValue a
                         -> m (OutcomeMetric a)


-- | The Maybe (OutcomeMetric a) captures the 'DeltaQ-ness' of the
--   nature of outcomes, may / may not complete.
type OutcomeEnhancedTracer m a
  = Tracer m (Either a (OutcomeFidelity (OutcomeMetric a)))

-- | Also need to know that observables happened in the "right way"

data OutcomeFidelity a
  = EndsBeforeStarted
  | StartsBeforeEnds a
  | ProgressedNormally a
--  might have "timeout" and/or Failureprogression?
  deriving (Show)

-- | Generic Trace transformer. It could be written to take
--   an initial argument, but restricting the scope of that
--   per-invocation state seems more appropriate (for the
--   moment). That may be of use if\/when explict management of
--   timeout was required and\/or non-termination of the outcome at
--   the end of a run was of interest.
mkOutcomeExtractor
    :: forall m a. (MonadIO m, Outcome m a)
    => m (OutcomeEnhancedTracer m a -> Tracer m a)
mkOutcomeExtractor = do
    maybeInterValue <- liftIO $ newMVar Nothing
    pure $ traceOutcomes maybeInterValue
  where
    traceOutcomes
        :: MVar (Maybe (IntermediateValue a))
        -> OutcomeEnhancedTracer m a
        -> Tracer m a
    traceOutcomes maybeInterValue tr = Tracer $ \a -> do
      classifedObservable <- classifyObservable a
      case classifedObservable of
        OutcomeOther    -> traceWith tr $ Left a
        -- modifyMVar_ is safer here.
        outcome         -> do
            observedResult <- liftIO $ readMVar maybeInterValue
            case observedResult of
                Nothing   -> outcomeWithoutValue outcome a
                (Just b)  -> outcomeWithValue outcome a b      
      where
        -- If we don't have any intermediate values and the outcome is
        -- @OutcomeStarts@, then we set the initial value inside the MVar.
        outcomeWithoutValue :: OutcomeProgressionStatus -> a -> m ()
        outcomeWithoutValue OutcomeStarts a = do 
            -- Forces evaluation?!
            !z <- captureObservableValue a
            traceWith tr $ Left a
            liftIO (putMVar maybeInterValue $ Just z)
        outcomeWithoutValue _otherwise a = do -- Outcome ends
            traceWith tr $ Left a
            traceWith tr $ Right EndsBeforeStarted
            -- We remove what we had been measuring, this should
            -- probably be an error as well.
            liftIO (putMVar maybeInterValue $ Nothing)

        -- If we do have any intermediate values and the outcome is
        -- @OutcomeStarts@, then we set the initial value inside the MVar.
        outcomeWithValue :: OutcomeProgressionStatus -> a -> IntermediateValue a -> m ()
        outcomeWithValue OutcomeEnds a b = do 
            -- Forces evaluation?!
            !z <- captureObservableValue a
            traceWith tr $ Left a
            v <- computeOutcomeMetric a b z
            traceWith tr $ Right (ProgressedNormally v)
            liftIO (putMVar maybeInterValue $ Nothing)
        outcomeWithValue _otherwise a b = do -- OutcomeStarts, this could be ignored since it "resets".
            -- Forces evaluation?!
            !z <- captureObservableValue a
            traceWith tr $ Left a
            v <- computeOutcomeMetric a b z
            traceWith tr $ Right (StartsBeforeEnds v) -- Probably some error.
            liftIO (putMVar maybeInterValue $ Just z)



\end{code}
