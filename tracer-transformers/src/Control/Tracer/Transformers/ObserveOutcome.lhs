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
{-# LANGUAGE TypeFamilies #-}

module Control.Tracer.Transformers.ObserveOutcome
    (
    -- * transformer
      Outcome (..)
    , OutcomeProgressionStatus (..)
    , mkOutcomeExtractor
    ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.IORef (newIORef,readIORef, writeIORef)

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

-- | Generic Trace transformer (reactive). It could be written to take
--   an initial argument, but restricting the scope of that
--   per-invocation state seems more appropriate (for the
--   moment). That may be of use if\/when explict management of
--   timeout was required and\/or non-termination of the outcome at
--   the end of a run was of interest.
mkOutcomeExtractor :: (MonadIO m, Outcome m a)
                   => m (OutcomeEnhancedTracer m a -> Tracer m a)
mkOutcomeExtractor =  (liftIO $ newIORef Nothing) >>= pure . go
  where
    go s tr =  Tracer $ \a -> do
      classifyObservable a >>= \case
        OutcomeOther
          -> traceWith tr $ Left a
        o -> (liftIO $ readIORef s) >>= \case
          Nothing
            | o == OutcomeStarts
              -> do !z <- captureObservableValue a
                    traceWith tr $ Left a
                    liftIO (writeIORef s $ Just z)
            | otherwise -- OutcomeEnds
              -> do traceWith tr $ Left a
                    traceWith tr $ Right EndsBeforeStarted
          (Just b)
            | o == OutcomeEnds
              -> do !z <- captureObservableValue a
                    traceWith tr $ Left a
                    v <- computeOutcomeMetric a b z
                    traceWith tr $ Right (ProgressedNormally v)
                    liftIO (writeIORef s $ Nothing)
            | otherwise -- OutcomeStarts
              -> do !z <- captureObservableValue a
                    traceWith tr $ Left a
                    v <- computeOutcomeMetric a b z
                    traceWith tr $ Right (StartsBeforeEnds v)
                    liftIO (writeIORef s $ Just z)

\end{code}
