\label{code:Control.Tracer.Transformers.Synopsizer}

%if style == newcode
\begin{code}
{-|
Module: Synopsizer

Synopsize runs of repeated events, as an initial message with a summary at the end.
-}
{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf     #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Tracer.Transformers.Synopsizer
    (
    -- * transformer
      mkSynopsizer
    , Synopsized(..)
    ) where

import           Control.Concurrent (MVar, newMVar, modifyMVarMasked)
import           Control.Monad (mapM_, join)
import           Control.Monad.IO.Class (MonadIO (..))

import           Control.Tracer (Tracer (..), traceWith)

\end{code}
%endif

\begin{code}
-- | State required to track and summarise repeats.
data SynopsizerState a
  = SynopsizerState
    { ssRepeats  :: {-# UNPACK #-} !Int
    , ssLast     :: !(Maybe a)
    }

-- | Abstract representation of a message stream with synopses.
data Synopsized a
  = One !a
    -- ^ Normal, unsynopsized message.
  | Many {-# UNPACK #-} !Int !a
    -- ^ Synopsis for a specified number of messages similar to a previous one.
    --   Note that several 'Many' messages can follow the initial 'One' message,
    --   because of configurable synopsis overflow.

-- | Generic Tracer transformer, intended for suppression of repeated messages.
--
--   The transformer is specified in terms of an internal counter (starting at zero),
--   and a given predicate on the state of the counter and the pair of subsequent messages.
--   If the predicate returns 'False', the message is suppressed, and the counter is increased.
--   Otherwise, the counter is reset to zero, and:
--     - the message is wrapped into 'One',
--     - an additional 'Many' message is added for the predecessor, if the counter was zero.
--
--   Caveat:  the resulting tracer has state, which is lost upon trace termination.
--   This means that if the trace ends with a run of positively-flagged messages, this will
--   not be reflected in the trace itself, as observed by the backends
--   -- they'll only receive the first message of the run.
mkSynopsizer :: forall m a
              . (MonadIO m)
             => ((Int, a) -> a -> Bool)
             -> Tracer m (Synopsized a) -> m (Tracer m a)
mkSynopsizer overflowTest tr =
  (liftIO . newMVar $ SynopsizerState 0 Nothing)
  >>= pure . mkTracer
  where
    mkTracer :: MVar (SynopsizerState a) -> Tracer m a
    mkTracer mv = Tracer $ \a ->
      join . liftIO $ modifyMVarMasked mv (pure . contended a)

    -- This is a fast, pure computation to be done inside the lock.
    contended :: a -> SynopsizerState a -> (SynopsizerState a, m ())
    contended a ss = 
      case ssLast ss of
        Nothing -> (,)
          (ss { ssRepeats = 0, ssLast = Just a })
          (traceWith tr (One a))

        Just prev ->
          if | (ssRepeats ss, prev) `overflowTest` a
             -> (,)
               (ss { ssRepeats = 0, ssLast = Just a })
               (mapM_ (traceWith tr) $
                [ Many (ssRepeats ss + 1) prev
                | ssRepeats ss /= 0]
                ++ [ One a ])

             | otherwise
             -> (,)
               (ss { ssRepeats = ssRepeats ss + 1, ssLast = Just a })
               (pure ())

\end{code}
