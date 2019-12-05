\label{code:Control.Tracer.Transformers.WithThreadAndTime}

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

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.IORef (newIORef,readIORef, writeIORef)

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

-- | Generic Tracer transformer.  Will omit consequent runs of similar log objects,
--   with similarity defined by a supplied predicate.  A run of similar log
--   objects is terminated by one of two events:
--     1. a dissimilar event, or
--     2. reaching the specified overflow limit for omitted messages.
--
--   At the end of a run of similar objects, a summary of omission is emitted.
--
--   Caveat 1:  the supplied criterion is expected to define a transitive relation.
--
--   Caveat 2:  the resulting tracer has state, which is lost upon trace termination.
--   This means that if the trace ends with a run of similar messages, this will
--   not be reflected in the trace itself, as observed by the backends
--   -- they'll only receive the first message of the run.
--
--   WARNING:  the resulting tracer is not thread-safe!
mkSynopsizer :: forall m a
              . (MonadIO m)
             => (a -> a -> Bool)
             -> Int
             -> Tracer m (Synopsized a) -> m (Tracer m a)
mkSynopsizer matchTest overflow tr =
  (liftIO . newIORef $ SynopsizerState 0 Nothing)
  >>= pure . go
  where
    go s = Tracer $ \a -> do
      ss  <- liftIO $ readIORef s
      ss' <- case ssLast ss of
        Nothing -> do
          traceWith tr (One a)
          pure ss { ssRepeats = 0, ssLast = Just a }
        Just prev ->
          if | prev `matchTest` a
             -> if | ssRepeats ss + 1 == overflow
                   -> traceWith tr (Many (ssRepeats ss + 1) prev) >>
                      pure ss { ssRepeats = 0 }

                   | otherwise
                   -> pure ss { ssRepeats = ssRepeats ss + 1 }

             | ssRepeats ss == 0
             -> traceWith tr (One a) >>
                pure ss { ssLast = Just a }

             | otherwise
             -> traceWith tr (Many (ssRepeats ss) prev) >>
                traceWith tr (One a) >>
                pure ss { ssRepeats = 0, ssLast = Just a }
      liftIO $ writeIORef s ss'

\end{code}
