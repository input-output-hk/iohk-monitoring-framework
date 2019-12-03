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
    ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.IORef (newIORef,readIORef, writeIORef)

import           Cardano.BM.Trace
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Severity
import           Control.Tracer (Tracer (..), traceWith)

\end{code}
%endif

\begin{code}
-- | State required to track and summarise repeats.
data SynopsizerState a
  = SynopsizerState
    { ssRepeats  :: {-# UNPACK #-} !Int
    , ssLast     :: !(Maybe (LogObject a))
    }

-- | Generic Trace transformer.  Will omit consequent runs of similar log objects,
--   with similarity defined by supplied predicate.  When a run of similar log
--   objects is terminated by a dissimilar one, a summary of omitted objects is
--   emitted as a log object with same severity and privacy as the first object.
--
--   Handy predicates to use:  'loTypeEq' and 'loContentEq'.
--
--   WARNING:  the resulting tracer is not thread-safe!
mkSynopsizer :: forall m a
              . (MonadIO m)
             => (LogObject a -> LogObject a -> Bool)
             -> Trace m a -> m (Trace m a)
mkSynopsizer matchTest tr =
  (liftIO . newIORef $ SynopsizerState 0 Nothing)
  >>= pure . go
  where
    go s = Tracer $ \a -> do
      ss  <- liftIO $ readIORef s
      ss' <- case ssLast ss of
        Nothing -> do
          traceWith tr a
          pure ss { ssRepeats = 0, ssLast = Just a }
        Just prev ->
          if | prev `matchTest` a
             -> pure ss { ssRepeats = ssRepeats ss + 1 }

             | ssRepeats ss == 0
             -> traceWith tr a >>
                pure ss { ssLast = Just a }

             | otherwise
             -> traceRepeat (ssRepeats ss) prev >>
                traceWith tr a >>
                pure ss { ssRepeats = 0, ssLast = Just a }
      liftIO $ writeIORef s ss'

    traceLOC :: [LoggerName] -> Severity -> PrivacyAnnotation -> LOContent a -> m ()
    traceLOC name sev' priv' a = do
      meta <- mkLOMeta sev' priv'
      traceWith tr $ LogObject name meta a

    traceRepeat :: Int -> LogObject a -> m ()
    traceRepeat repeats prev =
      traceLOC ["synopsis"] (severity $ loMeta prev) (privacy $ loMeta prev) $
        LogRepeats repeats

\end{code}
