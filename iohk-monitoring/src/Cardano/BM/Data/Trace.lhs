
\subsection{Cardano.BM.Data.Trace}
\label{code:Cardano.BM.Data.Trace}

%if style == newcode
\begin{code}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.BM.Data.Trace
  ( Trace
  , mkSynopsizedTrace
  )
  where

import           Control.Monad.IO.Class (MonadIO)

import           Cardano.BM.Data.LogItem
                     ( LOContent(..), LOMeta(..)
                     , LogObject(..), LoggerName, PrivacyAnnotation
                     , mkLOMeta)
import           Cardano.BM.Data.Severity (Severity)
import           Cardano.BM.Data.Tracer (Tracer(..), traceWith)

import           Control.Tracer.Transformers.Synopsizer
                     (Synopsized(..), mkSynopsizer)

\end{code}
%endif

\subsubsection{Trace}\label{code:Trace}\index{Trace}
A |Trace m a| is a |Tracer m (LogObject a)|.
\begin{code}

type Trace m a = Tracer m (LogObject a)
\end{code}

\subsubsection{Transformer for synopsization}
\label{code:mkSynopsizedTrace}
\index{mkSynopsizedTrace}
Build upon |Synopsizer| tracer transformer to suppress repeated messages,
based on a given overflow criterion.

The transformer is specified in terms of an internal counter (starting at zero),
and a given predicate on the state of the counter and the pair of subsequent messages.
If the predicate returns 'False', the message is suppressed, and the counter is increased.
Otherwise, the counter is reset to zero, and:
  - the message is wrapped into 'One',
  - an additional 'Many' message is added for the predecessor, if the counter was zero.

Handy predicates to use as part of the overflow predicate:  'loTypeEq' and 'loContentEq'.

Caveat:  the resulting tracer has state, which is lost upon trace termination.
This means that if the trace ends with a run of positively-flagged messages, this will
not be reflected in the trace itself, as observed by the backends
-- they'll only receive the first message of the run.
\begin{code}

mkSynopsizedTrace
  :: forall m a. MonadIO m
  => ((Int, LogObject a) -> LogObject a -> Bool)
  -> Trace m a
  -> m (Trace m a)
mkSynopsizedTrace overflowTest tr =
  mkSynopsizer overflowTest (transform tr)
 where
   transform :: Trace m a -> Tracer m (Synopsized (LogObject a))
   transform trace = Tracer $ \case
       One    a -> traceWith   trace   a
       Many n a -> traceRepeat trace n a

   traceLOC :: Trace m a -> [LoggerName] -> Severity -> PrivacyAnnotation -> LOContent a -> m ()
   traceLOC t name sev' priv' a = do
     meta <- mkLOMeta sev' priv'
     traceWith t $ LogObject name meta a

   traceRepeat :: Trace m a -> Int -> LogObject a -> m ()
   traceRepeat t repeats prev =
     traceLOC t ["synopsis"] (severity $ loMeta prev) (privacy $ loMeta prev) $
       LogRepeats repeats

\end{code}
