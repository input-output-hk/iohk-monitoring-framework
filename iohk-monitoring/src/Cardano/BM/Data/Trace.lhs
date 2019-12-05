
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
based on a given transitive similarity criterion.

A summary is printed every |overflow| entries, and at the end of a run
of similar messages.

Handy predicates to use:  'loTypeEq' and 'loContentEq'.

See the caveats in the |Synopsizer| tracer transformer description.
\begin{code}

mkSynopsizedTrace
  :: forall m a. MonadIO m
  => (LogObject a -> LogObject a -> Bool)
  -> Int
  -> Trace m a
  -> m (Trace m a)
mkSynopsizedTrace similarity overflow tr =
  mkSynopsizer similarity overflow (transform tr)
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
