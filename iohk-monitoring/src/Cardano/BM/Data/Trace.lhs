
\subsection{Cardano.BM.Data.Trace}
\label{code:Cardano.BM.Data.Trace}

%if style == newcode
\begin{code}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.BM.Data.Trace
  ( Trace
  , liftSynopsized
  )
  where

import           Control.Monad.IO.Class (MonadIO)

import           Cardano.BM.Data.LogItem
                     ( LOContent(..), LOMeta(..)
                     , LogObject(..)
                     , mkLOMeta)
import           Cardano.BM.Data.Tracer (Tracer(..), traceWith)

import           Control.Tracer.Transformers.Synopsizer
                     (Synopsized(..))

\end{code}
%endif

\subsubsection{Trace}\label{code:Trace}\index{Trace}
A |Trace m a| is a |Tracer m (LogObject a)|.
\begin{code}

type Trace m a = Tracer m (LogObject a)
\end{code}

\subsubsection{Transformer for synopsization}
\label{code:liftSynopsized}
\index{liftSynopsized}
Make a |Trace| |Synopsized|.
\begin{code}

liftSynopsized
  :: forall m a. MonadIO m
  => Trace m a
  -> Tracer m (Synopsized (LogObject a))
liftSynopsized tr =
  Tracer $ \case
    One    a       -> traceWith   tr   a
    Many n fir las -> traceRepeat tr n fir las
 where
   traceRepeat :: Trace m a -> Int -> LogObject a -> LogObject a -> m ()
   traceRepeat t repeats fir las = do
     meta <- mkLOMeta (severity $ loMeta fir) (privacy $ loMeta fir)
     traceWith t . LogObject ["synopsis"] meta $ LogRepeats repeats fir las

\end{code}
