\subsection{Cardano.BM.Data.Transformers}
\label{code:Cardano.BM.Data.Transformers}

%if style == newcode
\begin{code}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.BM.Data.Transformers
  ( liftCounting
  , liftFolding
  , liftSynopsized
  )
  where

import           Control.Monad.IO.Class (MonadIO)

import           Data.Text (Text)

import           Cardano.BM.Data.Aggregated (Measurable(..))
import           Cardano.BM.Data.LogItem
                     ( LOContent(..), LOMeta(..)
                     , LogObject(..), LoggerName
                     , PrivacyAnnotation(..)
                     , mkLOMeta)
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.Tracer (Tracer(..), traceWith)
import           Cardano.BM.Data.Trace

import           Control.Tracer.Transformers
import           Control.Tracer.Transformers.Synopsizer
                     (Synopsized(..))
\end{code}

\subsubsection{Transformer for counting events}
\label{code:liftCounting}
\index{liftCounting}
Lift a 'Counting' tracer into a 'Trace' of 'PureI' messages.
\begin{code}

liftCounting
  :: forall m a
  .  LOMeta -> [LoggerName] -> Text -> Trace m a
  -> Tracer m (Counting (LogObject a))
liftCounting meta name desc tr = Tracer (traceIncrement tr)
 where
   traceIncrement :: Trace m a -> Counting (LogObject a) -> m ()
   traceIncrement t (Counting n) =
     traceWith t . LogObject name meta . LogValue desc . PureI $ fromIntegral n

\end{code}

\subsubsection{Transformer for state folding}
\label{code:liftFolding}
\index{liftFolding}
Lift a 'Trace' tracer into a 'Trace' of 'PureI' messages,
thereby specialising it to 'Integral'.
\begin{code}

liftFolding
  :: forall m f a
  .  (Integral f) -- TODO:  generalise
  => LOMeta -> [LoggerName] -> Text -> Trace m a
  -> Tracer m (Folding (LogObject a) f)
liftFolding meta name desc tr = Tracer (traceIncrement tr)
 where
   traceIncrement :: Trace m a -> Folding (LogObject a) f -> m ()
   traceIncrement t (Folding f) =
     traceWith t . LogObject name meta . LogValue desc . PureI $ fromIntegral f

\end{code}

\subsubsection{Transformer for synopsization}
\label{code:liftSynopsized}
\index{liftSynopsized}
Make a |Trace| |Synopsized|.
\begin{code}

liftSynopsized
  :: forall m a. MonadIO m
  => Trace m a
  -> Severity
  -> PrivacyAnnotation
  -> Tracer m (Synopsized (LogObject a))
liftSynopsized tr sev priv =
  Tracer $ \case
    One a            -> traceWith   tr   a
    Many n _fir _las -> traceRepeat tr n =<< mkLOMeta sev priv
 where
   traceRepeat :: Trace m a -> Int -> LOMeta -> m ()
   traceRepeat t repeats meta =
     traceWith t . LogObject ["synopsis"] meta $ LogRepeats repeats

\end{code}
