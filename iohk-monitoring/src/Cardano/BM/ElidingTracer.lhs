
\subsection{Cardano.BM.ElidingTracer}
\label{code:Cardano.BM.ElidingTracer}

%if style == newcode
\begin{code}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}

module Cardano.BM.ElidingTracer
    (
      ElidingTracer (..)
    ) where

import           Control.Concurrent.MVar (MVar, newMVar, modifyMVar_)
import           Control.Monad (when)

import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Aggregated (Measurable(PureI))
import           Cardano.BM.Data.Trace
import           Cardano.BM.Data.Tracer
import           Cardano.BM.Trace (traceNamedObject)

\end{code}
%endif

\subsubsection{Tracer transformer for eliding messages}
\label{code:ElidingTracer}\index{ElidingTracer}

The eliding tracer transformer depends on two predicates to decide on which
observable type eliding messages is active (\ref{doelide}), and whether two messages can be
considered equivalent and thus be elided (\ref{isEquivalent}).
\begin{code}

class ElidingTracer a where
\end{code}

This predicate is |True| for message types for which eliding is enabled. Needs to be
overwritten in instances of |ElidingTracer|.
\begin{code}
  doelide :: a -> Bool
\end{code}

The predicate to determine if two messages are |equivalent|. This needs to be
overwritten in instances of |ElidingTracer|.
\begin{code}
  isEquivalent :: a -> a -> Bool
\end{code}

Create a new state |MVar|.
\label{code:newstate}\index{ElidingTracer!newstate}
\begin{code}
  newstate :: IO (MVar (Maybe a, Int))
  default newstate :: IO (MVar (Maybe a, Int))
  newstate = newMVar (Nothing, 0)
\end{code}

Internal state transitions.
\begin{code}
  starteliding :: (ToObject t, Transformable t IO a)
               => TracingFormatting -> TracingVerbosity -> Trace IO t
               -> a -> IO (Maybe a, Int)
  default starteliding :: (ToObject t, Transformable t IO a)
                       => TracingFormatting -> TracingVerbosity -> Trace IO t
                       -> a -> IO (Maybe a, Int)
  starteliding tform tverb tr ev = do
    traceWith (toLogObject' tform tverb tr) ev
    return (Just ev, 0)

  conteliding :: (ToObject t, Transformable t IO a)
              => TracingFormatting -> TracingVerbosity -> Trace IO t
              -> a -> (Maybe a, Int) -> IO (Maybe a, Int)
  default conteliding :: Transformable t IO a
                      => TracingFormatting -> TracingVerbosity -> Trace IO t
                      -> a -> (Maybe a, Int) -> IO (Maybe a, Int)
  conteliding _tform _tverb _tr _ (Nothing, _count) = return (Nothing, 0)
  conteliding _tform _tverb _tr ev (_old, count) = return (Just ev, count + 1)

  stopeliding :: (ToObject t, Transformable t IO a)
              => TracingFormatting -> TracingVerbosity -> Trace IO t
              -> a -> (Maybe a, Int) -> IO (Maybe a, Int)
  default stopeliding :: (ToObject t, Transformable t IO a)
                      => TracingFormatting -> TracingVerbosity -> Trace IO t
                      -> a -> (Maybe a, Int) -> IO (Maybe a, Int)
  stopeliding tform tverb tr ev (Nothing, _count) = do
    traceWith (toLogObject' tform tverb tr) ev
    return (Nothing, 0)
  stopeliding tform tverb tr ev (Just ev0, count) = do
    when (count > 1) $ do  -- report the number of elided messages
      meta <- mkLOMeta (defineSeverity ev0) (definePrivacyAnnotation ev0)
      traceNamedObject tr (meta, LogValue "before next, messages elided" (PureI $ toInteger (count - 1)))
    when (count > 0) $  -- output last elided message
      traceWith (toLogObject' tform tverb tr) ev0    
    traceWith (toLogObject' tform tverb tr) ev
    return (Nothing, 0)
\end{code}

The transformer from a Tracer IO emph{a} to Tracer IO (LogObject t) contains
the main logic of eliding messages.
\label{code:elideToLogObject}\index{ElidingTracer!elideToLogObject}
\begin{code}
  elideToLogObject
      :: (ToObject t, Transformable t IO a)
      => TracingFormatting -> TracingVerbosity -> MVar (Maybe a, Int)
      -> Trace IO t -> Tracer IO a
  default elideToLogObject
      :: (ToObject t, Transformable t IO a)
      => TracingFormatting -> TracingVerbosity -> MVar (Maybe a, Int)
      -> Trace IO t -> Tracer IO a
  elideToLogObject tform tverb mvar tr = Tracer $ \ev ->
    modifyMVar_ mvar $ \s@(old, _count) ->
    if doelide ev
      then
        case old of
          Nothing -> starteliding tform tverb tr ev
          Just ev0 ->
            if ev `isEquivalent` ev0
              then
                conteliding tform tverb tr ev s
              else
                stopeliding tform tverb tr ev s
      else
        stopeliding tform tverb tr ev s

\end{code}
