
\subsection{Cardano.BM.Data.Trace}
\label{code:Cardano.BM.Data.Trace}

%if style == newcode
\begin{code}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.BM.Data.Trace
  ( Trace(.., Trc)
  , nullTrace
  , mkBaseTrace
  , mapStatic
  , mapTracer
  , trName
  , trTracer
  , TraceStatic(..)
  )
  where

import           Control.Tracer

import           Cardano.BM.Data.LogItem
                   (LogObject(..), LoggerName, emptyLoggerName)

\end{code}
%endif

\subsubsection{Trace}\label{code:Trace}\index{Trace}
A |Trace m a| is a |Tracer| containing the context name and a |LogObject a|.
\begin{code}

nullTrace :: Applicative m => Trace m a
nullTrace = mkBaseTrace nullTracer

{-# COMPLETE Trc #-}
pattern Trc
  :: LoggerName
  -> Tracer m (TraceStatic, LogObject a)
  -> Trace m a
pattern Trc { trName, trTracer } =
  Trace
  { traceStatic = TraceStatic
    { loggerName = trName }
  , traceTracer = trTracer }

newtype TraceStatic
  = TraceStatic
    { loggerName :: LoggerName
    }

mapStatic :: (TraceStatic -> TraceStatic) -> Trace m a -> Trace m a
mapStatic f tr@Trace{traceStatic} = tr { traceStatic = f traceStatic }

mapTracer
  :: (Tracer m (TraceStatic, LogObject b) -> Tracer m (TraceStatic, LogObject a))
  -> Trace m b -> Trace m a
mapTracer f tr@Trace{traceTracer} = tr { traceTracer = f traceTracer }

data Trace m a
  = Trace
    { traceStatic :: TraceStatic
    , traceTracer :: Tracer m (TraceStatic, LogObject a)
    }

mkBaseTrace :: Tracer m (TraceStatic, LogObject a) -> Trace m a
mkBaseTrace = Trace (TraceStatic emptyLoggerName)

\end{code}
