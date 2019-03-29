\subsection{Control.Tracer.Transformers}
\label{code:Control.Tracer.Transformers}

%if style == newcode
\begin{code}
{-# LANGUAGE RankNTypes #-}

module Control.Tracer.Transformers
    ( showTracing
    , condTracing
    , condTracingM
    , natTracer
    ) where

import           Control.Monad (when)

import           Control.Tracer.Class

\end{code}
%endif

\subsubsection{Applying |show| on a |Tracer|'s messages}
The Tracer transformer exploiting Show.
 
\begin{code}
showTracing :: (Show a) => Tracer m String -> Tracer m a
showTracing = contramap show

\end{code}

\subsubsection{Conditional tracing - statically defined}\label{code:condTracing}\index{condTracing}
The Tracer transformer that allows for on/off control of tracing at
trace creation time.

\begin{code}
condTracing :: (Monad m) => (a -> Bool) -> Tracer m a -> Tracer m a
condTracing active tr = Tracer $ \s ->
    when (active s) (traceWith tr s)

\end{code}

\subsubsection{Conditional tracing - dynamically evaluated}\label{code:condTracingM}\index{condTracingM}
The tracer transformer that can exercise dynamic control
over tracing, the dynamic decision being made using the
context accessible in the monadic context.

\begin{code}
condTracingM :: (Monad m) => m (a -> Bool) -> Tracer m a -> Tracer m a
condTracingM activeP tr = Tracer $ \s -> do
    active <- activeP
    when (active s) (traceWith tr s)

\end{code}

\subsubsection{natTrace}\label{code:natTrace}\index{natTrace}
Natural transformation from monad |m| to monad |n|.
\begin{code}

natTracer :: (forall x . m x -> n x) -> Tracer m s -> Tracer n s
natTracer nat (Tracer tr) = Tracer (nat . tr)

\end{code}
