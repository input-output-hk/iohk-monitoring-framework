
\label{code:Control.Tracer}

%if style == newcode
\begin{code}
{-# LANGUAGE RankNTypes #-}
{-|
Module: Control.Tracer

'Tracer' is a contravariant functor to thread observable values through a
number of transformers, possibly annotating them with additional information,
or filtering them based on evaluating predicates.
-}
module Control.Tracer
    ( Tracer (..)
    , Contravariant(..)
    -- * tracing
    , traceWith
    -- * tracers
    , nullTracer
    , stdoutTracer
    , debugTracer
    -- * transformers
    , contramapM
    , showTracing
    , condTracing
    , condTracingM
    , natTracer
    ) where

import           Control.Monad (when, (>=>))
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Functor.Contravariant (Contravariant (..))
import           Debug.Trace (traceM)

\end{code}
%endif

\subsection{Examples}
Tracing using the contravariant |Tracer| naturally reads:

\begin{spec}
let logTrace = traceWith $ showTracing $ stdoutTracer
in  logTrace "hello world"

\end{spec}

%if style == newcode
\begin{code}
{-| example: simply output a message on the console

> let logTrace = traceWith $ showTracing $ stdoutTracer
> in  logTrace "hello world"

-}
\end{code}
%endif

%if style == newcode
\begin{code}
{-| example: calling a function and passing in a 'Tracer'

> example1 :: IO ()
> example1 = do
>     let logTrace a = traceWith (showTracing (contramap ("Debug: " ++) stdoutTracer)) a
>     void $ callFun1 logTrace

> callFun1 :: (String -> IO ()) -> IO Int
> callFun1 logTrace = do
>     logTrace "in function 1"
>     return 42

-}
\end{code}
%endif

\subsection{Contravariant |Tracer|}\label{code:Tracer}\index{Tracer}
The notion of a |Tracer| is an action that can be used to observe
information of interest during evaluation. |Tracer|s can capture (and
annotate) such observations with additional information from their
execution context.

%if style == newcode
\begin{code}
-- | 'runTracer' evaluates a 'Tracer' (i.e. consumes its argument)
\end{code}
%endif
\begin{code}
newtype Tracer m a = Tracer { runTracer :: a -> m () }

\end{code}

\index{Tracer!instance of Contravariant}
A |Tracer| is an instance of |Contravariant|, which permits new
|Tracer|s to be constructed that feed into the existing Tracer by use
of |contramap|.

\begin{code}
instance Contravariant (Tracer m) where
    contramap f (Tracer t) = Tracer (t . f)

\end{code}

Although a |Tracer| is invoked in a monadic context (which may be
|Identity|), the construction of a new |Tracer| is a pure function.
This brings with it the constraint that the derived |Tracer|s form a
hierachy which has its root at the top level tracer.

\index{Tracer!instance of Monoid}

In principle a |Tracer| is an instance of |Semigroup| and |Monoid|, by
sequential composition of the tracing actions.

\begin{code}
instance Applicative m => Semigroup (Tracer m s) where
    Tracer a1 <> Tracer a2 = Tracer $ \s -> a1 s *> a2 s

instance Applicative m => Monoid (Tracer m s) where
    mappend = (<>)
    mempty  = nullTracer
\end{code}

\subsubsection{nullTracer}\label{code:nullTracer}\index{nullTracer}
The simplest tracer - one that suppresses all output.

%if style == newcode
\begin{code}
-- | this 'Tracer' forgets about all arguments
\end{code}
%endif
\begin{code}
nullTracer :: Applicative m => Tracer m a
nullTracer = Tracer $ \_ -> pure ()

\end{code}

\subsubsection{traceWith}\label{code:traceWith}\index{traceWith}

%if style == newcode
\begin{code}
-- | trace an observable value with a 'Tracer'
\end{code}
%endif
\begin{code}
traceWith :: Tracer m a -> a -> m ()
traceWith = runTracer

\end{code}

\subsection{Transformers}
\subsubsection{Contravariant transformers using Kleisli arrows}
Tracers can be transformed using Kleisli arrows, e.g. arrows of the type
|Monad m => a -> m b|, technically this makes |Tracer| a contravariant functor
over |Kleisli| category.  The important difference from using `contramap` is
that the monadic action runs when a tracer is called, this might be the prefered
behaviour when trying to trace timeing information.

%if style == newcode
\begin{code}
-- | Transform a tracer using a Kleisli map.
\end{code}
%endif
\begin{code}
contramapM :: Monad m
           => (a -> m b)
           -> Tracer m b
           -> Tracer m a
contramapM f (Tracer tr) = Tracer (f >=> tr)
\end{code}

\subsubsection{Applying |show| on a |Tracer|'s messages}
The Tracer transformer exploiting Show.

%if style == newcode
\begin{code}
-- | transform a traced value to a showable instance.
\end{code}
%endif
\begin{code}
showTracing :: (Show a) => Tracer m String -> Tracer m a
showTracing = contramap show

\end{code}

\subsubsection{Conditional tracing - statically defined}\label{code:condTracing}\index{condTracing}
The Tracer transformer that allows for on/off control of tracing at
trace creation time.

%if style == newcode
\begin{code}
-- | conditionally trace an observable given the evaluation of a predicate.
\end{code}
%endif
\begin{code}
condTracing :: (Monad m) => (a -> Bool) -> Tracer m a -> Tracer m a
condTracing active tr = Tracer $ \s ->
    when (active s) (traceWith tr s)

\end{code}

\subsubsection{Conditional tracing - dynamically evaluated}\label{code:condTracingM}\index{condTracingM}
The tracer transformer that can exercise dynamic control
over tracing, the dynamic decision being made using the
context accessible in the monadic context.

%if style == newcode
\begin{code}
-- | conditionally trace an observable given the evaluation of a predicate in a monadic context.
\end{code}
%endif
\begin{code}
condTracingM :: (Monad m) => m (a -> Bool) -> Tracer m a -> Tracer m a
condTracingM activeP tr = Tracer $ \s -> do
    active <- activeP
    when (active s) (traceWith tr s)

\end{code}

\subsubsection{natTrace}\label{code:natTrace}\index{natTrace}
Natural transformation from monad |m| to monad |n|.
%if style == newcode
\begin{code}
-- | natural transformation from monad 'm' to monad 'n'.
\end{code}
%endif
\begin{code}
natTracer :: (forall x . m x -> n x) -> Tracer m s -> Tracer n s
natTracer nat (Tracer tr) = Tracer (nat . tr)

\end{code}

\subsection{Output}
\subsubsection{Directing a |Tracer|'s output to stdout}\label{code:stdoutTracer}\index{stdoutTracer}

The Tracer that prints a string (as a line) to stdout (usual caveats
about interleaving should be heeded).

%if style == newcode
\begin{code}
-- | Output a traced 'String' to 'stdout'
\end{code}
%endif
\begin{code}
stdoutTracer :: (MonadIO m) => Tracer m String
stdoutTracer = Tracer $ liftIO . putStrLn

\end{code}

\subsubsection{Outputting a |Tracer| with \emph{Debug.Trace}}\label{code:debugTracer}\index{debugTracer}

A Tracer that uses |TraceM| (from |Debug.Trace|) as its output mechanism.

%if style == newcode
\begin{code}
-- | Output a traced 'String' using 'Debug.Trace'
\end{code}
%endif
\begin{code}
debugTracer :: (Applicative m) => Tracer m String
debugTracer = Tracer Debug.Trace.traceM

\end{code}
