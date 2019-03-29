\subsection{Control.Tracer.Class}
\label{code:Control.Tracer.Class}

%if style == newcode
\begin{code}
module Control.Tracer.Class
    ( Tracer (..)
    , Contravariant(..)
    , nullTracer
    , traceWith
    ) where

import           Data.Functor.Contravariant (Contravariant (..))
import           Data.Semigroup (Semigroup(..))
import           Data.Monoid (Monoid(..))

\end{code}
%endif

\subsubsection{Contravariant Tracer}\label{code:Tracer}\index{Tracer}
The notion of a |Tracer| is an action that can be used to observe
information of interest during evaluation. |Tracer|s can capture (and
annotate) such observations with additional information from their
execution context.

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

\subsubsection{|nullTracer|}\label{code:nullTracer}\index{nullTracer}
The simplest tracer - one that suppresses all output.

\begin{code}
nullTracer :: Applicative m => Tracer m a
nullTracer = Tracer $ \_ -> pure ()

\end{code}

\subsubsection{traceWith}\label{code:traceWith}\index{traceWith}

\begin{code}
traceWith :: Tracer m a -> a -> m ()
traceWith = runTracer

\end{code}
