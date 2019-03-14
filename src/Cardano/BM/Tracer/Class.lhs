\subsection{Cardano.BM.Tracer.Class}
\label{code:Cardano.BM.Tracer.Class}

\begin{code}
module Cardano.BM.Tracer.Class
    ( Tracer (..)
    , Contravariant(..)
    , nullTracer
    ) where

import Data.Functor.Contravariant (Contravariant(..))

\end{code}

The notion of a `Tracer` is an action that can be used to observe
information of interest during evaluation. `Tracer`s can capture (and
annotate) such observations with additional information from their
execution context.

\begin{code}
newtype Tracer m s = Tracer { tracing :: s ->  m () }
\end{code}

A `Tracer` is an instance of `Contravariant`, which permits new
`Tracer`s to be constructed that feed into the existing Tracer by use
of `contramap`.

\begin{code}
instance Contravariant (Tracer m) where
    contramap f (Tracer t) = Tracer (t . f)

\end{code}

Although a `Tracer` is invoked in a monadic context (which may be
`Identity`), the construction of a new Tracer is a pure function.

This brings with it the constraint that the derived Tracer's form a
hierachy which has it root at the top level tracer.

The simplest tracer - one that does not generate output

\begin{code}
nullTracer :: (Applicative m) => Tracer m a
nullTracer = Tracer $ \_ -> pure ()

\end{code}

