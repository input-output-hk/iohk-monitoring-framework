\subsection{Cardano.BM.Tracer}
\label{code:Cardano.BM.Tracer}

%if style == newcode
\begin{code}
module Cardano.BM.Tracer.Transformers
    ( showTracing
    , condTracing
    , condTracingM
    ) where

import           Control.Monad (when)
import           Data.Functor.Contravariant (Contravariant (..), Op (..))

import           Cardano.BM.Tracer.Class

\end{code}
%endif

The Tracer transformers exploiting Show.

\begin{code}
showTracing :: (Show a) => Tracer m String -> Tracer m a
showTracing = contramap show

\end{code}

The Tracer transformer that allows for on/off control of tracing at
Trace creation time.

\begin{code}
condTracing :: (Monad m) => (a -> Bool) -> Tracer m a -> Tracer m a
condTracing active tr = Tracer $ Op $ \s -> do
    when (active s) (tracingWith tr s)

\end{code}

The tracer transformer that can exercise dynamic control
over tracing, the dynamic decision being made using the
context accessible in the monadic context.

\begin{code}
condTracingM :: (Monad m) => m (a -> Bool) -> Tracer m a -> Tracer m a
condTracingM activeP tr = Tracer $ Op $ \s -> do
    active <- activeP
    when (active s) (tracingWith tr s)
\end{code}
