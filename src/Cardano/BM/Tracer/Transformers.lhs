\subsection{Cardano.BM.Tracer}
\label{code:Cardano.BM.Tracer}

%if style == newcode
\begin{code}
module Cardano.BM.Tracer.Transformers
    ( showTracing
    , condTracing
    , condTracingM
    ) where

import Control.Monad (when)
import Data.Functor.Contravariant (Contravariant(..))

import Cardano.BM.Tracer.Class
-- import Cardano.BM.Tracer.Simple

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
condTracing active tr = Tracer $ \s -> do
    when (active s) (tracing tr s)
\end{code}

The Tracer transformer with dynamic control of tracing.

\begin{code}
condTracingM :: (Monad m) => m (a -> Bool) -> Tracer m a -> Tracer m a
condTracingM activeP tr = Tracer $ \s -> do
    active <- activeP
    when (active s) (tracing tr s)
\end{code}
