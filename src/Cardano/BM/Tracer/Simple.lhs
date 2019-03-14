\subsection{Cardano.BM.Tracer.Simple}
\label{code:Cardano.BM.Tracer.Simple}

%if style == newcode
\begin{code}
module Cardano.BM.Tracer.Simple
    ( stdoutTracer
    , debugTracer
    ) where

import           Control.Monad.IO.Class (MonadIO(..))
import           Debug.Trace (traceM)
import           Data.Functor.Contravariant (Op (..))

import           Cardano.BM.Tracer.Class

\end{code}
%endif

Some simple Tracers

The Tracer that prints a string (as a line) to stdout (usual caveats
about interleaving should be heeded).

\begin{code}
stdoutTracer :: (MonadIO m) => Tracer m String
stdoutTracer = Tracer $ Op $ liftIO . putStrLn

\end{code}

A Tracer that uses `TraceM` (from `Debug.Trace`) as its output mechanism.

\begin{code}
debugTracer :: (Applicative m) => Tracer m String
debugTracer = Tracer $ Op traceM

\end{code}
