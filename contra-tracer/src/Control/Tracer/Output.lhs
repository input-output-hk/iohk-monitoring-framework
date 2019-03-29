\subsection{Control.Tracer.Output}
\label{code:Control.Tracer.Output}

%if style == newcode
\begin{code}
module Control.Tracer.Output
    ( stdoutTracer
    , debugTracer
    ) where

import           Control.Monad.IO.Class (MonadIO(..))
import           Debug.Trace (traceM)

import           Control.Tracer.Class

\end{code}
%endif

\subsubsection{Directing a |Tracer|'s output to stdout}\label{code:stdoutTracer}\index{stdoutTracer}

The Tracer that prints a string (as a line) to stdout (usual caveats
about interleaving should be heeded).

\begin{code}
stdoutTracer :: (MonadIO m) => Tracer m String
stdoutTracer = Tracer $ liftIO . putStrLn

\end{code}

\subsubsection{Outputting a |Tracer| with \emph{Debug.Trace}}\label{code:debugTracer}\index{debugTracer}

A Tracer that uses |TraceM| (from |Debug.Trace|) as its output mechanism.

\begin{code}
debugTracer :: (Applicative m) => Tracer m String
debugTracer = Tracer Debug.Trace.traceM

\end{code}
