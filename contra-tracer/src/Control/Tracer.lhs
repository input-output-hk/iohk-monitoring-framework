
\subsection{Control.Tracer}
\label{code:Control.Tracer}

%if style == newcode
\begin{code}

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
    , showTracing
    , condTracing
    , condTracingM
    , natTracer
    -- * examples
    , example1
    ) where

import           Control.Monad (void)

import           Control.Tracer.Class
import           Control.Tracer.Output
import           Control.Tracer.Transformers

\end{code}
%endif

Tracing using the contravariant |Tracer| naturally reads:

\begin{spec}
let logTrace = traceWith $ showTracing $ stdoutTracer
in  logTrace "hello world"
\end{spec}

\begin{code}

example1 :: IO ()
example1 = do
    let logTrace a = traceWith (showTracing (contramap ("Debug: " ++) stdoutTracer)) a
    void $ callFun1 logTrace

callFun1 :: (String -> IO ()) -> IO Int
callFun1 logTrace = do
    logTrace "in function 1"
    return 42

\end{code}

