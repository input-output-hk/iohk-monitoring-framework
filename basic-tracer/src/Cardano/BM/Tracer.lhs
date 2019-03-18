
\subsection{Cardano.BM.Tracer}
\label{code:Cardano.BM.Tracer}

%if style == newcode
\begin{code}

module Cardano.BM.Tracer
    ( Tracer (..)
    , Contravariant(..)
    -- * tracing
    , tracingWith
    -- * tracers
    , nullTracer
    , stdoutTracer
    , debugTracer
    -- * transformers
    , showTracing
    , condTracing
    , condTracingM
    -- * examples
    , example1
    ) where

import           Control.Monad (void)

import           Cardano.BM.Tracer.Class
import           Cardano.BM.Tracer.Output
import           Cardano.BM.Tracer.Transformers

\end{code}
%endif

Tracing using the contravariant |Tracer| naturally reads:

\begin{spec}
let logTrace = tracing $ showTracing $ stdoutTracer
in  logTrace "hello world"
\end{spec}

\begin{code}

example1 :: IO ()
example1 = do
    let logTrace a = tracingWith (showTracing (contramap ("Debug: " ++) stdoutTracer)) a
    void $ callFun1 logTrace

callFun1 :: (String -> IO ()) -> IO Int
callFun1 logTrace = do
    logTrace "in function 1"
    return 42

\end{code}

