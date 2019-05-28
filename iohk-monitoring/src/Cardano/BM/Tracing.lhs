
\subsection{Cardano.BM.Tracing}
\label{code:Cardano.BM.Tracing}

%if style == newcode
\begin{code}

module Cardano.BM.Tracing
    ( Tracer (..)
    , LogObject (..)
    , ToLogObject (..)
    , ToObject (..)
    , appendName
    , nullTracer
    , traceWith
    , setupTrace
    ) where

import           Control.Tracer (Tracer (..), nullTracer, traceWith)
import           Cardano.BM.Data.LogItem (LogObject (..))
import           Cardano.BM.Data.Tracer (ToLogObject (..), ToObject (..))
import           Cardano.BM.Setup (setupTrace)
import           Cardano.BM.Trace (appendName)

\end{code}
%endif
