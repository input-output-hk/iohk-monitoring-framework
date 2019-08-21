
\subsection{Cardano.BM.Tracing}
\label{code:Cardano.BM.Tracing}

%if style == newcode
\begin{code}

module Cardano.BM.Tracing
    ( Tracer (..)
    , Trace
    , LogObject (..)
    , PrivacyAnnotation (..)
    , Severity (..)
    , ToLogObject (..)
    , ToObject (..)
    , Transformable (..)
    , TracingVerbosity (..)
    , TracingFormatting (..)
    , appendName
    , contramap
    , defaultConfigStdout
    , defaultConfigTesting
    , mkLOMeta
    , nullTracer
    , setupTrace
    , traceWith
    ) where

import           Control.Tracer (Tracer (..), contramap, nullTracer, traceWith)
import           Cardano.BM.Configuration.Static (defaultConfigStdout,
                     defaultConfigTesting)
import           Cardano.BM.Data.LogItem (LogObject (..),
                     PrivacyAnnotation (..), mkLOMeta)
import           Cardano.BM.Data.Severity (Severity (..))
import           Cardano.BM.Data.Trace (Trace)
import           Cardano.BM.Data.Tracer (ToLogObject (..), ToObject (..),
                     TracingFormatting (..), TracingVerbosity (..),
                     Transformable (..))
import           Cardano.BM.Setup (setupTrace)
import           Cardano.BM.Trace (appendName)

\end{code}
%endif
