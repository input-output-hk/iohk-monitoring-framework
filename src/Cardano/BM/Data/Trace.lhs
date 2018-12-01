
\subsection{Cardano.BM.Data.Trace}

%if False
\begin{code}

module Cardano.BM.Data.Trace
  ( Trace
  , TraceNamed
  , TraceContext (..)
  )
  where

import           Cardano.BM.BaseTrace
import           Cardano.BM.Configuration (Configuration)
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Severity

\end{code}
%endif

\subsubsection{Trace}\label{code:Trace}
A |Trace| consists of a \nameref{code:TraceContext} and a \nameref{code:TraceNamed} in |m|.
\begin{code}

type Trace m = (TraceContext, TraceNamed m)
\end{code}

\subsubsection{TraceNamed}\label{code:TraceNamed}
A |TraceNamed| is a specialized \nameref{code:BaseTrace} of type \nameref{code:LogNamed} with payload \nameref{code:LogObject}.
\begin{code}

type TraceNamed m = BaseTrace m (LogNamed LogObject)
\end{code}

\subsubsection{TraceContext}\label{code:TraceContext}
We keep the context's name and a reference to the |Configuration|
in the |TraceContext|.
\begin{code}

data TraceContext = TraceContext {
      loggerName :: LoggerName
    , configuration :: Configuration
    , minSeverity :: Severity
    }

\end{code}
