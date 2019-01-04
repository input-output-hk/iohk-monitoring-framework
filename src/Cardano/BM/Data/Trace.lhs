
\subsection{Cardano.BM.Data.Trace}

%if style == newcode
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
import           Cardano.BM.Data.SubTrace
import           Cardano.BM.Output.Switchboard (Switchboard)
\end{code}
%endif

\subsubsection{Trace}\label{code:Trace}\index{Trace}
A |Trace| consists of a \nameref{code:TraceContext} and a TraceNamed in |m|.
\begin{code}

type Trace m = (TraceContext, TraceNamed m)
\end{code}

\subsubsection{TraceNamed}\label{code:TraceNamed}\index{TraceNamed}
A |TraceNamed| is a specialized \nameref{code:BaseTrace} of type \nameref{code:LogNamed} with payload LogObject.
\begin{code}

type TraceNamed m = BaseTrace m (LogNamed LogObject)
\end{code}

\subsubsection{TraceContext}\label{code:TraceContext}\index{TraceContext}
\label{code:loggerName}\index{TraceContext!loggerName}
\label{code:configuration}\index{TraceContext!configuration}
\label{code:tracetype}\index{TraceContext!tracetype}
\label{code:minSeverity}\index{TraceContext!minSeverity}
\label{code:switchboard}\index{TraceContext!switchboard}
We keep the context's name and a reference to the |Configuration|
in the |TraceContext|.
\begin{code}

data TraceContext = TraceContext
    { loggerName    :: LoggerName
    , configuration :: Configuration
    , tracetype     :: SubTrace
    , minSeverity   :: Severity
    , switchboard   :: Switchboard
    }

\end{code}
