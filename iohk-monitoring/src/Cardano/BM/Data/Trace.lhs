
\subsection{Cardano.BM.Data.Trace}
\label{code:Cardano.BM.Data.Trace}

%if style == newcode
\begin{code}
{-# LANGUAGE RankNTypes        #-}

module Cardano.BM.Data.Trace
  ( Trace
  , TraceContext (..)
  )
  where

import           Cardano.BM.Configuration (Configuration)
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Tracer.Class (Tracer)

\end{code}
%endif

\subsubsection{Trace}\label{code:Trace}\index{Trace}
A |Trace| consists of a |TraceContext| and a |Tracer m (LogObject a)|.
\begin{code}

type Trace m a = (TraceContext, Tracer m (LogObject a))
\end{code}

\subsubsection{TraceContext}\label{code:TraceContext}\index{TraceContext}
\label{code:configuration}\index{TraceContext!configuration}
We keep the context's name and a reference to the |Configuration|
in the |TraceContext|.
\begin{code}

data TraceContext = TraceContext
    { configuration :: Configuration
    }

\end{code}
