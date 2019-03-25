
\subsection{Cardano.BM.Data.Trace}
\label{code:Cardano.BM.Data.Trace}

%if style == newcode
\begin{code}
{-# LANGUAGE RankNTypes        #-}

module Cardano.BM.Data.Trace
  ( Trace
  )
  where

import           Cardano.BM.Data.LogItem
import           Cardano.BM.Tracer (Tracer)

\end{code}
%endif

\subsubsection{Trace}\label{code:Trace}\index{Trace}
A |Trace m a| is a |Tracer m (LogObject a)|.
\begin{code}

type Trace m a = Tracer m (LogObject a)
\end{code}
