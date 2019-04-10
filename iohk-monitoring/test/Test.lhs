
\section{Test coverage}

Test coverage is calculated as the fraction of functions which are called from test routines.
This percentage is calculated by the tool |hpc| with a call to
\begin{verbatim}cabal new-test\end{verbatim}
Add to a local \tt{cabal.project.local} file these lines:
\newline
\begin{tabular}{l r}
tests: & True \\
coverage: & True \\
library-coverage: & True \\
\end{tabular}

\begin{figure}[htp]
\centering{
  \input{test-coverage.tex}
}
\caption{Test coverage of modules in percent as computed by the tool 'hpc'}\label{fig:coverage}
\end{figure}


\section{Test main entry point}

\begin{code}
{-# LANGUAGE CPP #-}

module Main
  (
    main
  ) where

main :: IO ()
main = return ()

\end{code}

\section{Test case generation}
%include ../test/Cardano/BM/Arbitrary/Aggregated.lhs

\section{Tests}

%include ../test/Cardano/BM/Test/Aggregated.lhs
%include ../test/Cardano/BM/Test/STM.lhs
%include ../test/Cardano/BM/Test/Trace.lhs
%include ../test/Cardano/BM/Test/Configuration.lhs
%include ../test/Cardano/BM/Test/Rotator.lhs
%include ../test/Cardano/BM/Test/Structured.lhs
%include ../test/Cardano/BM/Test/Tracer.lhs
