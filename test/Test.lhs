
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
module Main
  (
    main
  ) where

import           Test.Tasty

import qualified Cardano.BM.Test.Aggregated (tests)
import qualified Cardano.BM.Test.STM (tests)
import qualified Cardano.BM.Test.Trace (tests)
import qualified Cardano.BM.Test.Configuration (tests)
import qualified Cardano.BM.Test.Rotator (tests)
import qualified Cardano.BM.Test.Routing (tests)
import qualified Cardano.BM.Test.Monitoring (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "iohk-monitoring"
  [ Cardano.BM.Test.Aggregated.tests
  , Cardano.BM.Test.STM.tests
  , Cardano.BM.Test.Trace.tests
  , Cardano.BM.Test.Configuration.tests
  , Cardano.BM.Test.Rotator.tests
  , Cardano.BM.Test.Routing.tests
  , Cardano.BM.Test.Monitoring.tests
  ]
\end{code}

\section{Test case generation}
%include ../test/Cardano/BM/Arbitrary/Aggregated.lhs

\section{Tests}

%include ../test/Cardano/BM/Test/Aggregated.lhs
%include ../test/Cardano/BM/Test/STM.lhs
%include ../test/Cardano/BM/Test/Trace.lhs
%include ../test/Cardano/BM/Test/Configuration.lhs
%include ../test/Cardano/BM/Test/Rotator.lhs
