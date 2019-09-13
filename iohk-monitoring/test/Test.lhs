
\section{Test main entry point}

\begin{code}
{-# LANGUAGE CPP #-}

module Main
  (
    main
  ) where

import           Test.Tasty

#ifdef ENABLE_AGGREGATION
import qualified Cardano.BM.Test.Aggregated (tests)
#endif
import qualified Cardano.BM.Test.STM (tests)
import qualified Cardano.BM.Test.Trace (tests)
import qualified Cardano.BM.Test.Configuration (tests)
import qualified Cardano.BM.Test.LogItem (tests)
import qualified Cardano.BM.Test.Rotator (tests)
import qualified Cardano.BM.Test.Routing (tests)
import qualified Cardano.BM.Test.Structured (tests)
import qualified Cardano.BM.Test.Tracer (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "iohk-monitoring"
  [
#ifdef ENABLE_AGGREGATION
    Cardano.BM.Test.Aggregated.tests
  ,
#endif
    Cardano.BM.Test.STM.tests
  , Cardano.BM.Test.Trace.tests
  , Cardano.BM.Test.Configuration.tests
  , Cardano.BM.Test.LogItem.tests
  , Cardano.BM.Test.Rotator.tests
  , Cardano.BM.Test.Routing.tests
  , Cardano.BM.Test.Structured.tests
  , Cardano.BM.Test.Tracer.tests
  ]
\end{code}

\section{Test case generation}
%include ../test/Cardano/BM/Arbitrary/Aggregated.lhs

\section{Tests}

%include ../test/Cardano/BM/Test/LogItem.lhs
%include ../test/Cardano/BM/Test/Aggregated.lhs
%include ../test/Cardano/BM/Test/STM.lhs
%include ../test/Cardano/BM/Test/Trace.lhs
%include ../test/Cardano/BM/Test/Configuration.lhs
%include ../test/Cardano/BM/Test/Rotator.lhs
%include ../test/Cardano/BM/Test/Structured.lhs
%include ../test/Cardano/BM/Test/Tracer.lhs
