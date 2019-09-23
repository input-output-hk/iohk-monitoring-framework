
\subsubsection{Test main entry point}

\begin{code}
{-# LANGUAGE CPP #-}

module Main
  (
    main
  ) where

import           Test.Tasty

import qualified Cardano.BM.Test.Monitoring (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "iohk-monitoring"
  [
    Cardano.BM.Test.Monitoring.tests
  ]
\end{code}

\subsubsection{Tests}

%include ../test/Cardano/BM/Test/Monitoring.lhs
