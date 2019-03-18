
\begin{code}

module Main
  (
    main
  ) where

import           Test.Tasty

import qualified Cardano.BM.Test.Tracer (tests)


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "basic-tracer"
    [
      Cardano.BM.Test.Tracer.tests
    ]

\end{code}

