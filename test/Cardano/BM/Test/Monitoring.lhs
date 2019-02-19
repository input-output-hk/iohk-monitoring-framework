
\subsection{Testing parsing of monitoring expressions and actions}

%if style == newcode
\begin{code}
{-# LANGUAGE OverloadedStrings   #-}

module Cardano.BM.Test.Monitoring (
    tests
  ) where

import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)

import           Cardano.BM.Data.Aggregated
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.MonitoringEval
import           Cardano.BM.Data.Severity

import           Test.Tasty
import           Test.Tasty.HUnit

\end{code}
%endif

\subsubsection{Tests}
\begin{code}
tests :: TestTree
tests = testGroup "Monitoring tests" [
            unitTests
        ]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [
                  testCase
                      "parse and eval simple expression; must return False" $
                      parseEvalExpression "(time > (19 s))" False $ HM.fromList [("some", (Seconds 22))]
                , testCase
                      "parse and eval simple expression; must return True" $
                      parseEvalExpression "(time > (19 s))" True $ HM.fromList [("time", (Seconds 20))]
                , testCase
                      "parse and eval OR expression; must return True" $
                      parseEvalExpression "((time > (22 s)) Or (time < (18 s)))" True $ HM.fromList [("time", (Seconds 16))]
                , testCase
                      "parse and eval OR expression; must return True" $
                      parseEvalExpression "((time > (22 s)) Or (time < (18 s)))" True $ HM.fromList [("time", (Seconds 23))]
                , testCase
                      "parse and eval OR expression; must return False" $
                      parseEvalExpression "((time > (22 s)) Or (time < (18 s)))" False $ HM.fromList [("time", (Seconds 21))]
                , testCase
                      "parse and eval AND expression; must return True" $
                      parseEvalExpression "((time > (22 s)) And (lastalert > (300 s)))" True $
                                          HM.fromList [("lastalert", (Seconds 539)), ("time", (Seconds 23))]
            ]

\end{code}

\subsubsection{Unit tests}

\begin{code}
parseEvalExpression :: Text
                    -> Bool
                    -> Environment
                    -> Assertion
parseEvalExpression t res env =
    case parseMaybe t of
        Nothing -> error "failed to parse"
        Just e  -> evaluate env e @?= res

\end{code}
