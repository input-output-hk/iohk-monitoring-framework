
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
import           Cardano.BM.Data.MonitoringEval

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
                      parseEvalExpression "((time > (22 s)) Or (time < (18 s)))"
                                          True
                                          $ HM.fromList [("time", Seconds 23)]
                , testCase
                      "parse and eval OR expression; must return False" $
                      parseEvalExpression "((time > (22 s)) Or (time < (18 s)))"
                                          False
                                          $ HM.fromList [("time", Seconds 21)]
                , testCase
                      "parse and eval AND expression; must return True" $
                      parseEvalExpression "((time > (22 s)) And (lastalert > (300 s)))"
                                          True
                                          $ HM.fromList [ ("lastalert", Seconds 539)
                                                        , ("time",      Seconds 23)
                                                        ]
                , testCase
                      "parse and eval expression with algebra, measurable + measurable; must return True" $
                      parseEvalExpression "(time > ((19 s) + (10 s)))"
                                          True
                                          $ HM.fromList [("time", Seconds 30)]
                , testCase
                      "parse and eval expression with algebra, measurable - measurable; must return True" $
                      parseEvalExpression "(time == ((19 s)-(9 s)))"
                                          True
                                          $ HM.fromList [("time", Seconds 10)]
                , testCase
                      "parse and eval expression with algebra, measurable + variable; must return True" $
                      parseEvalExpression "(time > ((19 s) - stats.mean))"
                                          True
                                          $ HM.fromList [ ("time",       Seconds 100)
                                                        , ("stats.mean", Seconds 2)
                                                        ]
                , testCase
                      "parse and eval expression with algebra, measurable - variable; must return True" $
                      parseEvalExpression "(time==((100 ns)+ stats.mean))"
                                          True
                                          $ HM.fromList [ ("time",       Nanoseconds 150)
                                                        , ("stats.mean", Nanoseconds 50)
                                                        ]
                , testCase
                      "parse and eval expression, with variable; must return True" $
                      parseEvalExpression "(time> (stats.mean  )    )"
                                          True
                                          $ HM.fromList [ ("time",       Seconds 10)
                                                        , ("stats.mean", Seconds 9)
                                                        ]
                , testCase
                      "parse and eval expression with algebra, variable + measurable; must return True" $
                      parseEvalExpression "(  time<(stats.mean+(      10 s)      ))"
                                          True
                                          $ HM.fromList [ ("time",       Seconds 9)
                                                        , ("stats.mean", Seconds 2)
                                                        ]
                , testCase
                      "parse and eval expression with algebra, variable - variable; must return True" $
                      parseEvalExpression "(time < (stats.mean-stats.min))"
                                          True
                                          $ HM.fromList [ ("time",       Seconds 3)
                                                        , ("stats.mean", Seconds 20)
                                                        , ("stats.min",  Seconds 2)
                                                        ]
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
