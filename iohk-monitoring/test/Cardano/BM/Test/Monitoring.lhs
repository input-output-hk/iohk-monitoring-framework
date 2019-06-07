
\subsection{Testing parsing of monitoring expressions and actions}

%if style == newcode
\begin{code}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.BM.Test.Monitoring (
    tests
  ) where

import           Control.Monad
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent (threadDelay)
import qualified Data.HashMap.Strict as HM
import           Data.Either (isRight)
import           Data.Text (Text)

import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.Aggregated
import           Cardano.BM.Data.BackendKind
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.MonitoringEval
import           Cardano.BM.Data.Severity
import           Cardano.BM.Output.Monitoring
import           Cardano.BM.Setup
import           Cardano.BM.Trace

import           Test.Tasty
import           Test.Tasty.HUnit

\end{code}
%endif

\subsubsection{Tests}
\begin{code}
tests :: TestTree
tests = testGroup "Monitoring tests" [
              unitTests
            , actionsTests
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
                      "parse and eval expression with algebra, measurable * measurable; must return True" $
                      parseEvalExpression "(time > ((19 s) * (10 s)))"
                                          True
                                          $ HM.fromList [("time", Seconds 191)]
                , testCase
                      "parse and eval expression with algebra, measurable - measurable, wrong result; must return False" $
                      parseEvalExpression "(time > ((19 s) - (10 s)))"
                                          False
                                          $ HM.fromList [("time", Seconds 1)]
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
                      "parse and eval expression with algebra, measurable * variable; must return True" $
                      parseEvalExpression "(time >= ((15 s) * stats.mean))"
                                          True
                                          $ HM.fromList [ ("time",       Seconds 75)
                                                        , ("stats.mean", Seconds 5)
                                                        ]
                , testCase
                      "parse and eval expression with algebra, measurable + variable, wrong result; must return False" $
                      parseEvalExpression "(time == ((19 s) - stats.mean))"
                                          False
                                          $ HM.fromList [ ("time",       Seconds 100)
                                                        , ("stats.mean", Seconds 2)
                                                        ]
                , testCase
                      "parse and eval expression with algebra, measurable - variable; must return True" $
                      parseEvalExpression "(time<=((100 ns)+ stats.mean))"
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
                      "parse and eval expression, with variable, wrong result; must return False" $
                      parseEvalExpression "(time>( stats.mean)    )"
                                          False
                                          $ HM.fromList [ ("time",       Seconds 2)
                                                        , ("stats.mean", Seconds 90)
                                                        ]
                , testCase
                      "parse and eval expression with algebra, variable + measurable; must return True" $
                      parseEvalExpression "(  time<(stats.mean+(      10 s)      ))"
                                          True
                                          $ HM.fromList [ ("time",       Seconds 9)
                                                        , ("stats.mean", Seconds 2)
                                                        ]
                , testCase
                      "parse and eval expression with algebra, variable * measurable; must return True" $
                      parseEvalExpression "(  time==(stats.mean*(      10 s)      ))"
                                          True
                                          $ HM.fromList [ ("time",       Seconds 20)
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
                , testCase
                      "parse and eval expression with algebra, variable - variable, wrong result; must return False" $
                      parseEvalExpression "(time < (stats.mean-stats.min))"
                                          False
                                          $ HM.fromList [ ("time",       Seconds 300)
                                                        , ("stats.mean", Seconds 20)
                                                        , ("stats.min",  Seconds 2)
                                                        ]
                , testCase
                      "parse and eval expression with algebra, variable * variable, wrong result; must return False" $
                      parseEvalExpression "(time < (stats.mean*stats.min))"
                                          False
                                          $ HM.fromList [ ("time",       Seconds 300)
                                                        , ("stats.mean", Seconds 20)
                                                        , ("stats.min",  Seconds 2)
                                                        ]
            ]

actionsTests :: TestTree
actionsTests = testGroup "Actions tests" [
                     testCase
                         "test SetGlobalMinimalSeverity" $
                         testSetGlobalMinimalSeverity
                   , testCase
                         "test AlterSeverity" $
                         testAlterSeverity
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

\subsubsection{Actions tests}

\begin{code}
monitoringThr :: Trace IO Text -> IO (Async.Async ())
monitoringThr trace = do
    trace' <- appendName "monitoring" trace
    proc <- Async.async $ sendTo trace'
    return proc
  where
    sendTo tr = do
        (,) <$> (mkLOMeta Warning Public)
            <*> pure (LogValue "monitMe" (PureI 100))
        >>= traceNamedObject tr

testSetGlobalMinimalSeverity :: Assertion
testSetGlobalMinimalSeverity = do
    let initialGlobalSeverity = Debug
        targetGlobalSeverity  = Info

    c <- CM.empty
    CM.setMinSeverity c initialGlobalSeverity
    CM.setDefaultBackends c [MonitoringBK]
    CM.setSetupBackends c [MonitoringBK]

    CM.setBackends c "complex.monitoring.monitMe" (Just [MonitoringBK])

    CM.setMonitors c $ HM.fromList
        [ ( "complex.monitoring"
          , ( Nothing
            , Compare "monitMe" (GE, (OpMeasurable 10))
            , [SetGlobalMinimalSeverity targetGlobalSeverity]
            )
          )
        ]

    (tr' :: Trace IO Text, _) <- setupTrace_ c "complex"

    procMonitoring <- monitoringThr tr'
    _ <- Async.waitCatch procMonitoring

    threadDelay 1000000  -- 1 second
    currentGlobalSeverity <- CM.minSeverity c
    assertBool "Global minimal severity didn't change!" $
        currentGlobalSeverity == targetGlobalSeverity

testAlterSeverity :: Assertion
testAlterSeverity = do
    let initialSeverity = Debug
        targetSeverity  = Info

    c <- CM.empty
    CM.setSeverity c "complex.monitoring.monitMe" (Just initialSeverity)
    CM.setDefaultBackends c [MonitoringBK]
    CM.setSetupBackends c [MonitoringBK]

    CM.setBackends c "complex.monitoring.monitMe" (Just [MonitoringBK])

    CM.setMonitors c $ HM.fromList
        [ ( "complex.monitoring"
          , ( Nothing
            , Compare "monitMe" (GE, (OpMeasurable 10))
            , [AlterSeverity "complex.monitoring.monitMe" targetSeverity]
            )
          )
        ]

    (tr' :: Trace IO Text, _) <- setupTrace_ c "complex"

    procMonitoring <- monitoringThr tr'
    _ <- Async.waitCatch procMonitoring

    threadDelay 1000000  -- 1 second
    Just currentSeverity <- CM.inspectSeverity c "complex.monitoring.monitMe"
    assertBool "Severity didn't change!" $ targetSeverity == currentSeverity

\end{code}
