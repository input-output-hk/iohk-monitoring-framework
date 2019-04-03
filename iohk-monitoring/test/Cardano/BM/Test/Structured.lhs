
\subsection{Cardano.BM.Test.Structured}
\label{code:Cardano.BM.Test.Structured}

%if style == newcode
\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.BM.Test.Structured (
    tests
  ) where

import           Data.Text (Text)

import           Cardano.BM.Configuration.Static (defaultConfigTesting)
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Observable
import           Cardano.BM.Data.Output
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.SubTrace
import           Cardano.BM.Data.Tracer
import qualified Cardano.BM.Setup as Setup
import           Cardano.BM.Trace (Trace, appendName, evalFilters, logDebug,
                     logInfo, logInfoS, logNotice, logWarning, logError,
                     logCritical, logAlert, logEmergency,
                     traceInTVarIOConditionally)

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (Assertion, assertBool, testCase,
                     testCaseInfo)

\end{code}
%endif

\begin{code}
tests :: TestTree
tests = testGroup "Testing Structured Logging" [
            testCase "logging simple text" logText
    --   , testCase "logging data structures" logStructures
        ]

\end{code}

\subsubsection{Simple logging of text.}\label{code:logText}
\begin{code}
logText :: Assertion
logText = do
    cfg <- defaultConfigTesting
    baseTrace :: Trace IO Text <- Setup.setupTrace (Right cfg) "logText"
    
    let logTrace = toLogObject $ baseTrace  

    traceWith logTrace "This is a simple message."
    traceWith logTrace ".. and another!"

    assertBool "OK" True
\end{code}
