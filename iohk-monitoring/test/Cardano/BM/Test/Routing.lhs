
\subsection{Testing scribes and backends configuration routing}
\label{code:Cardano.BM.Test.Routing}

%if style == newcode
\begin{code}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.BM.Test.Routing (
    tests
  ) where

import           Control.Monad (forM_)
import           System.Directory (removeFile)

import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.BackendKind
import           Cardano.BM.Data.LogItem (LoggerName)
import           Cardano.BM.Data.Output
import           Cardano.BM.Data.Severity
import           Cardano.BM.Setup
import           Cardano.BM.Trace

import           Test.Tasty
import           Test.Tasty.HUnit

\end{code}
%endif

\subsubsection{Test declarations}
\begin{code}
tests :: TestTree
tests = testGroup "Routing tests" [
            unit_tests
        ]

unit_tests :: TestTree
unit_tests = testGroup "Unit tests" [
#if ! defined(mingw32_HOST_OS)
        testCase
            "default_scribe_must_log" $
            unit_generic_scribe_backend
                [KatipBK]
                []
                ["FileSK::out-test.txt"]
                []
                5
      , testCase
            "set_backend_must_log" $
            unit_generic_scribe_backend
                []
                [("test.one.alpha", Just [KatipBK])]
                ["FileSK::out-test.txt"]
                []
                1
      , testCase
            "set_scribe_must_log" $
            unit_generic_scribe_backend
                [KatipBK]
                []
                []
                [("test.one.alpha", Just ["FileSK::out-test.txt"])]
                1
      , testCase
            "set_scribe_with_wrong_filename_must_not_log" $
            unit_generic_scribe_backend
                [KatipBK]
                []
                []
                [("test.one.alpha", Just ["FileSK::out-te.txt"])]
                0
      , testCase
            "no_scribe_must_not_log" $
            unit_generic_scribe_backend
                [KatipBK]
                []
                []
                []
                0
#endif
    ]

\end{code}

\subsubsection{Unit tests}

-- -tests- tmp file creation
-- -tests- AggregationBK

Scribes indicated as default should normally log unless there is an explicit declaration for the
specific namespace.

Scribes explicitly declarated for a particular namespace should log.

If no |Scribe| is declared either by default or explicitly then no messages should be logged.

\begin{code}
#if ! defined(mingw32_HOST_OS)
unit_generic_scribe_backend :: [BackendKind]
                            -> [(LoggerName, Maybe [BackendKind])]
                            -> [ScribeId]
                            -> [(LoggerName, Maybe [ScribeId])]
                            -> Int
                            -> Assertion
unit_generic_scribe_backend defaultBackends setBackends defaultScribes setScribes expectedLines = do
    c <- CM.empty
    CM.setMinSeverity c Debug
    CM.setSetupBackends c [KatipBK]
    CM.setSetupScribes c [ ScribeDefinition {
                              scName = "out-test.txt"
                            , scKind = FileSK
                            , scFormat = ScText
                            , scPrivacy = ScPublic
                            , scRotation = Nothing
                            , scMinSev = minBound
                            , scMaxSev = maxBound
                            }
                         ]

    CM.setDefaultBackends c defaultBackends
    CM.setDefaultScribes c defaultScribes

    forM_ setBackends $ \(name, maybeBackends) ->
        CM.setBackends c name maybeBackends
    forM_ setScribes $ \(name, maybeScribes) ->
        CM.setScribes c name maybeScribes

    withTrace c "test" $ \(tr :: Trace IO String) -> do

        let tr1  = appendName "one" tr
            tr2  = appendName "two" tr

            tr1a = appendName "alpha" tr1
            tr2a = appendName "alpha" tr2

        logNotice tr "starting program"

        logNotice tr1  "Hello!"
        logNotice tr2  "Hello!"
        logNotice tr1a "Hello!"
        logNotice tr2a "Hello!"

    contents <- readFile "out-test.txt"
    let numMsgs = length $ lines contents

    removeFile "out-test.txt"

    assertBool
        (" defaultBackends: " ++ show defaultBackends ++
         " setBackends: " ++ show setBackends ++
         " defaultScribe: " ++ show defaultScribes ++
         " setScribes: " ++ show setScribes ++
         " numMsgs: " ++ show numMsgs)
        (numMsgs == expectedLines)
#endif

\end{code}
