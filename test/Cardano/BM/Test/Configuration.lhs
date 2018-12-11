
\subsection{Testing configuration}

%if style == newcode
\begin{code}
{-# LANGUAGE OverloadedStrings   #-}

module Cardano.BM.Test.Configuration (
    tests
  ) where

import qualified Data.HashMap.Strict as HM
import           Data.Yaml

-- import qualified Cardano.BM.Configuration as Config
import           Cardano.BM.Data.Configuration

import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.Output
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.Rotation

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

\end{code}
%endif

\subsubsection{Test declarations}
\begin{code}
tests :: TestTree
tests = testGroup "config tests" [
            property_tests
          , unit_tests
        ]

property_tests :: TestTree
property_tests = testGroup "Properties" [
        testProperty "minimal" prop_Configuration_minimal
    ]

unit_tests :: TestTree
unit_tests = testGroup "Unit tests" [
        testCase "static_representation" unit_Configuration_static_representation
      , testCase "parsed_representation" unit_Configuration_parsed_representation
    ]

\end{code}

\subsubsection{Property tests}
\begin{code}
prop_Configuration_minimal :: Bool
prop_Configuration_minimal = True

\end{code}

\subsubsection{Unit tests}
\begin{code}
unit_Configuration_static_representation :: Assertion
unit_Configuration_static_representation =
    let r = Representation
            { minSeverity = Info
            , rotation = RotationParameters 5000000 24 10
            , setupScribes =
                [ ScribeDefinition { scName = "stdout"
                                , scKind = StdoutSK
                                , scRotation = Nothing }
                ]
            , defaultScribes = [(StdoutSK, "stdout")]
            , setupBackends = [ EKGViewBK, KatipBK ]
            , defaultBackends = [ KatipBK ]
            , hasGUI = Just 12789
            , hasEKG = Just 18321
            , options =
                HM.fromList [ ("test1", (HM.singleton "value" "object1"))
                            , ("test2", (HM.singleton "value" "object2")) ]
            }
    in
    encode r @?= "\
                 \rotation:\n\
                 \  rpLogLimitBytes: 5000000\n\
                 \  rpKeepFilesNum: 10\n\
                 \  rpMaxAgeHours: 24\n\
                 \defaultBackends:\n\
                 \- KatipBK\nsetupBackends:\n\
                 \- EKGViewBK\n\
                 \- KatipBK\n\
                 \hasGUI: 12789\n\
                 \defaultScribes:\n\
                 \- - StdoutSK\n\
                 \  - stdout\n\
                 \options:\n\
                 \  test2:\n\
                 \    value: object2\n\
                 \  test1:\n\
                 \    value: object1\n\
                 \setupScribes:\n\
                 \- scName: stdout\n\
                 \  scRotation: null\n\
                 \  scKind: StdoutSK\n\
                 \hasEKG: 18321\n\
                 \minSeverity: Info\n"

unit_Configuration_parsed_representation :: Assertion
unit_Configuration_parsed_representation = do
    repr <- parseRepresentation "test/config.yaml"
    encode repr @?= "\
                    \rotation:\n\
                    \  rpLogLimitBytes: 5000000\n\
                    \  rpKeepFilesNum: 10\n\
                    \  rpMaxAgeHours: 24\n\
                    \defaultBackends:\n\
                    \- KatipBK\n\
                    \setupBackends:\n\
                    \- AggregationBK\n\
                    \- EKGViewBK\n\
                    \- KatipBK\n\
                    \hasGUI: null\n\
                    \defaultScribes:\n\
                    \- - StdoutSK\n\
                    \  - stdout\n\
                    \options:\n\
                    \  cfokey:\n\
                    \    value: Release-1.0.0\n\
                    \setupScribes:\n\
                    \- scName: testlog\n\
                    \  scRotation:\n\
                    \    rpLogLimitBytes: 25000000\n\
                    \    rpKeepFilesNum: 3\n\
                    \    rpMaxAgeHours: 24\n\
                    \  scKind: FileTextSK\n\
                    \- scName: stdout\n\
                    \  scRotation: null\n\
                    \  scKind: StdoutSK\n\
                    \hasEKG: 12789\n\
                    \minSeverity: Info\n"

\end{code}
