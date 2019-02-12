
\subsection{Testing configuration}

%if style == newcode
\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.Test.Configuration (
    tests
  ) where

import           Control.Concurrent.MVar (readMVar)
import           Data.Aeson.Types (Value (..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import           Data.Yaml

import           Cardano.BM.Data.Configuration
import           Cardano.BM.Configuration.Model (Configuration (..),
                     ConfigurationInternal (..), getScribes, getCachedScribes,
                     getDefaultBackends, getAggregatedKind, getGUIport,
                     getEKGport, empty, setDefaultScribes, setScribes, setup,
                     setDefaultAggregatedKind, setAggregatedKind, setGUIport,
                     setEKGport)
import           Cardano.BM.Configuration.Static (defaultConfigStdout)
import qualified Cardano.BM.Data.Aggregated as Agg
import           Cardano.BM.Data.AggregatedKind
import           Cardano.BM.Data.BackendKind
import           Cardano.BM.Data.MonitoringEval
import           Cardano.BM.Data.Output
import           Cardano.BM.Data.Observable (ObservableInstance (..))
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.SubTrace (SubTrace (..))
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
            propertyTests
          , unitTests
        ]

propertyTests :: TestTree
propertyTests = testGroup "Properties" [
        testProperty "minimal" prop_Configuration_minimal
    ]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [
        testCase "static representation" unitConfigurationStaticRepresentation
      , testCase "parsed representation" unitConfigurationParsedRepresentation
      , testCase "parsed configuration" unitConfigurationParsed
      , testCase "include EKG if defined" unitConfigurationCheckEKGpositive
      , testCase "not include EKG if not def" unitConfigurationCheckEKGnegative
      , testCase "check scribe caching" unitConfigurationCheckScribeCache
      , testCase "test ops on Configuration" unitConfigurationOps
    ]

\end{code}

\subsubsection{Property tests}
\begin{code}
prop_Configuration_minimal :: Bool
prop_Configuration_minimal = True

\end{code}

\subsubsection{Unit tests}

The configuration file only indicates that EKG is listening on port nnnnn. Infer that
|EKGViewBK| needs to be started as a backend.

\begin{code}
unitConfigurationCheckEKGpositive :: Assertion
unitConfigurationCheckEKGpositive = do
    let c = [ "rotation:"
            , "  rpLogLimitBytes: 5000000"
            , "  rpKeepFilesNum: 10"
            , "  rpMaxAgeHours: 24"
            , "minSeverity: Info"
            , "defaultBackends:"
            , "  - KatipBK"
            , "setupBackends:"
            , "  - KatipBK"
            , "defaultScribes:"
            , "- - StdoutSK"
            , "  - stdout"
            , "setupScribes:"
            , "- scName: stdout"
            , "  scRotation: null"
            , "  scKind: StdoutSK"
            , "hasEKG: 18321"
            , "options:"
            , "  test:"
            , "    value: nothing"
            ]
        fp = "/tmp/test_ekgv_config.yaml"
    writeFile fp $ unlines c
    repr <- parseRepresentation fp

    assertBool "expecting EKGViewBK to be setup" $
        EKGViewBK `elem` (setupBackends repr)

\end{code}

If there is no port defined for EKG, then do not start it even if present in the config.
\begin{code}
unitConfigurationCheckEKGnegative :: Assertion
unitConfigurationCheckEKGnegative = do
    let c = [ "rotation:"
            , "  rpLogLimitBytes: 5000000"
            , "  rpKeepFilesNum: 10"
            , "  rpMaxAgeHours: 24"
            , "minSeverity: Info"
            , "defaultBackends:"
            , "  - KatipBK"
            , "  - EKGViewBK"
            , "setupBackends:"
            , "  - KatipBK"
            , "  - EKGViewBK"
            , "defaultScribes:"
            , "- - StdoutSK"
            , "  - stdout"
            , "setupScribes:"
            , "- scName: stdout"
            , "  scRotation: null"
            , "  scKind: StdoutSK"
            , "###hasEKG: 18321"
            , "options:"
            , "  test:"
            , "    value: nothing"
            ]
        fp = "/tmp/test_ekgv_config.yaml"
    writeFile fp $ unlines c
    repr <- parseRepresentation fp

    assertBool "EKGViewBK shall not be setup" $
        not $ EKGViewBK `elem` (setupBackends repr)
    assertBool "EKGViewBK shall not receive messages" $
        not $ EKGViewBK `elem` (defaultBackends repr)

\end{code}

\begin{code}
unitConfigurationStaticRepresentation :: Assertion
unitConfigurationStaticRepresentation =
    let r = Representation
            { minSeverity = Info
            , rotation = Just $ RotationParameters
                                    { rpLogLimitBytes = 5000000
                                    , rpMaxAgeHours   = 24
                                    , rpKeepFilesNum  = 10
                                    }
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
                 \- KatipBK\n\
                 \setupBackends:\n\
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

unitConfigurationParsedRepresentation :: Assertion
unitConfigurationParsedRepresentation = do
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
                    \  mapSubtrace:\n\
                    \    iohk.benchmarking:\n\
                    \      tag: ObservableTrace\n\
                    \      contents:\n\
                    \      - GhcRtsStats\n\
                    \      - MonotonicClock\n\
                    \    iohk.deadend: NoTrace\n\
                    \  mapSeverity:\n\
                    \    iohk.startup: Debug\n\
                    \    iohk.background.process: Error\n\
                    \    iohk.testing.uncritical: Warning\n\
                    \  mapAggregatedkinds:\n\
                    \    iohk.interesting.value: EwmaAK {alpha = 0.75}\n\
                    \    iohk.background.process: StatsAK\n\
                    \  cfokey:\n\
                    \    value: Release-1.0.0\n\
                    \  mapMonitors:\n\
                    \    chain.creation.block:\n\
                    \    - monitor: ((time > (23 s)) Or (time < (17 s)))\n\
                    \    - actions:\n\
                    \      - AlterMinSeverity \"chain.creation\" Debug\n\
                    \    ! '#aggregation.critproc.observable':\n\
                    \    - monitor: (mean >= (42))\n\
                    \    - actions:\n\
                    \      - CreateMessage \"exceeded\" \"the observable has been too long too high!\"\n\
                    \      - AlterGlobalMinSeverity Info\n\
                    \  mapScribes:\n\
                    \    iohk.interesting.value:\n\
                    \    - StdoutSK::stdout\n\
                    \    - FileTextSK::testlog\n\
                    \    iohk.background.process: FileTextSK::testlog\n\
                    \  mapBackends:\n\
                    \    iohk.interesting.value:\n\
                    \    - EKGViewBK\n\
                    \    - AggregationBK\n\
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

unitConfigurationParsed :: Assertion
unitConfigurationParsed = do
    cfg <- setup "test/config.yaml"
    cfgInternal <- readMVar $ getCG cfg
    cfgInternal @?= ConfigurationInternal
        { cgMinSeverity       = Info
        , cgMapSeverity       = HM.fromList [ ("iohk.startup", Debug)
                                            , ("iohk.background.process", Error)
                                            , ("iohk.testing.uncritical", Warning)
                                            ]
        , cgMapSeverityCache  = HM.fromList [ ("iohk.startup", Debug)
                                            , ("iohk.background.process", Error)
                                            , ("iohk.testing.uncritical", Warning)
                                            ]
        , cgMapSubtrace       = HM.fromList [ ("iohk.benchmarking",
                                                    ObservableTrace [GhcRtsStats, MonotonicClock])
                                            , ("iohk.deadend", NoTrace)
                                            ]
        , cgOptions           = HM.fromList
            [ ("mapSubtrace",
                HM.fromList [("iohk.benchmarking",
                              Object (HM.fromList [("tag",String "ObservableTrace")
                                                  ,("contents",Array $ V.fromList
                                                        [String "GhcRtsStats"
                                                        ,String "MonotonicClock"])]))
                            ,("iohk.deadend",String "NoTrace")])
            , ("mapMonitors", HM.fromList [("chain.creation.block",Array $ V.fromList
                                            [Object (HM.fromList [("monitor",String "((time > (23 s)) Or (time < (17 s)))")])
                                            ,Object (HM.fromList [("actions",Array $ V.fromList
                                                [String "AlterMinSeverity \"chain.creation\" Debug"])])])
                                          ,("#aggregation.critproc.observable",Array $ V.fromList
                                            [Object (HM.fromList [("monitor",String "(mean >= (42))")])
                                            ,Object (HM.fromList [("actions",Array $ V.fromList
                                                [String "CreateMessage \"exceeded\" \"the observable has been too long too high!\""
                                                ,String "AlterGlobalMinSeverity Info"])])])])
            , ("mapSeverity", HM.fromList [("iohk.startup",String "Debug")
                                          ,("iohk.background.process",String "Error")
                                          ,("iohk.testing.uncritical",String "Warning")])
            , ("mapAggregatedkinds", HM.fromList [("iohk.interesting.value",
                                                        String "EwmaAK {alpha = 0.75}")
                                                 ,("iohk.background.process",
                                                        String "StatsAK")])
            , ("cfokey",HM.fromList [("value",String "Release-1.0.0")])
            , ("mapScribes", HM.fromList [("iohk.interesting.value",
                                            Array $ V.fromList [String "StdoutSK::stdout"
                                                               ,String "FileTextSK::testlog"])
                                         ,("iohk.background.process",String "FileTextSK::testlog")])
            , ("mapBackends", HM.fromList [("iohk.interesting.value",
                                                Array $ V.fromList [String "EKGViewBK"
                                                                   ,String "AggregationBK"])])
            ]
        , cgMapBackend        = HM.fromList [ ("iohk.interesting.value", [EKGViewBK,AggregationBK]) ]
        , cgDefBackendKs      = [KatipBK]
        , cgSetupBackends     = [AggregationBK, EKGViewBK, KatipBK]
        , cgMapScribe         = HM.fromList [ ("iohk.interesting.value",
                                                    ["StdoutSK::stdout","FileTextSK::testlog"])
                                            , ("iohk.background.process", ["FileTextSK::testlog"])
                                            ]
        , cgMapScribeCache    = HM.fromList [ ("iohk.interesting.value",
                                                    ["StdoutSK::stdout","FileTextSK::testlog"])
                                            , ("iohk.background.process", ["FileTextSK::testlog"])
                                            ]
        , cgDefScribes        = ["StdoutSK::stdout"]
        , cgSetupScribes      = [ ScribeDefinition
                                    { scKind     = FileTextSK
                                    , scName     = "testlog"
                                    , scRotation = Just $ RotationParameters
                                                    { rpLogLimitBytes = 25000000
                                                    , rpMaxAgeHours   = 24
                                                    , rpKeepFilesNum  = 3
                                                    }
                                    }
                                , ScribeDefinition
                                    { scKind = StdoutSK
                                    , scName = "stdout"
                                    , scRotation = Nothing
                                    }
                                ]
        , cgMapAggregatedKind = HM.fromList [ ("iohk.interesting.value", EwmaAK {alpha = 0.75} )
                                            , ("iohk.background.process", StatsAK)
                                            ]
        , cgDefAggregatedKind = StatsAK
        , cgMonitors          = HM.fromList [ ("chain.creation.block", ((OR (Compare "time" ((>), (Agg.Seconds 23))) (Compare "time" ((<), (Agg.Seconds 17))))
                                                                       , ["AlterMinSeverity \"chain.creation\" Debug"]
                                                                       )
                                              )
                                            , ("#aggregation.critproc.observable", (Compare "mean" ((>=), (Agg.PureI 42))
                                                                                   , ["CreateMessage \"exceeded\" \"the observable has been too long too high!\""
                                                                                   , "AlterGlobalMinSeverity Info"]
                                                                                   )
                                              )
                                            ]
        , cgPortEKG           = 12789
        , cgPortGUI           = 0
        }

\end{code}

Test caching and inheritance of Scribes.
\begin{code}
unitConfigurationCheckScribeCache :: Assertion
unitConfigurationCheckScribeCache = do
    configuration <- empty

    let defScribes = ["FileTextSK::node.log"]
    setDefaultScribes configuration defScribes

    let scribes12 = ["StdoutSK::stdout", "FileTextSK::out.txt"]
    setScribes configuration "name1.name2" $ Just scribes12

    scribes1234 <- getScribes configuration "name1.name2.name3.name4"
    scribes1    <- getScribes configuration "name1"

    scribes1234cached <- getCachedScribes configuration "name1.name2.name3.name4"
    scribesXcached    <- getCachedScribes configuration "nameX"

    assertBool "Scribes for name1.name2.name3.name4 must be the same as name1.name2" $
        scribes1234 == scribes12
    assertBool "Scribes for name1 must be the default ones" $
        scribes1 == defScribes
    assertBool "Scribes for name1.name2.name3.name4 must have been cached" $
        scribes1234cached == Just scribes1234
    assertBool "Scribes for nameX must not have been cached since getScribes was not called" $
        scribesXcached == Nothing

\end{code}


Test operations on Configuration.
\begin{code}
unitConfigurationOps :: Assertion
unitConfigurationOps = do
    configuration <- defaultConfigStdout

    defBackends <- getDefaultBackends configuration

    setDefaultAggregatedKind configuration $ EwmaAK 0.01
    -- since loggername does not exist the default must be inherited
    defAggregatedKind <- getAggregatedKind configuration "non-existent loggername"

    setAggregatedKind configuration "name1" $ Just StatsAK
    name1AggregatedKind <- getAggregatedKind configuration "name1"

    setEKGport configuration 11223
    ekgPort <- getEKGport configuration

    setGUIport configuration 1080
    guiPort <- getGUIport configuration

    assertBool "Default backends" $
        defBackends == [KatipBK]

    assertBool "Default aggregated kind" $
        defAggregatedKind == EwmaAK 0.01

    assertBool "Specific name aggregated kind" $
        name1AggregatedKind == StatsAK

    assertBool "Set EKG port" $
        ekgPort == 11223

    assertBool "Set GUI port" $
        guiPort == 1080

\end{code}
