
\subsection{Testing configuration}
\label{code:Cardano.BM.Test.Configuration}

%if style == newcode
\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.Test.Configuration (
    tests
  ) where

import           Control.Concurrent.MVar (readMVar)
import           Data.Aeson.Types (Value (..))
import           Data.ByteString (intercalate)
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
                                   , scPrivacy = ScPublic
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
    encode r @?=
        (intercalate "\n"
            [ "rotation:"
            , "  rpLogLimitBytes: 5000000"
            , "  rpKeepFilesNum: 10"
            , "  rpMaxAgeHours: 24"
            , "defaultBackends:"
            , "- KatipBK"
            , "setupBackends:"
            , "- EKGViewBK"
            , "- KatipBK"
            , "hasGUI: 12789"
            , "defaultScribes:"
            , "- - StdoutSK"
            , "  - stdout"
            , "options:"
            , "  test2:"
            , "    value: object2"
            , "  test1:"
            , "    value: object1"
            , "setupScribes:"
            , "- scName: stdout"
            , "  scRotation: null"
            , "  scKind: StdoutSK"
            , "  scPrivacy: ScPublic"
            , "hasEKG: 18321"
            , "minSeverity: Info"
            , "" -- to force a line feed at the end of the file
            ]
        )

unitConfigurationParsedRepresentation :: Assertion
unitConfigurationParsedRepresentation = do
    repr <- parseRepresentation "test/config.yaml"
    encode repr @?=
        (intercalate "\n"
            [ "rotation:"
            , "  rpLogLimitBytes: 5000000"
            , "  rpKeepFilesNum: 10"
            , "  rpMaxAgeHours: 24"
            , "defaultBackends:"
            , "- KatipBK"
            , "setupBackends:"
            , "- AggregationBK"
            , "- EKGViewBK"
            , "- KatipBK"
            , "hasGUI: null"
            , "defaultScribes:"
            , "- - StdoutSK"
            , "  - stdout"
            , "options:"
            , "  mapSubtrace:"
            , "    iohk.benchmarking:"
            , "      tag: ObservableTrace"
            , "      contents:"
            , "      - GhcRtsStats"
            , "      - MonotonicClock"
            , "    iohk.deadend: NoTrace"
            , "  mapSeverity:"
            , "    iohk.startup: Debug"
            , "    iohk.background.process: Error"
            , "    iohk.testing.uncritical: Warning"
            , "  mapAggregatedkinds:"
            , "    iohk.interesting.value: EwmaAK {alpha = 0.75}"
            , "    iohk.background.process: StatsAK"
            , "  cfokey:"
            , "    value: Release-1.0.0"
            , "  mapMonitors:"
            , "    chain.creation.block:"
            , "    - monitor: ((time > (23 s)) Or (time < (17 s)))"
            , "    - actions:"
            , "      - AlterMinSeverity \"chain.creation\" Debug"
            , "    '#aggregation.critproc.observable':"
            , "    - monitor: (mean >= (42))"
            , "    - actions:"
            , "      - CreateMessage \"exceeded\" \"the observable has been too long too high!\""
            , "      - AlterGlobalMinSeverity Info"
            , "  mapScribes:"
            , "    iohk.interesting.value:"
            , "    - StdoutSK::stdout"
            , "    - FileTextSK::testlog"
            , "    iohk.background.process: FileTextSK::testlog"
            , "  mapBackends:"
            , "    iohk.interesting.value:"
            , "    - EKGViewBK"
            , "    - AggregationBK"
            , "setupScribes:"
            , "- scName: testlog"
            , "  scRotation:"
            , "    rpLogLimitBytes: 25000000"
            , "    rpKeepFilesNum: 3"
            , "    rpMaxAgeHours: 24"
            , "  scKind: FileTextSK"
            , "  scPrivacy: ScPrivate"
            , "- scName: stdout"
            , "  scRotation: null"
            , "  scKind: StdoutSK"
            , "  scPrivacy: ScPublic"
            , "hasEKG: 12789"
            , "minSeverity: Info"
            , "" -- to force a line feed at the end of the file
            ]
        )

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
                                                                   ,String "AggregationBK"
                                                                   ])])
            ]
        , cgMapBackend        = HM.fromList [ ("iohk.interesting.value"
                                              , [ EKGViewBK
                                                , AggregationBK
                                                ]
                                              )
                                            ]
        , cgDefBackendKs      = [KatipBK]
        , cgSetupBackends     = [
                                  AggregationBK
                                ,
                                  EKGViewBK
                                , KatipBK]
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
                                    , scPrivacy  = ScPrivate
                                    , scRotation = Just $ RotationParameters
                                                    { rpLogLimitBytes = 25000000
                                                    , rpMaxAgeHours   = 24
                                                    , rpKeepFilesNum  = 3
                                                    }
                                    }
                                , ScribeDefinition
                                    { scKind = StdoutSK
                                    , scName = "stdout"
                                    , scPrivacy = ScPublic
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
