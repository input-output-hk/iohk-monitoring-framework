
\subsection{Testing configuration}
\label{code:Cardano.BM.Test.Configuration}

%if style == newcode
\begin{code}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.Test.Configuration (
    tests
  ) where

import           Prelude hiding (Ordering (..))

import           Control.Arrow ((***))
import           Control.Concurrent.MVar (readMVar)
import           Data.Aeson.Types (Value (..))
import           Data.ByteString (intercalate)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import           Data.Yaml
import           System.IO.Temp (withSystemTempFile)

import           Cardano.BM.Data.Configuration
import           Cardano.BM.Configuration.Model (Configuration (..),
                     ConfigurationInternal (..), getScribes, getCachedScribes,
                     getDefaultBackends, getAggregatedKind, getGUIport,
                     getEKGport, empty, setDefaultScribes, setScribes, setup,
                     setDefaultAggregatedKind, setAggregatedKind, setGUIport,
                     setEKGport, exportConfiguration)
import           Cardano.BM.Configuration.Static (defaultConfigStdout)
import qualified Cardano.BM.Data.Aggregated as Agg
import           Cardano.BM.Data.AggregatedKind
import           Cardano.BM.Data.BackendKind
import           Cardano.BM.Data.LogItem
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
      , testCase "export configuration: from file" unitConfigurationExport
      , testCase "export configuration: defaultConfigStdout" unitConfigurationExportStdout
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
                                   , scFormat   = ScText
                                   , scPrivacy = ScPublic
                                   , scRotation = Nothing }
                ]
            , defaultScribes = [(StdoutSK, "stdout")]
            , setupBackends = [ EKGViewBK, KatipBK ]
            , defaultBackends = [ KatipBK ]
            , hasGUI = Just 12789
            , hasGraylog = Just 12788
            , hasEKG = Just 18321
            , hasPrometheus = Just ("localhost", 12799)
            , traceForwardTo = Just (RemotePipe "to")
            , traceAcceptAt = Just [RemoteAddrNamed "a" (RemotePipe "at")]
            , options =
                HM.fromList [ ("test1", Object (HM.singleton "value" "object1"))
                            , ("test2", Object (HM.singleton "value" "object2")) ]
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
            , "hasPrometheus:"
            , "- localhost"
            , "- 12799"
            , "hasGraylog: 12788"
            , "hasGUI: 12789"
            , "traceForwardTo:"
            , "  tag: RemotePipe"
            , "  contents: to"
            , "traceAcceptAt:"
            , "- remoteAddr:"
            , "    tag: RemotePipe"
            , "    contents: at"
            , "  nodeName: a"
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
            , "  scFormat: ScText"
            , "  scPrivacy: ScPublic"
            , "hasEKG: 18321"
            , "minSeverity: Info"
            , "" -- to force a line feed at the end of the file
            ]
        )

unitConfigurationParsedRepresentation :: Assertion
unitConfigurationParsedRepresentation = do
    repr <- readRepresentation "test/config.yaml"
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
            , "hasPrometheus: null"
            , "hasGraylog: 12788"
            , "hasGUI: null"
            , "traceForwardTo:"
            , "  tag: RemotePipe"
            , "  contents: to"
            , "traceAcceptAt:"
            , "- remoteAddr:"
            , "    tag: RemotePipe"
            , "    contents: at"
            , "  nodeName: a"
            , "defaultScribes:"
            , "- - StdoutSK"
            , "  - stdout"
            , "options:"
            , "  mapSubtrace:"
            , "    iohk.benchmarking:"
            , "      contents:"
            , "      - GhcRtsStats"
            , "      - MonotonicClock"
            , "      subtrace: ObservableTraceSelf"
            , "    iohk.deadend:"
            , "      subtrace: NoTrace"
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
            , "      actions:"
            , "      - CreateMessage Warning \"chain.creation\""
            , "      - AlterSeverity \"chain.creation\" Debug"
            , "      monitor: ((time > (23 s)) Or (time < (17 s)))"
            , "    '#aggregation.critproc.observable':"
            , "      actions:"
            , "      - CreateMessage Warning \"the observable has been too long too high!\""
            , "      - SetGlobalMinimalSeverity Info"
            , "      monitor: (mean >= (42))"
            , "  mapScribes:"
            , "    iohk.interesting.value:"
            , "    - StdoutSK::stdout"
            , "    - FileSK::testlog"
            , "    iohk.background.process: FileSK::testlog"
            , "  mapBackends:"
            , "    iohk.user.defined:"
            , "    - kind: UserDefinedBK"
            , "      name: MyBackend"
            , "    - KatipBK"
            , "    iohk.interesting.value:"
            , "    - EKGViewBK"
            , "    - AggregationBK"
            , "setupScribes:"
            , "- scName: testlog"
            , "  scRotation:"
            , "    rpLogLimitBytes: 25000000"
            , "    rpKeepFilesNum: 3"
            , "    rpMaxAgeHours: 24"
            , "  scKind: FileSK"
            , "  scFormat: ScText"
            , "  scPrivacy: ScPrivate"
            , "- scName: stdout"
            , "  scRotation: null"
            , "  scKind: StdoutSK"
            , "  scFormat: ScText"
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
        , cgDefRotation       = Just $ RotationParameters
                                        { rpLogLimitBytes = 5000000
                                        , rpMaxAgeHours   = 24
                                        , rpKeepFilesNum  = 10
                                        }
        , cgMapSeverity       = HM.fromList $ map (loggerNameFromText *** id)
                                            [ ("iohk.startup", Debug)
                                            , ("iohk.background.process", Error)
                                            , ("iohk.testing.uncritical", Warning)
                                            ]
        , cgMapSubtrace       = HM.fromList $ map (loggerNameFromText *** id)
                                            [ ("iohk.benchmarking",
                                                    ObservableTraceSelf [GhcRtsStats, MonotonicClock])
                                            , ("iohk.deadend", NoTrace)
                                            ]
        , cgMapSubtraceCache  = HM.fromList $ map (loggerNameFromText *** id)
                                            [ ("iohk.benchmarking",
                                                    ObservableTraceSelf [GhcRtsStats, MonotonicClock])
                                            , ("iohk.deadend", NoTrace)
                                            ]
        , cgOptions           = HM.fromList
            [ ("mapSubtrace",
                Object $
                HM.fromList [("iohk.benchmarking",
                              Object (HM.fromList [("subtrace",String "ObservableTraceSelf")
                                                  ,("contents",Array $ V.fromList
                                                        [String "GhcRtsStats"
                                                        ,String "MonotonicClock"])]))
                            ,("iohk.deadend",
                              Object (HM.fromList [("subtrace",String "NoTrace")]))])
            , ("mapMonitors", Object $
                              HM.fromList [("chain.creation.block",Object (HM.fromList
                                            [("monitor",String "((time > (23 s)) Or (time < (17 s)))")
                                            ,("actions",Array $ V.fromList
                                                [ String "CreateMessage Warning \"chain.creation\""
                                                , String "AlterSeverity \"chain.creation\" Debug"
                                                ])]))
                                          ,("#aggregation.critproc.observable",Object (HM.fromList
                                            [("monitor",String "(mean >= (42))")
                                            ,("actions",Array $ V.fromList
                                                [ String "CreateMessage Warning \"the observable has been too long too high!\""
                                                , String "SetGlobalMinimalSeverity Info"
                                                ])]))])
            , ("mapSeverity", Object $ 
                              HM.fromList [("iohk.startup",String "Debug")
                                          ,("iohk.background.process",String "Error")
                                          ,("iohk.testing.uncritical",String "Warning")])
            , ("mapAggregatedkinds", Object $
                                     HM.fromList [("iohk.interesting.value",
                                                        String "EwmaAK {alpha = 0.75}")
                                                 ,("iohk.background.process",
                                                        String "StatsAK")])
            , ("cfokey", Object $ HM.fromList [("value",String "Release-1.0.0")])
            , ("mapScribes", Object $ HM.fromList
                                         [("iohk.interesting.value",
                                            Array $ V.fromList [String "StdoutSK::stdout"
                                                               ,String "FileSK::testlog"])
                                         ,("iohk.background.process",String "FileSK::testlog")])
            , ("mapBackends", Object $
                              HM.fromList [("iohk.user.defined",
                                                Array $ V.fromList [Object (HM.fromList [("kind", String "UserDefinedBK")
                                                                                        ,("name", String "MyBackend")])
                                                                    ,String "KatipBK"
                                                                   ])
                                          ,("iohk.interesting.value",
                                                Array $ V.fromList [String "EKGViewBK"
                                                                   ,String "AggregationBK"
                                                                   ])])
            ]
        , cgMapBackend        = HM.fromList $ map (loggerNameFromText *** id)
                                            [ ("iohk.user.defined"
                                              , [ UserDefinedBK "MyBackend"
                                                , KatipBK
                                                ]
                                              )
                                            , ("iohk.interesting.value"
                                              , [ EKGViewBK
                                                , AggregationBK
                                                ]
                                              )
                                            ]
        , cgMapBackendCache   = HM.fromList $ map (loggerNameFromText *** id)
                                            [ ("iohk.user.defined"
                                              , [ UserDefinedBK "MyBackend"
                                                , KatipBK
                                                ]
                                              )
                                            , ("iohk.interesting.value"
                                              , [ EKGViewBK
                                                , AggregationBK
                                                ]
                                              )
                                            ]
        , cgDefBackendKs      = [ KatipBK ]
        , cgSetupBackends     = [ AggregationBK
                                , EKGViewBK
                                , KatipBK
                                ]
        , cgMapScribe         = HM.fromList $ map (loggerNameFromText *** id)
                                            [ ("iohk.interesting.value",
                                                    ["StdoutSK::stdout","FileSK::testlog"])
                                            , ("iohk.background.process", ["FileSK::testlog"])
                                            ]
        , cgMapScribeCache    = HM.fromList $ map (loggerNameFromText *** id)
                                            [ ("iohk.interesting.value",
                                                    ["StdoutSK::stdout","FileSK::testlog"])
                                            , ("iohk.background.process", ["FileSK::testlog"])
                                            ]
        , cgDefScribes        = ["StdoutSK::stdout"]
        , cgSetupScribes      = [ ScribeDefinition
                                    { scKind     = FileSK
                                    , scFormat   = ScText
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
                                    , scFormat = ScText
                                    , scName = "stdout"
                                    , scPrivacy = ScPublic
                                    , scRotation = Nothing
                                    }
                                ]
        , cgMapAggregatedKind = HM.fromList $ map (loggerNameFromText *** id)
                                            [ ("iohk.interesting.value", EwmaAK {alpha = 0.75} )
                                            , ("iohk.background.process", StatsAK)
                                            ]
        , cgDefAggregatedKind = StatsAK
        , cgMonitors          = HM.fromList $ map (loggerNameFromText *** id)
                                            [ ( "chain.creation.block"
                                              , ( Nothing
                                                , (OR (Compare "time" (GT, (OpMeasurable (Agg.Seconds 23)))) (Compare "time" (LT, (OpMeasurable (Agg.Seconds 17)))))
                                                , [ CreateMessage Warning "chain.creation"
                                                  , AlterSeverity (loggerNameFromText "chain.creation") Debug
                                                  ]
                                                )
                                              )
                                            , ( "#aggregation.critproc.observable"
                                              , ( Nothing
                                                , Compare "mean" (GE, (OpMeasurable (Agg.PureI 42)))
                                                , [ CreateMessage Warning "the observable has been too long too high!"
                                                  , SetGlobalMinimalSeverity Info
                                                  ]
                                                )
                                              )
                                            ]
        , cgPortEKG           = 12789
        , cgPortGraylog       = 12788
        , cgBindAddrPrometheus = Nothing
        , cgPortGUI           = 0
        , cgForwardTo         = Just (RemotePipe "to")
        , cgAcceptAt          = Just [RemoteAddrNamed "a" (RemotePipe "at")]
        }

unitConfigurationExport :: Assertion
unitConfigurationExport = do
    cfg  <- setup "test/config.yaml"

    cfg' <- withSystemTempFile "config.yaml-1213" $ \file0 _ -> do
                let file = file0 <> "-copy"
                exportConfiguration cfg file
                setup file

    cfgInternal  <- readMVar $ getCG cfg
    cfgInternal' <- readMVar $ getCG cfg'

    cfgInternal' @?= cfgInternal

unitConfigurationExportStdout :: Assertion
unitConfigurationExportStdout = do
    cfg <- defaultConfigStdout

    cfg' <- withSystemTempFile "config.yaml-1213" $ \file0 _ -> do
                let file = file0 <> "-copy"
                exportConfiguration cfg file
                setup file

    cfgInternal  <- readMVar $ getCG cfg
    cfgInternal' <- readMVar $ getCG cfg'

    cgMinSeverity        cfgInternal' @?= cgMinSeverity        cfgInternal
    cgDefRotation        cfgInternal' @?= cgDefRotation        cfgInternal
    cgMapSeverity        cfgInternal' @?= cgMapSeverity        cfgInternal
    cgMapSubtrace        cfgInternal' @?= cgMapSubtrace        cfgInternal
    cgMapSubtraceCache   cfgInternal' @?= cgMapSubtraceCache   cfgInternal
    cgOptions            cfgInternal' @?= cgOptions            cfgInternal
    cgMapBackend         cfgInternal' @?= cgMapBackend         cfgInternal
    cgMapBackendCache    cfgInternal' @?= cgMapBackendCache    cfgInternal
    cgDefBackendKs       cfgInternal' @?= cgDefBackendKs       cfgInternal
    cgSetupBackends      cfgInternal' @?= cgSetupBackends      cfgInternal
    cgMapScribe          cfgInternal' @?= cgMapScribe          cfgInternal
    cgMapScribeCache     cfgInternal' @?= cgMapScribeCache     cfgInternal
    cgDefScribes         cfgInternal' @?= cgDefScribes         cfgInternal
    cgSetupScribes       cfgInternal' @?= cgSetupScribes       cfgInternal
    cgMapAggregatedKind  cfgInternal' @?= cgMapAggregatedKind  cfgInternal
    cgDefAggregatedKind  cfgInternal' @?= cgDefAggregatedKind  cfgInternal
    cgMonitors           cfgInternal' @?= cgMonitors           cfgInternal
    cgPortEKG            cfgInternal' @?= cgPortEKG            cfgInternal
    cgPortGraylog        cfgInternal' @?= cgPortGraylog        cfgInternal
    cgBindAddrPrometheus cfgInternal' @?= cgBindAddrPrometheus cfgInternal
    cgPortGUI            cfgInternal' @?= cgPortGUI            cfgInternal
    cfgInternal' @?= cfgInternal

\end{code}

Test caching and inheritance of Scribes.
\begin{code}
unitConfigurationCheckScribeCache :: Assertion
unitConfigurationCheckScribeCache = do
    configuration <- empty

    let defScribes = ["FileSK::node.log"]
    setDefaultScribes configuration defScribes

    let scribes12 = ["StdoutSK::stdout", "FileSK::out.txt"]

    setScribes configuration (loggerNameFromText "name1.name2") $ Just scribes12

    scribes1234 <- getScribes configuration (loggerNameFromText "name1.name2.name3.name4")
    scribes1    <- getScribes configuration (loggerNameFromText "name1")

    scribes1234cached <- getCachedScribes configuration (loggerNameFromText "name1.name2.name3.name4")
    scribesXcached    <- getCachedScribes configuration (loggerNameFromText "nameX")

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
    defAggregatedKind <- getAggregatedKind configuration
      (loggerNameFromText "non-existent.loggername")

    setAggregatedKind configuration (loggerNameFromText "name1") $ Just StatsAK
    name1AggregatedKind <- getAggregatedKind configuration
      (loggerNameFromText "name1")

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
