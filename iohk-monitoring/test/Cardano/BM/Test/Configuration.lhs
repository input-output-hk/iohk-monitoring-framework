
\subsection{Testing configuration}
\label{code:Cardano.BM.Test.Configuration}

%if style == newcode
\begin{code}

module Cardano.BM.Test.Configuration (
    tests
  ) where

import           Prelude hiding (Ordering (..))

import           Control.Concurrent.MVar (readMVar)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import           Data.Yaml
import           System.IO.Temp (withSystemTempFile)

import           Cardano.BM.Data.Configuration
import           Cardano.BM.Configuration.Model (Configuration (..),
                     ConfigurationInternal (..), getScribes, getCachedScribes,
                     getDefaultBackends, getAggregatedKind, getGUIport,
                     getEKGBindAddr, empty, setDefaultScribes, setScribes, setup,
                     setDefaultAggregatedKind, setAggregatedKind, setGUIport,
                     setEKGBindAddr, exportConfiguration)
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
import qualified Data.Aeson.KeyMap as KeyMap

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
-- These tests make invalid assumptions about ordering. Disable them for now.
--        testCase "static representation" unitConfigurationStaticRepresentation
--      , testCase "parsed representation" unitConfigurationParsedRepresentation
       testCase "parsed configuration" unitConfigurationParsed
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

\begin{lstlisting}
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
                [ ScribeDefinition { scName     = "stdout"
                                   , scKind     = StdoutSK
                                   , scFormat   = ScText
                                   , scPrivacy  = ScPublic
                                   , scMinSev   = minBound
                                   , scMaxSev   = maxBound
                                   , scRotation = Nothing }
                ]
            , defaultScribes = [(StdoutSK, "stdout")]
            , setupBackends = [ EKGViewBK, KatipBK ]
            , defaultBackends = [ KatipBK ]
            , hasGUI = Just 12789
            , hasGraylog = Just 12788
            , hasEKG = Just $ Endpoint ("localhost", 18321)
            , hasPrometheus = Just ("localhost", 12799)
            , traceForwardTo = Just (RemotePipe "to")
            , forwardDelay = Just 1000
            , traceAcceptAt = Just [RemoteAddrNamed "a" (RemotePipe "at")]
            , options =
                HM.fromList [ ("test1", Object (KeyMap.singleton "value" "object1"))
                            , ("test2", Object (KeyMap.singleton "value" "object2")) ]
            }
    in
    encode r @?=
        (intercalate "\n"
            [ "rotation:"
            , "  rpKeepFilesNum: 10"
            , "  rpMaxAgeHours: 24"
            , "  rpLogLimitBytes: 5000000"
            , "traceAcceptAt:"
            , "- remoteAddr:"
            , "    contents: at"
            , "    tag: RemotePipe"
            , "  nodeName: a"
            , "minSeverity: Info"
            , "traceForwardTo:"
            , "  contents: to"
            , "  tag: RemotePipe"
            , "hasGUI: 12789"
            , "forwardDelay: 1000"
            , "hasGraylog: 12788"
            , "options:"
            , "  test1:"
            , "    value: object1"
            , "  test2:"
            , "    value: object2"
            , "defaultBackends:"
            , "- KatipBK"
            , "defaultScribes:"
            , "- - StdoutSK"
            , "  - stdout"
            , "hasEKG:"
            , "- localhost"
            , "- 18321"
            , "hasPrometheus:"
            , "- localhost"
            , "- 12799"
            , "setupScribes:"
            , "- scName: stdout"
            , "  scMaxSev: Emergency"
            , "  scRotation: null"
            , "  scPrivacy: ScPublic"
            , "  scFormat: ScText"
            , "  scKind: StdoutSK"
            , "  scMinSev: Debug"
            , "setupBackends:"
            , "- EKGViewBK"
            , "- KatipBK"
            , ""
            ]
        )

unitConfigurationParsedRepresentation :: Assertion
unitConfigurationParsedRepresentation = do
    repr <- readRepresentation "test/config.yaml"
    encode repr @?=
        (intercalate "\n"
            [ "rotation:"
            , "  rpKeepFilesNum: 10"
            , "  rpMaxAgeHours: 24"
            , "  rpLogLimitBytes: 5000000"
            , "traceAcceptAt:"
            , "- remoteAddr:"
            , "    contents: at"
            , "    tag: RemotePipe"
            , "  nodeName: a"
            , "minSeverity: Info"
            , "traceForwardTo:"
            , "  contents: to"
            , "  tag: RemotePipe"
            , "hasGUI: null"
            , "forwardDelay: 1000"
            , "hasGraylog: 12788"
            , "options:"
            , "  mapSeverity:"
            , "    iohk.testing.uncritical: Warning"
            , "    iohk.background.process: Error"
            , "    iohk.startup: Debug"
            , "  mapMonitors:"
            , "    '#aggregation.critproc.observable':"
            , "      monitor: (mean >= (42))"
            , "      actions:"
            , "      - CreateMessage Warning \"the observable has been too long too high!\""
            , "      - SetGlobalMinimalSeverity Info"
            , "    chain.creation.block:"
            , "      monitor: ((time > (23 s)) Or (time < (17 s)))"
            , "      actions:"
            , "      - CreateMessage Warning \"chain.creation\""
            , "      - AlterSeverity \"chain.creation\" Debug"
            , "  mapBackends:"
            , "    iohk.interesting.value:"
            , "    - EKGViewBK"
            , "    - AggregationBK"
            , "    iohk.user.defined:"
            , "    - kind: UserDefinedBK"
            , "      name: MyBackend"
            , "    - KatipBK"
            , "  cfokey:"
            , "    value: Release-1.0.0"
            , "  mapSubtrace:"
            , "    iohk.deadend:"
            , "      subtrace: NoTrace"
            , "    iohk.benchmarking:"
            , "      subtrace: ObservableTraceSelf"
            , "      contents:"
            , "      - GhcRtsStats"
            , "      - MonotonicClock"
            , "  mapScribes:"
            , "    iohk.background.process: FileSK::testlog"
            , "    iohk.interesting.value:"
            , "    - StdoutSK::stdout"
            , "    - FileSK::testlog"
            , "  mapAggregatedkinds:"
            , "    iohk.background.process: StatsAK"
            , "    iohk.interesting.value: EwmaAK {alpha = 0.75}"
            , "defaultBackends:"
            , "- KatipBK"
            , "defaultScribes:"
            , "- - StdoutSK"
            , "  - stdout"
            , "hasEKG:"
            , "- 127.0.0.1"
            , "- 12789"
            , "hasPrometheus: null"
            , "setupScribes:"
            , "- scName: testlog"
            , "  scMaxSev: Emergency"
            , "  scRotation:"
            , "    rpKeepFilesNum: 3"
            , "    rpMaxAgeHours: 24"
            , "    rpLogLimitBytes: 25000000"
            , "  scPrivacy: ScPrivate"
            , "  scFormat: ScText"
            , "  scKind: FileSK"
            , "  scMinSev: Debug"
            , "- scName: stdout"
            , "  scMaxSev: Emergency"
            , "  scRotation: null"
            , "  scPrivacy: ScPublic"
            , "  scFormat: ScText"
            , "  scKind: StdoutSK"
            , "  scMinSev: Debug"
            , "setupBackends:"
            , "- AggregationBK"
            , "- EKGViewBK"
            , "- KatipBK"
            , ""
            ]
        )
\end{lstlisting}

\begin{code}

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
        , cgMapSeverity       = HM.fromList [ ("iohk.startup", Debug)
                                            , ("iohk.background.process", Error)
                                            , ("iohk.testing.uncritical", Warning)
                                            ]
        , cgMapSubtrace       = HM.fromList [ ("iohk.benchmarking",
                                                    ObservableTraceSelf [GhcRtsStats, MonotonicClock])
                                            , ("iohk.deadend", NoTrace)
                                            ]
        , cgOptions           = HM.fromList
            [ ("mapSubtrace",
                Object $
                KeyMap.fromList [("iohk.benchmarking",
                              Object (KeyMap.fromList [("subtrace",String "ObservableTraceSelf")
                                                  ,("contents",Array $ V.fromList
                                                        [String "GhcRtsStats"
                                                        ,String "MonotonicClock"])]))
                            ,("iohk.deadend",
                              Object (KeyMap.fromList [("subtrace",String "NoTrace")]))])
            , ("mapMonitors", Object $
                              KeyMap.fromList [("chain.creation.block",Object (KeyMap.fromList
                                            [("monitor",String "((time > (23 s)) Or (time < (17 s)))")
                                            ,("actions",Array $ V.fromList
                                                [ String "CreateMessage Warning \"chain.creation\""
                                                , String "AlterSeverity \"chain.creation\" Debug"
                                                ])]))
                                          ,("#aggregation.critproc.observable",Object (KeyMap.fromList
                                            [("monitor",String "(mean >= (42))")
                                            ,("actions",Array $ V.fromList
                                                [ String "CreateMessage Warning \"the observable has been too long too high!\""
                                                , String "SetGlobalMinimalSeverity Info"
                                                ])]))])
            , ("mapSeverity", Object $
                              KeyMap.fromList [("iohk.startup",String "Debug")
                                          ,("iohk.background.process",String "Error")
                                          ,("iohk.testing.uncritical",String "Warning")])
            , ("mapAggregatedkinds", Object $
                                     KeyMap.fromList [("iohk.interesting.value",
                                                        String "EwmaAK {alpha = 0.75}")
                                                 ,("iohk.background.process",
                                                        String "StatsAK")])
            , ("cfokey", Object $ KeyMap.fromList [("value",String "Release-1.0.0")])
            , ("mapScribes", Object $ KeyMap.fromList [("iohk.interesting.value",
                                            Array $ V.fromList [String "StdoutSK::stdout"
                                                               ,String "FileSK::testlog"])
                                         ,("iohk.background.process",String "FileSK::testlog")])
            , ("mapBackends", Object $
                              KeyMap.fromList [("iohk.user.defined",
                                                Array $ V.fromList [Object (KeyMap.fromList [("kind", String "UserDefinedBK")
                                                                                        ,("name", String "MyBackend")])
                                                                    ,String "KatipBK"
                                                                   ])
                                          ,("iohk.interesting.value",
                                                Array $ V.fromList [String "EKGViewBK"
                                                                   ,String "AggregationBK"
                                                                   ])])
            ]
        , cgMapBackend        = HM.fromList [ ("iohk.user.defined"
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
        , cgMapScribe         = HM.fromList [ ("iohk.interesting.value",
                                                    ["StdoutSK::stdout","FileSK::testlog"])
                                            , ("iohk.background.process", ["FileSK::testlog"])
                                            ]
        , cgMapScribeCache    = HM.fromList [ ("iohk.interesting.value",
                                                    ["StdoutSK::stdout","FileSK::testlog"])
                                            , ("iohk.background.process", ["FileSK::testlog"])
                                            ]
        , cgDefScribes        = ["StdoutSK::stdout"]
        , cgSetupScribes      = [ ScribeDefinition
                                    { scKind     = FileSK
                                    , scFormat   = ScText
                                    , scName     = "testlog"
                                    , scPrivacy  = ScPrivate
                                    , scMinSev   = minBound
                                    , scMaxSev   = maxBound
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
                                    , scMinSev   = minBound
                                    , scMaxSev   = maxBound
                                    , scRotation = Nothing
                                    }
                                ]
        , cgMapAggregatedKind = HM.fromList [ ("iohk.interesting.value", EwmaAK {alpha = 0.75} )
                                            , ("iohk.background.process", StatsAK)
                                            ]
        , cgDefAggregatedKind = StatsAK
        , cgMonitors          = HM.fromList [ ( "chain.creation.block"
                                              , ( Nothing
                                                , (OR (Compare "time" (GT, (OpMeasurable (Agg.Seconds 23)))) (Compare "time" (LT, (OpMeasurable (Agg.Seconds 17)))))
                                                , [ CreateMessage Warning "chain.creation"
                                                  , AlterSeverity "chain.creation" Debug
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
        , cgBindAddrEKG       = Just $ Endpoint ("127.0.0.1", 12789)
        , cgPortGraylog       = 12788
        , cgBindAddrPrometheus = Nothing
        , cgPortGUI           = 0
        , cgForwardTo         = Just (RemotePipe "to")
        , cgForwardDelay      = Just 1000
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
    cgOptions            cfgInternal' @?= cgOptions            cfgInternal
    cgMapBackend         cfgInternal' @?= cgMapBackend         cfgInternal
    cgDefBackendKs       cfgInternal' @?= cgDefBackendKs       cfgInternal
    cgSetupBackends      cfgInternal' @?= cgSetupBackends      cfgInternal
    cgMapScribe          cfgInternal' @?= cgMapScribe          cfgInternal
    cgMapScribeCache     cfgInternal' @?= cgMapScribeCache     cfgInternal
    cgDefScribes         cfgInternal' @?= cgDefScribes         cfgInternal
    cgSetupScribes       cfgInternal' @?= cgSetupScribes       cfgInternal
    cgMapAggregatedKind  cfgInternal' @?= cgMapAggregatedKind  cfgInternal
    cgDefAggregatedKind  cfgInternal' @?= cgDefAggregatedKind  cfgInternal
    cgMonitors           cfgInternal' @?= cgMonitors           cfgInternal
    cgBindAddrEKG        cfgInternal' @?= cgBindAddrEKG        cfgInternal
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

    setEKGBindAddr configuration (Just $ Endpoint ("localhost", 11223))
    ekgBindAddr <- getEKGBindAddr configuration

    setGUIport configuration 1080
    guiPort <- getGUIport configuration

    assertBool "Default backends" $
        defBackends == [KatipBK]

    assertBool "Default aggregated kind" $
        defAggregatedKind == EwmaAK 0.01

    assertBool "Specific name aggregated kind" $
        name1AggregatedKind == StatsAK

    assertBool "Set EKG host/port" $
        ekgBindAddr == (Just $ Endpoint ("localhost", 11223))

    assertBool "Set GUI port" $
        guiPort == 1080

\end{code}
