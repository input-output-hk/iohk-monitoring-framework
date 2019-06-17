\begin{code}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if defined(linux_HOST_OS)
#define LINUX
#endif

module Main
  ( main )
  where

import           Control.Concurrent (threadDelay)

import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Configuration.Static (defaultConfigStdout)
#ifdef LINUX
import           Cardano.BM.Data.BackendKind
import           Cardano.BM.Data.Output (ScribeDefinition (..),
                     ScribePrivacy (..), ScribeKind (..), ScribeFormat (..))
#endif
import           Cardano.BM.Setup (setupTrace)
import           Cardano.BM.Trace (Trace, appendName, logDebug, logError,
                     logInfo, logNotice, logWarning)


main :: IO ()
main = do
    c <- defaultConfigStdout
#ifdef LINUX
    CM.setSetupBackends c [KatipBK, GraylogBK]
    CM.setDefaultBackends c [KatipBK, GraylogBK]
    CM.setGraylogPort c 3456
    CM.setSetupScribes c [ ScribeDefinition {
                              scName = "text"
                            , scFormat = ScText
                            , scKind = StdoutSK
                            , scPrivacy = ScPublic
                            , scRotation = Nothing
                            }
                         ,  ScribeDefinition {
                              scName = "json"
                            , scFormat = ScJson
                            , scKind = StdoutSK
                            , scPrivacy = ScPublic
                            , scRotation = Nothing
                            }
                         ,  ScribeDefinition {
                              scName = "systemd"
                            , scFormat = ScText
                            , scKind = JournalSK
                            , scPrivacy = ScPublic
                            , scRotation = Nothing
                            }
                         ]
    CM.setScribes c "simple.systemd" (Just ["JournalSK::systemd"])
#endif
    CM.setScribes c "simple.json" (Just ["StdoutSK::json"])
    tr :: Trace IO String <- setupTrace (Right c) "simple"
    trText <- appendName "text" tr
    trJson <- appendName "json" tr
#ifdef LINUX
    trSystemd <- appendName "systemd" tr
#endif

    logDebug   trText    "this is a debug message\nwith a second line"
    logDebug   trJson    "this is a debug message\nwith a second line"
    logInfo    trText    "this is an information."
    logInfo    trJson    "this is an information."
    logNotice  trText    "this is a notice!"
    logNotice  trJson    "this is a notice!"
    logWarning trText    "this is a warning!"
    logWarning trJson    "this is a warning!"
    logError   trText    "this is an error!"
    logError   trJson    "this is an error!"
#ifdef LINUX
    logError   trSystemd "this is an error!"
#endif

    threadDelay 80000

    return ()

\end{code}
