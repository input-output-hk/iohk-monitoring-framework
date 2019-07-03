\begin{code}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

#if defined(linux_HOST_OS)
#define LINUX
#endif

module Main
  ( main )
  where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.MVar (MVar, newMVar, modifyMVar_, withMVar)
import           Data.Aeson (FromJSON)

import           Cardano.BM.Backend.Switchboard (addExternalBackend)
import           Cardano.BM.Data.Backend
import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Configuration.Static (defaultConfigStdout)
#ifdef LINUX
import           Cardano.BM.Data.Output (ScribeDefinition (..),
                     ScribePrivacy (..), ScribeKind (..), ScribeFormat (..))
#endif
import           Cardano.BM.Setup (setupTrace_)
import           Cardano.BM.Trace (Trace, appendName, logDebug, logError,
                     logInfo, logNotice, logWarning)

\end{code}

\subsubsection{a simple backend}
\begin{code}
type MyBackendMVar a = MVar (MyBackendInternal a)
newtype MyBackend a = MyBackend { myBE :: MyBackendMVar a }
data MyBackendInternal a = MyBackendInternal {
                            counter :: Int
                         }

instance (FromJSON a) => IsBackend MyBackend a where
    typeof _ = UserDefinedBK "MyBackend"
    realize _ = MyBackend <$> newMVar (MyBackendInternal 0)
    unrealize be = putStrLn $ "unrealize " <> show (typeof be)

instance IsEffectuator MyBackend a where
    effectuate be _item = do
        modifyMVar_ (myBE be) $ \mybe ->
            return $ mybe { counter = counter mybe + 1}

    handleOverflow _ = putStrLn "Error: MyBackend's queue full!"

\end{code}

\subsubsection{Entry procedure}
\begin{code}
main :: IO ()
main = do
    c <- defaultConfigStdout
    CM.setDefaultBackends c [KatipBK, UserDefinedBK "MyBackend"]
#ifdef LINUX
    CM.setSetupBackends c [KatipBK, GraylogBK]
    CM.setDefaultBackends c [KatipBK, GraylogBK, UserDefinedBK "MyBackend"]
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
    (tr :: Trace IO String, sb) <- setupTrace_ c "simple"
    be :: MyBackend String <- realize c
    let mybe = MkBackend { bEffectuate = effectuate be, bUnrealize = unrealize be }
    addExternalBackend sb mybe "MyBackend"
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

    withMVar (myBE be) $ \backend ->
        putStrLn $ "read in total " ++ (show $ counter backend) ++ " messages."

    return ()

\end{code}
