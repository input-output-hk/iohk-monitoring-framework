\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main )
  where

import           Control.Concurrent (threadDelay)

import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Configuration.Static (defaultConfigStdout)
import           Cardano.BM.Setup (setupTrace)
import           Cardano.BM.Trace (Trace, appendName, logDebug, logError,
                     logInfo, logNotice, logWarning)


main :: IO ()
main = do
    c <- defaultConfigStdout
    CM.setScribes c "simple.json" (Just ["StdoutSK::json"])
    tr :: Trace IO String <- setupTrace (Right c) "simple"
    trText <- appendName "text" tr
    trJson <- appendName "json" tr

    logDebug   trText "this is a debug message"
    logDebug   trJson "this is a debug message"
    logInfo    trText "this is an information."
    logInfo    trJson "this is an information."
    logNotice  trText "this is a notice!"
    logNotice  trJson "this is a notice!"
    logWarning trText "this is a warning!"
    logWarning trJson "this is a warning!"
    logError   trText "this is an error!"
    logError   trJson "this is an error!"

    threadDelay 80000

    return ()

\end{code}