\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main )
  where

import           Control.Concurrent (threadDelay)

import           Cardano.BM.Configuration.Static (defaultConfigStdout)
import           Cardano.BM.Setup (setupTrace)
import           Cardano.BM.Trace (logDebug, logError, logInfo, logNotice
                     logWarning)


main :: IO ()
main = do
    c <- defaultConfigStdout
    tr <- setupTrace (Right c) "simple"

    logDebug   tr "this is a debug message"
    logInfo    tr "this is an information."
    logNotice  tr "this is a notice!"
    logWarning tr "this is a warning!"
    logError   tr "this is an error!"

    threadDelay 80000

    return ()

\end{code}