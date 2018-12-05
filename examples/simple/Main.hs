{-# LANGUAGE OverloadedStrings   #-}

module Main
  ( main )
  where

import           Control.Concurrent (threadDelay)

import           Cardano.BM.Configuration.Static
import           Cardano.BM.Setup
import           Cardano.BM.Trace


main :: IO ()
main = do
    c <- defaultConfigStdout
    tr <- setupTrace (Right c) "simple"

    logDebug   tr "this is a debug message"
    logInfo    tr "this is an information."
    logNotice  tr "this is a notice!"
    logWarning tr "this is a warning!"
    logError   tr "this is an error!"

    threadDelay (fromIntegral 80000)

    return ()