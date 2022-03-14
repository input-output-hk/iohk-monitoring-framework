\begin{code}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main
  ( main )
  where

import           Data.Aeson (ToJSON (..), (.=))
import           Data.Text (Text)

import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Monad (forever)

import           Cardano.BM.Data.Tracer (trStructured)
import           Cardano.BM.Setup (setupTrace_, shutdown)
import           Cardano.BM.Stats (readResourceStats)
import           Cardano.BM.Stats.Resources
import           Cardano.BM.Trace (logInfo)
import           Cardano.BM.Tracing hiding (setupTrace)

\end{code}

\begin{code}
instance HasPrivacyAnnotation ResourceStats where
    getPrivacyAnnotation _ = Public
instance HasSeverityAnnotation ResourceStats where
    getSeverityAnnotation _ = Notice

instance ToObject ResourceStats where
    toObject _ sts =
        "stats" .= toJSON sts

instance Transformable Text IO ResourceStats where
    trTransformer verb tr = trStructured verb tr

\end{code}

\subsubsection{Continuously output resource stats}
\begin{code}
main :: IO ()
main = do
    c <- defaultConfigStdout
    (tr :: Trace IO Text, sb) <- setupTrace_ c "stats"

    let trace = appendName "node-stats" tr
    thr <- Async.async $ forever $ do
        readResourceStats >>= traceStats trace
        logInfo trace "traced stats."
        threadDelay 5000000   -- 5 seconds

    Async.link thr
    threadDelay 30000000  -- 30 seconds
    Async.cancel thr
    shutdown sb
    return ()
  where
      traceStats :: Trace IO Text -> Maybe ResourceStats -> IO ()
      traceStats _ Nothing = pure ()
      traceStats tr (Just sts) =
        traceWith (toLogObject tr) sts

\end{code}
