\subsubsection{Module header and import directives}
\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main )
  where

import qualified Control.Concurrent.Async as Async
import           Control.Monad (forM_)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)

import           Criterion (Benchmark, bench, nfIO)
import           Criterion.Main (defaultMain)

import           Cardano.BM.Backend.Switchboard
import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.Aggregated (Measurable (..))
import           Cardano.BM.Data.BackendKind
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.MonitoringEval
import           Cardano.BM.Data.Severity
import           Cardano.BM.Setup
import           Cardano.BM.Trace

\end{code}

\subsubsection{Define configuration}
\begin{code}
prepare_configuration :: IO CM.Configuration
prepare_configuration = do
    c <- CM.empty
    CM.setMinSeverity c Warning
    CM.setSetupBackends c [ MonitoringBK ]
    CM.setDefaultBackends c [ MonitoringBK ]

    CM.setMonitors c $ HM.fromList
        [ ( "performance.monitoring"
          , ( Nothing
            , Compare "monitMe" (GE, (OpMeasurable 42))
            , [SetGlobalMinimalSeverity Debug]
            )
          )
        ]
    CM.setBackends c "performance.monitoring" (Just [MonitoringBK])
    return c

\end{code}

\subsubsection{Thread that outputs a value to monitoring |Trace|}
\begin{code}
monitoringThr :: Trace IO Text -> Int -> IO (Async.Async ())
monitoringThr trace objNumber = do
  let trace' = appendName "monitoring" trace
  obj <- (,) <$> (mkLOMeta Warning Public) <*> pure (LogValue "monitMe" (PureD 123.45))
  proc <- Async.async (loop trace' obj)
  return proc
  where
    loop tr lo = do
      forM_ [1 .. objNumber] $ \_ -> traceNamedObject tr lo
       -- terminate Switchboard
      killPill <- (,) <$> (mkLOMeta Warning Public) <*> pure KillPill
      traceNamedObject tr killPill
\end{code}

\subsubsection{Main entry point}
\begin{code}
main :: IO ()
main = defaultMain
    [ benchMain 1000
    , benchMain 10000
    , benchMain 100000
    , benchMain 1000000
    ]

benchMain :: Int -> Benchmark
benchMain objNumber = bench (show objNumber ++ " objects") $ nfIO $ do
    c <- prepare_configuration
    (tr :: Trace IO Text, sb) <- setupTrace_ c "performance"
    procMonitoring <- monitoringThr tr objNumber
    _ <- Async.wait procMonitoring
    _ <- waitForTermination sb
    return ()

\end{code}
