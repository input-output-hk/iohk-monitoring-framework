
\subsection{Cardano.BM.Backend.Prometheus}
\label{module:Cardano.BM.Backend.Prometheus}

%if style == newcode
\begin{code}

module Cardano.BM.Backend.Prometheus
    ( spawnPrometheus
    , passToPrometheus
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Monad.IO.Class (MonadIO (..))
import           Network.Wai.Handler.Warp (Port, defaultSettings, runSettings,
                   setHost, setPort)
import           System.Metrics.Prometheus.Http.Scrape (prometheusApp)
import           System.Metrics.Prometheus.Registry (RegistrySample, sample)
import           System.Metrics.Prometheus.RegistryT (execRegistryT)
import qualified System.Remote.Monitoring as EKG
import           System.Remote.Monitoring.Prometheus (registerEKGStore,
                   AdapterOptions (..))

\end{code}
%endif

\subsubsection{Spawn Prometheus client from existing EKG server}
\label{code:spawnPrometheus}\index{spawnPrometheus}
\label{code:passToPrometheus}\index{passToPrometheus}
\begin{code}

spawnPrometheus :: EKG.Server -> Port -> IO (Async.Async ())
spawnPrometheus s p = Async.async $ passToPrometheus s p

passToPrometheus :: EKG.Server -> Port -> IO ()
passToPrometheus server port =
    let store = EKG.serverMetricStore server
        reg = execRegistryT $ registerEKGStore store $ AdapterOptions mempty Nothing 1
    in serveMetrics (reg >>= sample)
  where
    serveMetrics :: MonadIO m => IO RegistrySample -> m ()
    serveMetrics = liftIO . runSettings settings . prometheusApp ["metrics"]
    settings = setPort port . setHost "127.0.0.1" $ defaultSettings

\end{code}
