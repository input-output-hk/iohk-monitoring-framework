
\subsection{Cardano.BM.Backend.Prometheus}
\label{module:Cardano.BM.Backend.Prometheus}



%if style == newcode
\begin{code}

module Cardano.BM.Backend.Prometheus
    ( spawnPrometheus
    , passToPrometheus
    ) where

import qualified Control.Concurrent.Async as Async
import           Network.Wai.Handler.Warp (Port)
import           System.Metrics.Prometheus.Http.Scrape (serveHttpTextMetrics)
import           System.Metrics.Prometheus.Registry (sample)
import           System.Metrics.Prometheus.RegistryT (execRegistryT)
import qualified System.Remote.Monitoring as EKG
import           System.Remote.Monitoring.Prometheus (registerEKGStore, AdapterOptions (..))

\end{code}
%endif

\subsubsection{Spawn Prometheus client from existing EKG server}\label{code:Monitor}\index{Monitor}
\begin{code}

spawnPrometheus :: EKG.Server -> Port -> IO (Async.Async ())
spawnPrometheus s p = Async.async $ passToPrometheus s p

passToPrometheus :: EKG.Server -> Port -> IO ()
passToPrometheus server port = do
    let store = EKG.serverMetricStore server
    let reg = execRegistryT $ registerEKGStore store $ AdapterOptions mempty Nothing 1

    serveHttpTextMetrics port ["metrics"] (reg >>= sample) -- http://localhost:{port}/metrics server

\end{code}
