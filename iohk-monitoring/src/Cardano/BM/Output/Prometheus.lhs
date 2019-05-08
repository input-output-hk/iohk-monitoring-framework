
\subsection{Cardano.BM.Output.Prometheus}
\label{module:Cardano.BM.Output.Prometheus}



%if style == newcode
\begin{code}

module Cardano.BM.Output.Prometheus
    ( spawnPrometheus
    , passToPrometheus
    ) where

import qualified Control.Concurrent.Async as Async
import           System.Metrics.Prometheus.Http.Scrape (serveHttpTextMetrics)
import           System.Metrics.Prometheus.Registry (sample)
import           System.Metrics.Prometheus.RegistryT (execRegistryT)
import qualified System.Remote.Monitoring as EKG
import           System.Remote.Monitoring.Prometheus (registerEKGStore, defaultOptions)

\end{code}
%endif

\subsubsection{Spawn Prometheus client from existing EKG server}\label{code:Monitor}\index{Monitor}
\begin{code}

spawnPrometheus :: EKG.Server -> IO (Async.Async ())
spawnPrometheus = Async.async . passToPrometheus

passToPrometheus :: EKG.Server -> IO ()
passToPrometheus server = do
    let store = EKG.serverMetricStore server
    reg <- execRegistryT $ registerEKGStore store $ defaultOptions mempty

    serveHttpTextMetrics 8080 ["metrics"] (sample reg) -- http://localhost:8080/metrics server

\end{code}
