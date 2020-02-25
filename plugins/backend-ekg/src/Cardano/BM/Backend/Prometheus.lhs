
\subsection{Cardano.BM.Backend.Prometheus}
\label{module:Cardano.BM.Backend.Prometheus}

%if style == newcode
\begin{code}

module Cardano.BM.Backend.Prometheus
    ( spawnPrometheus
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Monad.IO.Class (MonadIO (..))
import qualified Data.HashMap.Strict as HM
import           Data.ByteString.Builder
import           Data.ByteString.Char8 (ByteString)
import           Data.Text (Text, replace)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Text.Read (double)
import           Snap.Core (Snap, route, writeLBS)
import           Snap.Http.Server (Config, ConfigLog (..), defaultConfig, setAccessLog,
                     setBind, setErrorLog, setPort, simpleHttpServe)
import           System.Metrics (Value (..), sampleAll)
import qualified System.Remote.Monitoring as EKG

\end{code}
%endif

\subsubsection{Spawn Prometheus client from existing EKG server}
\label{code:spawnPrometheus}\index{spawnPrometheus}
\begin{code}

spawnPrometheus :: EKG.Server -> ByteString -> Int -> IO (Async.Async ())
spawnPrometheus ekg host port = Async.async $
    simpleHttpServe config site
  where
    config :: Config Snap a
    config = setPort port . setBind host . setAccessLog lg . setErrorLog lg $ defaultConfig
    lg = ConfigNoLog
    site :: Snap ()
    site = route [ ("/metrics/", webhandler ekg) ]
    webhandler :: EKG.Server -> Snap ()
    webhandler srv = do
        samples <- liftIO $ sampleAll $ EKG.serverMetricStore srv
        writeLBS . toLazyByteString . renderSamples $ HM.toList samples
        pure ()
    renderSamples :: [(Text, Value)] -> Builder
    renderSamples [] = mempty
    renderSamples samples = mconcat
        [ case sv of
            Counter c -> renderNamedValue sk (int64Dec c)
            Gauge g -> renderNamedValue sk (int64Dec g)
            Label l -> if isFloat l
                         then renderNamedValue sk (byteString $ encodeUtf8 l)
                         else mempty
            _ -> mempty
        | (sk,sv) <- samples ]
    renderNamedValue :: Text -> Builder -> Builder
    renderNamedValue nm bld =
        (byteString $ prepareName nm)
        <> charUtf8 ' '
        <> bld
        <> charUtf8 '\n'
    prepareName nm = encodeUtf8 $ replace " " "_" $ replace "-" "_" $ replace "." "_" nm
    isFloat v = case double v of
        Right (_n, "") -> True  -- only floating point number parsed, no leftover
        _ -> False

\end{code}
