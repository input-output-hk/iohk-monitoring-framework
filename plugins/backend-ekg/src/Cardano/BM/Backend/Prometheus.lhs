
\subsection{Cardano.BM.Backend.Prometheus}
\label{module:Cardano.BM.Backend.Prometheus}

%if style == newcode
\begin{code}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}

module Cardano.BM.Backend.Prometheus
    ( spawnPrometheus
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Monad.IO.Class (MonadIO (..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as A
import           Data.Aeson ((.=))
import           Data.ByteString.Builder
import           Data.ByteString.Char8 (ByteString)
import           Data.List (find, partition)
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Text.Read (double)
import           GHC.Generics
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

data MetricsGroup = MetricsGroup
    { namespace :: !Text
    , metrics   :: ![Metric]
    } deriving (Generic, A.ToJSON)

data Metric
    = NoMetric
    | Metric
        { mName  :: !Text
        , mType  :: !Text
        , mValue :: !Number
        }

instance A.ToJSON Metric where
    toJSON NoMetric = A.Null
    toJSON (Metric n t v) = A.object ["name" .= n, "type" .= t, "value" .= v]

data Number
    = NumberInt Integer
    | NumberReal Double

instance A.ToJSON Number where
    toJSON (NumberInt i)  = A.Number $ fromInteger i
    toJSON (NumberReal r) = A.Number $ fromRational (toRational r)

spawnPrometheus :: EKG.Server -> ByteString -> Int -> Maybe Text -> IO (Async.Async ())
spawnPrometheus ekg host port prometheusOutput = Async.async $
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
        let output = case prometheusOutput of
                         Nothing     -> renderSimpleOutput samples
                         Just "json" -> renderJSONOutput samples
                         Just _      -> renderSimpleOutput samples
        writeLBS output
        pure ()

    -- Simple output: key value.

    renderSimpleOutput = toLazyByteString . renderSamples . HM.toList

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
    prepareName nm = encodeUtf8 $ T.filter (flip elem (['a'..'z']++['A'..'Z']++['_'])) $ T.replace " " "_" $ T.replace "-" "_" $ T.replace "." "_" nm
    isFloat v = case double v of
        Right (_n, "") -> True  -- only floating point number parsed, no leftover
        _ -> False

    -- JSON output

    renderJSONOutput samples =
        let rtsNamespace = "rts.gc"
            (rtsSamples, otherSamples) = partition (\(sk, _) -> rtsNamespace `T.isPrefixOf` sk) $ HM.toList samples
            rtsMetrics = extractRtsGcMetrics rtsNamespace rtsSamples
            otherMetrics = extractOtherMetrics otherSamples
        in A.encode [rtsMetrics, otherMetrics]

    -- rts.gc metrics are always here because they are predefined in ekg-core,
    -- so we can group them.
    extractRtsGcMetrics :: Text -> [(Text, Value)] -> MetricsGroup
    extractRtsGcMetrics ns samples = MetricsGroup
        { namespace = ns
        , metrics =
            [ case sv of
                Counter c -> intMetric sk c
                Gauge g   -> intMetric sk g
                _         -> NoMetric -- rts.gc can contain Counter or Gauge only.
            | (sk, sv) <- samples
            ]
        }
      where
        intMetric sk v =
            Metric { mName  = maybe "" id $ T.stripPrefix (ns <> ".") sk
                   , mType  = "int" -- All values are Int64.
                   , mValue = NumberInt (fromIntegral v)
                   }

    -- We cannot make any assumptions about the format of 'sk' in other samples,
    -- so group other samples into 'common' group.
    extractOtherMetrics :: [(Text, Value)] -> MetricsGroup
    extractOtherMetrics samples = MetricsGroup
        { namespace = "common"
        , metrics =
            [ case sv of
                Counter c -> mkMetric sk $ NumberInt (fromIntegral c)
                Gauge g   -> mkMetric sk $ NumberInt (fromIntegral g)
                Label l   -> case double l of
                                 Left _       -> NoMetric
                                 Right (r, _) -> mkMetric sk $ NumberReal r
                _         -> NoMetric
            | (sk, sv) <- samples
            ]
        }
      where
        mkMetric sk number =
            let (withoutType, typeSuffix) = stripTypeSuffix sk number
            in Metric { mName = withoutType, mType = typeSuffix, mValue = number }
        stripTypeSuffix sk number =
            let types = ["us", "ns", "s", "B", "int", "real"]
                parts = T.splitOn "." sk
                typeSuffix = if not . null $ parts then last parts else ""
            in if typeSuffix `elem` types
                   then (fromJust $ T.stripSuffix ("." <> typeSuffix) sk, typeSuffix)
                   else case number of
                            NumberInt _  -> (sk, "int")
                            NumberReal _ -> (sk, "real")
\end{code}
