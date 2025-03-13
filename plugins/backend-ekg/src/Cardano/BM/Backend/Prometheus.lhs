
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
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.List (find, partition)
import           Data.Maybe (fromJust)
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Text.Read (double)
import           GHC.Conc (labelThread, myThreadId)
import           GHC.Generics
import           Network.HTTP.Types (status200)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           System.Metrics (Value (..), sampleAll)
import qualified System.Remote.Monitoring.Wai as Wai

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
        , mLabel :: !(Maybe Text)
        , mNumber :: !Number
        }

instance A.ToJSON Metric where
    toJSON NoMetric = A.Null
    toJSON (Metric n t Nothing v) = A.object ["name" .= n, "type" .= t, "value" .= v]
    toJSON (Metric n t (Just l) v) = A.object ["name" .= n, "type" .= t, "label" .= l, "value" .= v]

data Number
    = NumberInt Integer
    | NumberReal Double

instance A.ToJSON Number where
    toJSON (NumberInt i)  = A.Number $ fromInteger i
    toJSON (NumberReal r) = A.Number $ fromRational (toRational r)

spawnPrometheus :: Wai.Server -> Warp.HostPreference -> Int -> Maybe Text -> IO (Async.Async ())
spawnPrometheus ekg host port prometheusOutput =
    Async.async $ do
      myThreadId >>= flip labelThread "Prometheus (lobemo-backend-ekg)"
      Warp.runSettings settings site

  where
    settings :: Warp.Settings
    settings = Warp.setPort port . Warp.setHost host $ Warp.defaultSettings

    site :: Wai.Application
    site _request respond = do
        -- We ignore the request and simple respond with the data.
        samples <- sampleAll $ Wai.serverMetricStore ekg
        let output = case prometheusOutput of
                         Just "json" -> renderJSONOutput samples
                         _other -> renderSimpleOutput samples
        respond $ Wai.responseLBS status200 [] output

-- Simple output: key value.
renderSimpleOutput :: HM.HashMap Text Value -> LBS.ByteString
renderSimpleOutput =
    toLazyByteString . renderSamples . HM.toList
  where
    renderSamples :: [(Text, Value)] -> Builder
    renderSamples [] = mempty
    renderSamples samples = mconcat
        [ case sv of
            Counter c -> renderNamedValue sk (int64Dec c)
            Gauge g -> renderNamedValue sk (int64Dec g)
            Label l -> if "{" `T.isPrefixOf` l
                            then renderLabel sk l
                            else if (isFloat l)
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

    renderLabel :: Text -> Text -> Builder
    renderLabel nm l =
        (byteString $ prepareName nm)
        <> charUtf8 ' '
        <> byteString (textToUtf8ByteString l)
        <> charUtf8 ' '
        <> charUtf8 '1'
        <> charUtf8 '\n'
    prepareName nm = encodeUtf8 $ T.filter (flip elem (['a'..'z']++['A'..'Z']++['_'])) $ T.replace " " "_" $ T.replace "-" "_" $ T.replace "." "_" nm
    isFloat v = case double v of
        Right (_n, "") -> True  -- only floating point number parsed, no leftover
        _ -> False

-- JSON output
renderJSONOutput :: HM.HashMap Text Value -> LBS.ByteString
renderJSONOutput samples =
    let rtsNamespace = "rts.gc"
        (rtsSamples, otherSamples) = partition (\(sk, _) -> rtsNamespace `T.isPrefixOf` sk) $ HM.toList samples
        rtsMetrics = extractRtsGcMetrics rtsNamespace rtsSamples
        otherMetrics = extractOtherMetrics otherSamples
    in A.encode [rtsMetrics, otherMetrics]

  where
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
                   , mLabel = Nothing
                   , mNumber = NumberInt (fromIntegral v)
                   }

    -- We cannot make any assumptions about the format of 'sk' in other samples,
    -- so group other samples into 'common' group.
    extractOtherMetrics :: [(Text, Value)] -> MetricsGroup
    extractOtherMetrics samples = MetricsGroup
        { namespace = "common"
        , metrics =
            [ case sv of
                Counter c -> mkMetric sk Nothing $ NumberInt (fromIntegral c)
                Gauge g   -> mkMetric sk Nothing $ NumberInt (fromIntegral g)
                Label l   -> case double l of
                                 Right (r, _) ->
                                    mkMetric sk Nothing $ NumberReal r
                                 Left _       ->
                                    case T.uncons l of
                                        Just ('{', _) -> mkMetric sk (Just l) (NumberInt 1)
                                        _ -> NoMetric
                _         -> NoMetric
            | (sk, sv) <- samples
            ]
        }
      where
        mkMetric sk condTxt number =
            let (withoutType, typeSuffix) = stripTypeSuffix sk number
            in Metric { mName = withoutType, mType = typeSuffix, mLabel = condTxt, mNumber = number }
        stripTypeSuffix sk number =
            let types = ["us", "ns", "s", "B", "int", "real"]
                parts = T.splitOn "." sk
                typeSuffix = if not . null $ parts then last parts else ""
            in if typeSuffix `elem` types
                   then (fromJust $ T.stripSuffix ("." <> typeSuffix) sk, typeSuffix)
                   else case number of
                            NumberInt _   -> (sk, "int")
                            NumberReal _  -> (sk, "real")

textToUtf8ByteString :: Text -> ByteString
textToUtf8ByteString txt = encodeUtf8 txt

\end{code}
