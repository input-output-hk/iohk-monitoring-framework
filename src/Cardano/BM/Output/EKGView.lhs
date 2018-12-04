
\subsection{Cardano.BM.Output.EKGView}
\label{module:Cardano.BM.Output.EKGView}

%if style == newcode
\begin{code}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.Output.EKGView
    (
      EKGView
    , setup
    , pass
    --, takedown
    ) where

import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar,
                     takeMVar)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)

import qualified System.Metrics.Gauge as Gauge
import qualified System.Metrics.Label as Label
import           System.Remote.Monitoring (Server, forkServer, getGauge,
                     getLabel)

import           Cardano.BM.Configuration (Configuration)
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.LogItem

\end{code}
%endif

The ekgview is a singleton.
\begin{code}
type EKGViewMVar = MVar EKGViewInternal
newtype EKGView = EKGView
    { getEV :: EKGViewMVar }

-- Our internal state
data EKGViewInternal = EKGViewInternal
    { evGauges  :: HM.HashMap Text Gauge.Gauge
    , evLabels  :: HM.HashMap Text Label.Label
    , _ekgServer :: Server
    }

\end{code}

\begin{code}
setup :: Configuration -> IO EKGView
setup _ = do
    evref <- newEmptyMVar
    ehdl <- forkServer "127.0.0.1" 16543
    putMVar evref $ EKGViewInternal HM.empty HM.empty ehdl
    return $ EKGView evref

\end{code}

\begin{code}
instance HasPass EKGView where
    pass ekgview item =
        let update :: LogObject -> LoggerName -> EKGViewInternal -> IO (Maybe EKGViewInternal)
            update (LP (LogMessage logitem)) logname ekg@(EKGViewInternal _ labels server) =
                case HM.lookup logname labels of
                    Nothing -> do
                        ekghdl <- getLabel logname server
                        Label.set ekghdl (liPayload logitem)
                        return $ Just $ ekg { evLabels = HM.insert logname ekghdl labels}
                    Just ekghdl -> do
                        Label.set ekghdl (liPayload logitem)
                        return Nothing
            update (LP (LogValue iname value)) logname ekg@(EKGViewInternal gauges _ server) =
                let name = logname <> "." <> iname
                in
                case HM.lookup name gauges of
                    Nothing -> do
                        ekghdl <- getGauge name server
                        Gauge.set ekghdl (fromInteger value)
                        return $ Just $ ekg { evGauges = HM.insert name ekghdl gauges}
                    Just ekghdl -> do
                        Gauge.set ekghdl (fromInteger value)
                        return Nothing

            update _ _ _ = return Nothing
        in do
        ekg <- takeMVar (getEV ekgview)
        upd <- update (lnItem item) (lnName item) ekg
        case upd of
            Nothing   -> putMVar (getEV ekgview) ekg
            Just ekg' -> putMVar (getEV ekgview) ekg'

\end{code}

