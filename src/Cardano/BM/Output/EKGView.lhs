
\subsection{Cardano.BM.Output.EKGView}
\label{module:Cardano.BM.Output.EKGView}

%if style == newcode
\begin{code}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.Output.EKGView
    (
      EKGView
    , effectuate
    , realize
    , unrealize
    ) where

import           Control.Concurrent (killThread)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar,
                     takeMVar)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text, pack)
import           Data.Version (showVersion)

import qualified System.Metrics.Gauge as Gauge
import qualified System.Metrics.Label as Label
import           System.Remote.Monitoring (Server, forkServer, getGauge,
                     getLabel, serverThreadId)

import           Paths_iohk_monitoring (version)

import           Cardano.BM.Configuration (getEKGport)
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.LogItem

\end{code}
%endif

\subsubsection{Structure of EKGView}\label{code:EKGView}
\begin{code}
type EKGViewMVar = MVar EKGViewInternal
newtype EKGView = EKGView
    { getEV :: EKGViewMVar }

data EKGViewInternal = EKGViewInternal
    { evGauges  :: HM.HashMap Text Gauge.Gauge
    , evLabels  :: HM.HashMap Text Label.Label
    , evServer :: Server
    }

\end{code}

\subsubsection{EKG view is an effectuator}
\begin{code}
instance IsEffectuator EKGView where
    effectuate ekgview item =
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

\subsubsection{|EKGView| implements |Backend| functions}

|EKGView| is an \nameref{code:IsBackend}
\begin{code}
instance IsBackend EKGView where
    typeof _ = EKGViewBK
    
    realize config = do
        evref <- newEmptyMVar
        evport <- getEKGport config
        ehdl <- forkServer "127.0.0.1" evport
        ekghdl <- getLabel "iohk-monitoring version" ehdl
        Label.set ekghdl $ pack(showVersion version)
        putMVar evref $ EKGViewInternal
                        { evGauges = HM.empty
                        , evLabels = HM.empty
                        , evServer = ehdl
                        }
        return $ EKGView evref

    unrealize ekgview = do
        ekg <- takeMVar $ getEV ekgview
        killThread $ serverThreadId $ evServer ekg

\end{code}

\subsubsection{Interactive testing |EKGView|}
\begin{spec}
test :: IO ()
test = do
    c <- Cardano.BM.Configuration.setup "test/config.yaml"
    ev <- Cardano.BM.Output.EKGView.realize c

    effectuate ev $ LogNamed "test.questions" (LP (LogValue "answer" 42))
    effectuate ev $ LogNamed "test.monitor023" (LP (LogMessage (LogItem Public Warning "!!!! ALARM !!!!")))
\end{spec}
