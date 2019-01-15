
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
                     readMVar, takeMVar)
import           Control.Monad.IO.Class (liftIO)
import           Data.Functor.Contravariant (Op (..))
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text, pack, stripPrefix)
import           Data.Version (showVersion)

import qualified System.Metrics.Label as Label
import           System.Remote.Monitoring (Server, forkServer,
                     getLabel, serverThreadId)

import           Paths_iohk_monitoring (version)

import qualified Cardano.BM.BaseTrace as BaseTrace
import           Cardano.BM.Configuration (Configuration, getEKGport)
import           Cardano.BM.Data.Aggregated
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.SubTrace
import           Cardano.BM.Data.Trace
import qualified Cardano.BM.Trace as Trace

\end{code}
%endif

\subsubsection{Structure of EKGView}\label{code:EKGView}
\begin{code}
type EKGViewMVar = MVar EKGViewInternal
newtype EKGView = EKGView
    { getEV :: EKGViewMVar }

data EKGViewInternal = EKGViewInternal
    { evLabels  :: HM.HashMap Text Label.Label
    , evServer  :: Server
    , evTrace   :: Trace IO
    }

\end{code}

\begin{code}
ekgTrace :: EKGView -> Configuration -> IO (Trace IO)
ekgTrace ekg c = do
    let trace = ekgTrace' ekg
        ctx   = TraceContext { loggerName = ""
                             , configuration = c
                             , minSeverity = Debug
                             , tracetype = Neutral
                             , shutdown = pure ()
                             }
    Trace.subTrace "#ekgview" (ctx,trace)
  where
    ekgTrace' :: EKGView -> TraceNamed IO
    ekgTrace' ekgview = BaseTrace.BaseTrace $ Op $ \lognamed -> do
        let setlabel :: Text -> Text -> EKGViewInternal -> IO (Maybe EKGViewInternal)
            setlabel name label ekg_i@(EKGViewInternal labels server _) =
                case HM.lookup name labels of
                    Nothing -> do
                        ekghdl <- getLabel name server
                        Label.set ekghdl label
                        return $ Just $ ekg_i { evLabels = HM.insert name ekghdl labels}
                    Just ekghdl -> do
                        Label.set ekghdl label
                        return Nothing

            update :: LogObject -> LoggerName -> EKGViewInternal -> IO (Maybe EKGViewInternal)
            update (LP (LogMessage logitem)) logname ekg_i =
                setlabel logname (liPayload logitem) ekg_i
            update (LP (LogValue iname value)) logname ekg_i =
                let logname' = logname <> "." <> iname
                in
                setlabel logname' (pack $ show value) ekg_i

            update _ _ _ = return Nothing

        ekgup <- takeMVar (getEV ekgview)
        let lognam0 = (lnName lognamed)
            lognam1 = case stripPrefix "#ekgview.#aggregation." lognam0 of
                      Nothing -> lognam0
                      Just ln' -> ln' 
            logname = case stripPrefix "#ekgview." lognam1 of
                      Nothing -> lognam1
                      Just ln' -> ln' 
        upd <- update (lnItem lognamed) logname ekgup
        case upd of
            Nothing     -> putMVar (getEV ekgview) ekgup
            Just ekgup' -> putMVar (getEV ekgview) ekgup'

\end{code}


\subsubsection{EKG view is an effectuator}
\begin{code}
instance IsEffectuator EKGView where
    effectuate ekgview item = do
        ekg <- readMVar (getEV ekgview)
        let trace0 = evTrace ekg
        trace <- Trace.appendName (lnName item) trace0
        case (lnItem item) of
            AggregatedMessage ags -> liftIO $ do
                let traceAgg :: [(Text,Aggregated)] -> IO ()
                    traceAgg [] = return ()
                    traceAgg ((n,AggregatedEWMA ewma):r) = do
                        trace' <- Trace.appendName n trace
                        Trace.traceNamedObject trace' (LP (LogValue "avg" $ avg ewma))
                        traceAgg r
                    traceAgg ((n,AggregatedStats stats):r) = do
                        trace' <- Trace.appendName n trace
                        Trace.traceNamedObject trace' (LP (LogValue "mean" (PureD $ meanOfStats stats)))
                        Trace.traceNamedObject trace' (LP (LogValue "min" $ fmin stats))
                        Trace.traceNamedObject trace' (LP (LogValue "max" $ fmax stats))
                        Trace.traceNamedObject trace' (LP (LogValue "count" $ PureI $ fcount stats))
                        Trace.traceNamedObject trace' (LP (LogValue "last" $ flast stats))
                        Trace.traceNamedObject trace' (LP (LogValue "stdev" (PureD $ stdevOfStats stats)))
                        traceAgg r
                traceAgg ags
            _                     -> liftIO $ Trace.traceNamedObject trace (lnItem item)

\end{code}

\subsubsection{|EKGView| implements |Backend| functions}

|EKGView| is an |IsBackend|
\begin{code}
instance IsBackend EKGView where
    typeof _ = EKGViewBK
    
    realize config = do
        evref <- newEmptyMVar
        let ekgview = EKGView evref
        evport <- getEKGport config
        ehdl <- forkServer "127.0.0.1" evport
        ekghdl <- getLabel "iohk-monitoring version" ehdl
        Label.set ekghdl $ pack(showVersion version)
        ekgtrace <- ekgTrace ekgview config
        putMVar evref $ EKGViewInternal
                        { evLabels = HM.empty
                        , evServer = ehdl
                        , evTrace = ekgtrace
                        }
        return ekgview

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
