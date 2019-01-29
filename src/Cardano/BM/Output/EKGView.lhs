
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
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar,
                     readMVar, takeMVar)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as TBQ
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
    { evQueue   :: TBQ.TBQueue (Maybe NamedLogItem)
    , evLabels  :: EKGViewMap
    , evServer  :: Server
    }

\end{code}

\subsubsection{Relation from variable name to label handler}
We keep the label handlers for later update in a |HashMap|.
\begin{code}
type EKGViewMap = HM.HashMap Text Label.Label

\end{code}

\subsubsection{Internal |Trace|}
This is an internal |Trace|, named "\#ekgview", which can be used to control
the messages that are being displayed by EKG.
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
    ekgTrace' ekgview = BaseTrace.BaseTrace $ Op $ \(LogNamed lognamed lo) -> do
        let setlabel :: Text -> Text -> EKGViewInternal -> IO (Maybe EKGViewInternal)
            setlabel name label ekg_i@(EKGViewInternal _ labels server) =
                case HM.lookup name labels of
                    Nothing -> do
                        ekghdl <- getLabel name server
                        Label.set ekghdl label
                        return $ Just $ ekg_i { evLabels = HM.insert name ekghdl labels}
                    Just ekghdl -> do
                        Label.set ekghdl label
                        return Nothing

            update :: LogObject -> LoggerName -> EKGViewInternal -> IO (Maybe EKGViewInternal)
            update (LogObject _ (LogMessage logitem)) logname ekg_i =
                setlabel logname (liPayload logitem) ekg_i
            update (LogObject _ (LogValue iname value)) logname ekg_i =
                let logname' = logname <> "." <> iname
                in
                setlabel logname' (pack $ show value) ekg_i

            update _ _ _ = return Nothing

        ekgup <- takeMVar (getEV ekgview)
        let -- strip off some prefixes not necessary for display
            lognam1 = case stripPrefix "#ekgview.#aggregation." lognamed of
                      Nothing -> lognamed
                      Just ln' -> ln'
            logname = case stripPrefix "#ekgview." lognam1 of
                      Nothing -> lognam1
                      Just ln' -> ln'
        upd <- update lo logname ekgup
        case upd of
            Nothing     -> putMVar (getEV ekgview) ekgup
            Just ekgup' -> putMVar (getEV ekgview) ekgup'

\end{code}


\subsubsection{EKG view is an effectuator}
Function |effectuate| is called to pass in a |NamedLogItem| for display in EKG.
If the log item is an |AggregatedStats| message, then all its constituents are
put into the queue.
\begin{code}
instance IsEffectuator EKGView where
    effectuate ekgview item = do
        ekg <- readMVar (getEV ekgview)
        let queue a = do
                        nocapacity <- atomically $ TBQ.isFullTBQueue (evQueue ekg)
                        if nocapacity
                        then return ()
                        else atomically $ TBQ.writeTBQueue (evQueue ekg) (Just a)
        case (lnItem item) of
            (LogObject lometa (AggregatedMessage ags)) -> liftIO $ do
                let logname = lnName item
                    traceAgg :: [(Text,Aggregated)] -> IO ()
                    traceAgg [] = return ()
                    traceAgg ((n,AggregatedEWMA ewma):r) = do
                        queue $ LogNamed (logname <> "." <> n) $ LogObject lometa (LogValue "avg" $ avg ewma)
                        traceAgg r
                    traceAgg ((n,AggregatedStats stats):r) = do
                        let statsname = logname <> "." <> n
                            qbasestats s' nm = do
                                queue $ LogNamed nm $ LogObject lometa (LogValue "mean" (PureD $ meanOfStats s'))
                                queue $ LogNamed nm $ LogObject lometa (LogValue "min" $ fmin s')
                                queue $ LogNamed nm $ LogObject lometa (LogValue "max" $ fmax s')
                                queue $ LogNamed nm $ LogObject lometa (LogValue "count" $ PureI $ fromIntegral $ fcount s')
                                queue $ LogNamed nm $ LogObject lometa (LogValue "stdev" (PureD $ stdevOfStats s'))
                        queue $ LogNamed statsname $ LogObject lometa (LogValue "last" $ flast stats)
                        qbasestats (fbasic stats) $ statsname <> ".basic"
                        qbasestats (fdelta stats) $ statsname <> ".delta"
                        qbasestats (ftimed stats) $ statsname <> ".timed"
                        traceAgg r
                traceAgg ags
            (LogObject _ (LogMessage _)) -> queue item
            (LogObject _ (LogValue _ _)) -> queue item
            _                            -> return ()

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
        queue <- atomically $ TBQ.newTBQueue 512
        dispatcher <- spawnDispatcher queue ekgtrace
        -- link the given Async to the current thread, such that if the Async
        -- raises an exception, that exception will be re-thrown in the current
        -- thread, wrapped in ExceptionInLinkedThread.
        Async.link dispatcher
        putMVar evref $ EKGViewInternal
                        { evLabels = HM.empty
                        , evServer = ehdl
                        , evQueue = queue
                        }
        return ekgview

    unrealize ekgview = do
        ekg <- takeMVar $ getEV ekgview
        killThread $ serverThreadId $ evServer ekg

\end{code}

\subsubsection{Asynchrouniously reading log items from the queue and their processing}
\begin{code}
spawnDispatcher :: TBQ.TBQueue (Maybe NamedLogItem)
                -> Trace.Trace IO
                -> IO (Async.Async ())
spawnDispatcher evqueue trace =
    Async.async $ qProc
  where
    qProc = do
        maybeItem <- atomically $ TBQ.readTBQueue evqueue
        case maybeItem of
            Just (LogNamed logname logvalue) -> do
                trace' <- Trace.appendName logname trace
                Trace.traceNamedObject trace' logvalue
                qProc
            Nothing -> return ()  -- stop here

\end{code}

\subsubsection{Interactive testing |EKGView|}
\begin{spec}
test :: IO ()
test = do
    c <- Cardano.BM.Setup.setupTrace (Left "test/config.yaml") "ekg"
    ev <- Cardano.BM.Output.EKGView.realize c

    effectuate ev $ LogNamed "test.questions" (LogValue "answer" 42)
    effectuate ev $ LogNamed "test.monitor023" (LogMessage (LogItem Public Warning "!!!! ALARM !!!!"))
\end{spec}
