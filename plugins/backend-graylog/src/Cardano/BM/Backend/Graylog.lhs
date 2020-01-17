
\subsection{Cardano.BM.Backend.Graylog}
\label{code:Cardano.BM.Backend.Graylog}

%if style == newcode
\begin{code}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Cardano.BM.Backend.Graylog
    (
      Graylog
    -- * Plugin
    , plugin
    ) where

import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar,
                     readMVar, withMVar, tryTakeMVar)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import           Control.Exception.Safe (SomeException, catch, throwM)
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (FromJSON, ToJSON (..), Value, encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as BS8
import           Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import qualified Network.Socket as Net
import           Network.Socket.ByteString (sendAll)
import           System.IO (stderr)
import           Text.Printf (printf)

import           Cardano.BM.Backend.ProcessQueue (processQueue)
import           Cardano.BM.Configuration (Configuration, getGraylogPort)
import           Cardano.BM.Data.Aggregated
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Severity
import           Cardano.BM.Plugin
import qualified Cardano.BM.Trace as Trace

\end{code}
%endif

\subsubsection{Plugin definition}
\begin{code}
plugin :: (IsEffectuator s a, ToJSON a, FromJSON a)
       => Configuration -> Trace.Trace IO a -> s a -> IO (Plugin a)
plugin config trace sb = do
    be :: Cardano.BM.Backend.Graylog.Graylog a <- realizefrom config trace sb
    return $ BackendPlugin
               (MkBackend { bEffectuate = effectuate be, bUnrealize = unrealize be })
               (bekind be)
\end{code}

\subsubsection{Structure of Graylog}\label{code:Graylog}\index{Graylog}
\begin{code}
type GraylogMVar a = MVar (GraylogInternal a)
newtype Graylog a = Graylog
    { getGL :: GraylogMVar a }

data GraylogInternal a = GraylogInternal
    { glQueue    :: TBQ.TBQueue (Maybe (LogObject a))
    , glDispatch :: Async.Async ()
    }

\end{code}

\subsubsection{Graylog is an effectuator}\index{Graylog!instance of IsEffectuator}
Function |effectuate| is called to pass in a |LogObject| to forward to Graylog.
In case the queue is full, all new items are dropped.
\begin{code}
instance IsEffectuator Graylog a where
    effectuate graylog item = do
        gelf <- readMVar (getGL graylog)
        let enqueue a = do
                        nocapacity <- atomically $ TBQ.isFullTBQueue (glQueue gelf)
                        if nocapacity
                        then handleOverflow graylog
                        else atomically $ TBQ.writeTBQueue (glQueue gelf) (Just a)
        case item of
            (LogObject logname lometa (AggregatedMessage ags)) -> liftIO $ do
                let traceAgg :: [(Text,Aggregated)] -> IO ()
                    traceAgg [] = return ()
                    traceAgg ((n,AggregatedEWMA agewma):r) = do
                        enqueue $ LogObject (logname <> [n]) lometa (LogValue "avg" $ avg agewma)
                        traceAgg r
                    traceAgg ((n,AggregatedStats stats):r) = do
                        let statsname = logname <> [n]
                            qbasestats s' nm = do
                                enqueue $ LogObject nm lometa (LogValue "mean" (PureD $ meanOfStats s'))
                                enqueue $ LogObject nm lometa (LogValue "min" $ fmin s')
                                enqueue $ LogObject nm lometa (LogValue "max" $ fmax s')
                                enqueue $ LogObject nm lometa (LogValue "count" $ PureI $ fromIntegral $ fcount s')
                                enqueue $ LogObject nm lometa (LogValue "stdev" (PureD $ stdevOfStats s'))
                        enqueue $ LogObject statsname lometa (LogValue "last" $ flast stats)
                        qbasestats (fbasic stats) $ statsname <> ["basic"]
                        qbasestats (fdelta stats) $ statsname <> ["delta"]
                        qbasestats (ftimed stats) $ statsname <> ["timed"]
                        traceAgg r
                traceAgg ags
            (LogObject _ _ (LogMessage _)) -> enqueue item
            (LogObject _ _ (LogValue _ _)) -> enqueue item
            _                              -> return ()

    handleOverflow _ = TIO.hPutStrLn stderr "Notice: Graylogs's queue full, dropping log items!"

\end{code}

\subsubsection{|Graylog| implements |Backend| functions}\index{Graylog!instance of IsBackend}

|Graylog| is an |IsBackend|
\begin{code}
instance (ToJSON a, FromJSON a) => IsBackend Graylog a where
    bekind _ = GraylogBK

    realize _ = fail "Graylog cannot be instantiated by 'realize'"

    realizefrom config sbtrace _ = do
        glref <- newEmptyMVar
        let graylog = Graylog glref
#ifdef PERFORMANCE_TEST_QUEUE
        let qSize = 1000000
#else
        let qSize = 1024
#endif
        queue <- atomically $ TBQ.newTBQueue qSize
        dispatcher <- spawnDispatcher config queue sbtrace
        -- link the given Async to the current thread, such that if the Async
        -- raises an exception, that exception will be re-thrown in the current
        -- thread, wrapped in ExceptionInLinkedThread.
        Async.link dispatcher
        putMVar glref $ GraylogInternal
                        { glQueue = queue
                        , glDispatch = dispatcher
                        }
        return graylog

    unrealize graylog = do
        let clearMVar :: MVar b -> IO ()
            clearMVar = void . tryTakeMVar

        (dispatcher, queue) <- withMVar (getGL graylog) (\gelf ->
                                return (glDispatch gelf, glQueue gelf))
        -- send terminating item to the queue
        atomically $ TBQ.writeTBQueue queue Nothing
        -- wait for the dispatcher to exit
        res <- Async.waitCatch dispatcher
        either throwM return res
        clearMVar $ getGL graylog

\end{code}

\subsubsection{Asynchronously reading log items from the queue and their processing}
\begin{code}
spawnDispatcher :: forall a. ToJSON a
                => Configuration
                -> TBQ.TBQueue (Maybe (LogObject a))
                -> Trace.Trace IO a
                -> IO (Async.Async ())
spawnDispatcher config evqueue sbtrace =
    let gltrace = Trace.appendName "#graylog" sbtrace
    in
    Async.async $ Net.withSocketsDo $ qProc gltrace Nothing
  where
    {-@ lazy qProc @-}
    qProc :: Trace.Trace IO a -> Maybe Net.Socket -> IO ()
    qProc gltrace conn =
        processQueue
            evqueue
            processGraylog
            (gltrace, conn)
            (\(_, c) -> closeConn c)

    processGraylog :: LogObject a -> (Trace.Trace IO a, Maybe Net.Socket)
                   -> IO (Trace.Trace IO a, Maybe Net.Socket)
    processGraylog item (gltrace, mConn) =
        case mConn of
            (Just conn) -> do
                sendLO conn item
                    `catch` \(e :: SomeException) -> do
                        let trace' = Trace.appendName "sending" gltrace
                        mle <- mkLOMeta Error Public
                        Trace.traceNamedObject trace' (mle, LogError (pack $ show e))
                        threadDelay 50000
                        void $ processGraylog item (gltrace, mConn)
                return (gltrace, mConn)
            Nothing     -> do
                mConn' <- tryConnect gltrace
                processGraylog item (gltrace, mConn')

    sendLO :: Net.Socket -> LogObject a -> IO ()
    sendLO conn obj =
        let msg = BS8.toStrict $ encodeMessage obj
        in sendAll conn msg
    closeConn :: Maybe Net.Socket -> IO ()
    closeConn Nothing = return ()
    closeConn (Just conn) = Net.close conn
    tryConnect :: Trace.Trace IO a -> IO (Maybe Net.Socket)
    tryConnect gltrace = do
        port <- getGraylogPort config
        let hints = Net.defaultHints { Net.addrSocketType = Net.Datagram }
        (addr:_) <- Net.getAddrInfo (Just hints) (Just "127.0.0.1") (Just $ show port)
        sock <- Net.socket (Net.addrFamily addr) (Net.addrSocketType addr) (Net.addrProtocol addr)
        Net.connect sock (Net.addrAddress addr) >> return (Just sock)
            `catch` \(e :: SomeException) -> do
                let trace' = Trace.appendName "connecting" gltrace
                mle <- mkLOMeta Error Public
                Trace.traceNamedObject trace' (mle, LogError (pack $ show e))
                return Nothing

    encodeMessage :: ToJSON a => LogObject a -> BS8.ByteString
    encodeMessage lo = encode $ mkGelfItem lo

\end{code}

\subsubsection{Gelf data structure}
GELF defines a data format of the message payload: \url{https://docs.graylog.org/en/3.0/pages/gelf.html}
\begin{code}
data GelfItem = GelfItem {
        version :: !Text,
        host :: !Text,
        short_message :: !Text,
        full_message :: !Value,
        timestamp :: !Double,
        level :: !Int,
        _tid :: !Text,
        _privacy :: !Text
    }

mkGelfItem :: ToJSON a => LogObject a -> GelfItem
mkGelfItem (LogObject loname lometa locontent) = GelfItem {
        version = "1.1",
        host = "hostname",
        short_message = loname2text loname,
        full_message = toJSON locontent,
        timestamp = (fromInteger . toInteger $ utc2ns (tstamp lometa) :: Double) / 1000000000,
        level = fromEnum (maxBound @Severity) - fromEnum (severity lometa),
        _tid = tid lometa,
        _privacy = pack $ show $ privacy lometa
    }

instance ToJSON GelfItem where
    toJSON gli = object [
            "version" .= version gli,
            "host" .= host gli,
            "short_message" .= short_message gli,
            "full_message" .= full_message gli,
            "timestamp" .= (printf "%0.3f" $ timestamp gli :: String),
            "level" .= level gli,
            "_tid" .= _tid gli,
            "_privacy" .= _privacy gli
        ]
\end{code}
