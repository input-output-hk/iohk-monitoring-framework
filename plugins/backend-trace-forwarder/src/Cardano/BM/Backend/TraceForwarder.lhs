
\subsection{Cardano.BM.Backend.TraceForwarder}
\label{module:Cardano.BM.Backend.TraceForwarder}

%if style == newcode
\begin{code}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

#if !defined(mingw32_HOST_OS)
#define POSIX
#endif

module Cardano.BM.Backend.TraceForwarder
    ( TraceForwarder (..)
    -- * Plugin
    , plugin
    ) where

import           Control.Exception
import           Control.Monad (forever, when)
import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import           Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe (fromMaybe)
import           Data.Text (Text, unpack)
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import           Data.Typeable (Typeable)
import           GHC.Natural (Natural)

import qualified Network.Socket as Socket
import           System.IO (Handle, IOMode (..), hClose, stderr)
import           Text.Read (readMaybe)

import           Cardano.BM.Configuration
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.Configuration (RemoteAddr(..))
import           Cardano.BM.Data.LogItem (LOMeta (..), LogObject (..))
import           Cardano.BM.Data.Severity (Severity (..))
import           Cardano.BM.IOManager
import           Cardano.BM.Plugin
import qualified Cardano.BM.Snocket as Snocket
import qualified Cardano.BM.Trace as Trace

\end{code}
%endif

|TraceForwarder| is a backend responsible for redirecting logs to a
different process (running a |TraceAcceptor| backend), by means of
either a pipe or a socket.

The |TraceForwarder| is looking up a minimum |Severity| in the options
section of the configuration. This filters out all messages that have not at
least the |Severity|.
\subsubsection{Plugin definition}
\begin{code}
plugin :: forall a s . (IsEffectuator s a, ToJSON a, FromJSON a)
       => Configuration -> Trace.Trace IO a -> s a -> Text -> IO (Plugin a)
plugin config _trace _sb tfid = do
    opts <- getTextOption config tfid
    let minsev = case opts of
                   Nothing -> Debug
                   Just sevtext -> fromMaybe Debug (readMaybe $ unpack sevtext)
    be :: Cardano.BM.Backend.TraceForwarder.TraceForwarder a <- realize config
    dispatcherThr <- spawnDispatcher config (getTF be)
    modifyMVar_ (getTF be) $ \initialBE ->
      return $ initialBE
                 { tfFilter     = minsev
                 , tfDispatcher = Just dispatcherThr
                 }
    return $ BackendPlugin
               (MkBackend { bEffectuate = effectuate be, bUnrealize = unrealize be })
               (bekind be)

\end{code}

\subsubsection{Structure of TraceForwarder}\label{code:TraceForwarder}\index{TraceForwarder}
Contains the handler to the pipe or to the socket.
\begin{code}
newtype TraceForwarder a = TraceForwarder
    { getTF :: TraceForwarderMVar a }

type TraceForwarderMVar a = MVar (TraceForwarderInternal a)

data TraceForwarderInternal a = TraceForwarderInternal
    { tfQueue      :: TBQ.TBQueue (LogObject a)
    , tfHandle     :: Maybe Handle
    , tfRemoteAddr :: RemoteAddr
    , tfFilter     :: Severity
    , tfDispatcher :: Maybe (Async.Async ())
    }
\end{code}

\subsubsection{TraceForwarder is an effectuator}\index{TraceForwarder!instance of IsEffectuator}
Every |LogObject| before being written to the given handler is converted to
|ByteString| through its |JSON| representation.
\begin{code}
instance (ToJSON a) => IsEffectuator TraceForwarder a where
    effectuate tf lo = do
        let currentMVar = getTF tf
        currentTF <- readMVar currentMVar
        when (severity (loMeta lo) >= tfFilter currentTF) $ do
          let queue = tfQueue currentTF
          noCapacity <- atomically $ TBQ.isFullTBQueue queue
          if noCapacity
            then handleOverflow tf
            else atomically $ TBQ.writeTBQueue queue lo

    handleOverflow _ = TIO.hPutStrLn stderr "Notice: TraceForwarder's queue is full, dropping log items!"

\end{code}


\subsubsection{|TraceForwarder| implements |Backend| functions}\index{TraceForwarder!instance of IsBackend}

|TraceForwarder| is an |IsBackend|
\begin{code}
instance (FromJSON a, ToJSON a) => IsBackend TraceForwarder a where
    type BackendFailure TraceForwarder = TraceForwarderBackendFailure

    bekind _ = TraceForwarderBK

    realize cfg = getForwardTo cfg >>= \case
      Nothing -> fail "Trace forwarder not configured:  option 'forwardTo'"
      Just addr -> do
        queue <- atomically $ TBQ.newTBQueue queueMaxSize
        tfMVar <- newMVar $ TraceForwarderInternal
                              { tfQueue      = queue
                              , tfHandle     = Nothing
                              , tfRemoteAddr = addr
                              , tfFilter     = Debug
                              , tfDispatcher = Nothing
                              }
        return $ TraceForwarder tfMVar

    unrealize tf = do
      currentTF <- readMVar (getTF tf)
      -- Cancel dispatcher thread.
      case tfDispatcher currentTF of
        Nothing  -> return ()
        Just thr -> Async.uninterruptibleCancel thr
      -- If there's a handle - close it.
      closeHandle $ tfHandle currentTF

closeHandle :: Maybe Handle -> IO ()
closeHandle (Just h) = hClose h
closeHandle Nothing  = return ()

connectForwarder :: IOManager -> RemoteAddr -> IO Handle
connectForwarder iomgr (RemotePipe pipePath) = do
  let sn = Snocket.localSnocket iomgr pipePath
  Snocket.localFDToHandle =<< doConnect sn (Snocket.localAddressFromPath pipePath)
connectForwarder iomgr (RemoteSocket host port) = do
  let sn = Snocket.socketSnocket iomgr
  addrs <- Socket.getAddrInfo Nothing (Just host) (Just port)
  case addrs of
    [] -> throwIO (TraceForwarderSocketError ("bad socket address: " <> host <> ":" <> port))
    a:_ -> doConnect sn (Socket.addrAddress a)
           >>= flip Socket.socketToHandle ReadWriteMode

doConnect :: Snocket.Snocket IO fd addr -> addr -> IO fd
doConnect sn remoteAddr = do
  sd <- Snocket.openToConnect sn remoteAddr
  Snocket.connect sn sd remoteAddr
  pure sd

data TraceForwarderBackendFailure
  = TraceForwarderConnectionError String
  | TraceForwarderSocketError String
  deriving (Show, Typeable)

instance Exception TraceForwarderBackendFailure

\end{code}

\subsubsection{Asynchronously reading log items from the queue and sending them to an acceptor.}
\begin{code}
spawnDispatcher :: ToJSON a => Configuration -> TraceForwarderMVar a -> IO (Async.Async ())
spawnDispatcher config tfMVar = do
  -- To reduce network traffic it's possible to send log items not one by one,
  -- but collect them in the queue and periodically send this list as one ByteString.
  forwardDelay <- getForwardDelay config >>= \case
    Nothing -> pure defaultDelayInMs
    Just delayInMs -> pure delayInMs
  Async.async $ processQueue forwardDelay
 where
  -- If the configuration doesn't specify forward delay, use default one.
  defaultDelayInMs :: Word
  defaultDelayInMs = 1000

  processQueue :: Word -> IO ()
  processQueue delayInMs = forever $ do
    threadDelay $ fromIntegral delayInMs * 1000
    currentTF <- readMVar tfMVar
    itemsList <- atomically $ TBQ.flushTBQueue (tfQueue currentTF)
    -- Try to write it to the handle. If there's a problem with connection,
    -- this thread will initiate re\-establishing of the connection and
    -- will wait until it's established.
    sendItems config tfMVar itemsList

-- Try to send log items to the handle.
sendItems :: ToJSON a => Configuration -> TraceForwarderMVar a -> [LogObject a] -> IO ()
sendItems _ _ [] = return ()
sendItems config tfMVar items@(lo:_) =
  tfHandle <$> readMVar tfMVar >>= \case
    Nothing -> do
      -- There's no handle, initiate the connection.
      establishConnection 1 1 tfMVar
      -- Connection is re\-established, try to send log item.
      sendItems config tfMVar items
    Just h ->
      try (BSC.hPutStrLn h $! encodedHostname) >>= \case
        Right _ ->
          -- Hostname was written to the handler successfully,
          -- try to write serialized list of LogObjects.
          try (BSC.hPutStrLn h $! bs) >>= \case
            Right _ ->
              return () -- Everything is ok, LogObjects were written to the handler.
            Left (_e :: IOException) -> do
              reConnectIfQueueIsAlmostFull
              threadDelay 10000
              sendItems config tfMVar items
        Left (_e :: IOException) -> do
          reConnectIfQueueIsAlmostFull
          threadDelay 10000
          sendItems config tfMVar items
 where
  encodedHostname = encodeUtf8 (hostname . loMeta $ lo)

  (_, bs) = jsonToBS items

  jsonToBS :: ToJSON b => b -> (Int, BS.ByteString)
  jsonToBS a =
    let bs' = BL.toStrict $ encode a
    in (BS.length bs', bs')

  -- Handle is bad, it looks like the connection is broken.
  -- Check if the queue is almost full.
  reConnectIfQueueIsAlmostFull = do
    currentTF <- readMVar tfMVar
    currentQueueSize <- atomically $ TBQ.lengthTBQueue (tfQueue currentTF)
    when (queueIsAlmostFull currentQueueSize) $ do
      -- The queue is almost full, it means that log items will be dropped soon.
      -- Initiate re-establishing of connection.
      closeHandle $ tfHandle currentTF
      modifyMVar_ tfMVar $ \be -> return $ be { tfHandle = Nothing }

  -- When the queue is almost full (80 percent of its max size)
  -- we initiate re-establishing of connection.
  queueIsAlmostFull queueSize = queueSize >= round almostFullSize
   where
    almostFullSize :: Float
    almostFullSize = 0.8 * fromIntegral queueMaxSize

queueMaxSize :: Natural
queueMaxSize = 500

establishConnection :: Int -> Int -> TraceForwarderMVar a -> IO ()
establishConnection delayInSec delayInSec' tfMVar = withIOManager $ \iomgr -> do
  addr <- tfRemoteAddr <$> readMVar tfMVar
  try (connectForwarder iomgr addr) >>= \case
    Right h ->
      modifyMVar_ tfMVar $ \be -> return $ be { tfHandle = Just h }
    Left (e :: IOException) -> do
      -- Cannot establish it, let's try again..
      threadDelay $ 1000000 * delayInSec'
      if delayInSec' < 60
        then
          -- Next attempt to re-establish the connection will be perform after Fibonacci-calculated delay.
          establishConnection delayInSec' (delayInSec + delayInSec') tfMVar
        else
          -- Next attempt to re-establish the connection will be perform after fixed delay (1 minute).
          establishConnection 1 60 tfMVar

\end{code}
