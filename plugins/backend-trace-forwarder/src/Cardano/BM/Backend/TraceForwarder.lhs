
\subsection{Cardano.BM.Backend.TraceForwarder}
\label{module:Cardano.BM.Backend.TraceForwarder}

%if style == newcode
\begin{code}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
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
import           Control.Monad (forever, mapM_, void, when)
import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVarIO)
import           Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import           Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text, pack, unpack)
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
import           Cardano.BM.Data.LogItem (LOContent (..), LOMeta (..), LogObject (..))
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

The callback 'getStateDigest' is used as a source of |LogObject|s
that should be sent additionally, only once after the connection is
re\-established. The application that uses 'lobemo-backend-trace-forwarder'
plugin provides this callback.

Note [Handle and GC]
~~~~~~~~~~~~~~~~~~~~
'tfHandle' contains |Maybe| |Handle| which will be updated periodically:
the main reason of it is a reconnection with 'TraceAcceptorBK' plugin in other process.
Once the current value of 'tfHandle' is replaced by |Nothing|, later it will be
cleaned up by GC, please see the documentation:
https://hackage.haskell.org/package/base-4.14.0.0/docs/System-IO.html#t:Handle

\subsubsection{Plugin definition}
\begin{code}
plugin :: forall a s . (IsEffectuator s a, ToJSON a, FromJSON a)
       => Configuration
       -> Trace.Trace IO a
       -> s a
       -> Text
       -> IO [LogObject a]
       -> IO (Plugin a)
plugin config _trace _sb tfid getStateDigest = do
    opts <- getTextOption config tfid
    let minsev = case opts of
                   Nothing -> Debug
                   Just sevtext -> fromMaybe Debug (readMaybe $ unpack sevtext)
    be :: Cardano.BM.Backend.TraceForwarder.TraceForwarder a <- realize config
    dispatcherThr <- spawnDispatcher config (getTF be)
    atomically $ modifyTVar' (getTF be) $ \initialBE ->
      initialBE
        { tfFilter         = minsev
        , tfDispatcher     = Just dispatcherThr
        , tfGetStateDigest = getStateDigest
        }
    return $ BackendPlugin
               (MkBackend { bEffectuate = effectuate be, bUnrealize = unrealize be })
               (bekind be)

\end{code}

\subsubsection{Structure of TraceForwarder}\label{code:TraceForwarder}\index{TraceForwarder}
Contains the handler to the pipe or to the socket.
\begin{code}
newtype TraceForwarder a = TraceForwarder
    { getTF :: TraceForwarderTVar a }

type TraceForwarderTVar a = TVar (TraceForwarderInternal a)

data TraceForwarderInternal a = TraceForwarderInternal
    { tfQueue            :: TBQ.TBQueue (LogObject a)
    , tfHandle           :: Maybe Handle
    , tfRemoteAddr       :: RemoteAddr
    , tfFilter           :: Severity
    , tfDispatcher       :: Maybe (Async.Async ())
    , tfQueueFullCounter :: IORef Int
    , tfGetStateDigest   :: IO [LogObject a]
    }
\end{code}

\subsubsection{TraceForwarder is an effectuator}\index{TraceForwarder!instance of IsEffectuator}
Every |LogObject| before being written to the given handler is converted to
|ByteString| through its |JSON| representation.
\begin{code}
instance (ToJSON a) => IsEffectuator TraceForwarder a where
    effectuate tf lo = do
      currentTF <- readTVarIO $ getTF tf
      -- Severity filter allows to ignore LogObjects with too low severity.
      -- However, errors and metrics should be forwarded in any case,
      -- regardless their severity.
      if isError
        then writeMessageToQueue currentTF
        else case loContent lo of
               LogValue _ _ -> writeMessageToQueue currentTF
               _            -> when (severity (loMeta lo) >= tfFilter currentTF) $
                                 writeMessageToQueue currentTF
     where
      isError = errByConstr || errBySev
       where
        errByConstr =
          case loContent lo of
            LogError _ -> True
            _          -> False
        errBySev = severity (loMeta lo) >= Error

      writeMessageToQueue currentTF' = do
        let queue = tfQueue currentTF'
        (,) currentQueueSize noCapacity <- atomically $
            (,) <$> TBQ.lengthTBQueue queue
                <*> TBQ.isFullTBQueue queue
        if | noCapacity -> do
             let counterIORef = tfQueueFullCounter currentTF'
             overflowed <- atomicModifyIORef' counterIORef $ \counter ->
               if counter >= overflowCriticalNum
                 then (1, True)
                 else (counter + 1, False)
             when overflowed $ handleOverflow tf
           | queueIsAlmostFull currentQueueSize -> do
             -- Since the queue is almost full, it probably means that
             -- the connection with acceptor is broken or too slow.
             -- Remove current tfHandle, please see Note [Handle and GC].
             atomically $ modifyTVar' (getTF tf) $ \be -> be { tfHandle = Nothing }
             -- Spawn new thread to establish new connection.
             void $ Async.async $ establishConnection (getTF tf)
             -- Since the queue is not full yet, write |LogObject| in it.
             atomically $ TBQ.writeTBQueue queue lo
           | otherwise ->
             atomically $ TBQ.writeTBQueue queue lo

      queueIsAlmostFull queueSize = queueSize >= round almostFullSize
       where
        almostFullSize :: Float
        almostFullSize = 0.8 * fromIntegral queueMaxSize

    handleOverflow _ = TIO.hPutStrLn stderr $ "Notice: TraceForwarder's queue is full, "
                                              <> pack (show overflowCriticalNum)
                                              <> " log items were dropped!"

overflowCriticalNum :: Int
overflowCriticalNum = 200

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
        counter <- newIORef 0
        tfTVar <- newTVarIO $
          TraceForwarderInternal
            { tfQueue            = queue
            , tfHandle           = Nothing
            , tfRemoteAddr       = addr
            , tfFilter           = Debug
            , tfDispatcher       = Nothing
            , tfQueueFullCounter = counter
            , tfGetStateDigest   = return []
            }
        return $ TraceForwarder tfTVar

    unrealize tf = do
      currentTF <- readTVarIO (getTF tf)
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
spawnDispatcher :: ToJSON a => Configuration -> TraceForwarderTVar a -> IO (Async.Async ())
spawnDispatcher config tfTVar = do
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
    -- First of all, check if tfHandle contains an actual handle.
    currentTF <- readTVarIO tfTVar
    case tfHandle currentTF of
      Nothing ->
        -- There's no handle. It means that connection isn't established yet,
        -- so there's no need to read items from the queue, just try to establish
        -- the new connection.
        void $ Async.async $ establishConnection tfTVar
      Just h -> do
        -- The handle is here: the connection is established and probably alive,
        -- so read items from the queue and try to write them in the handle.
        itemsList <- atomically $ TBQ.flushTBQueue (tfQueue currentTF)
        sendItems config tfTVar h itemsList

-- Try to send log items to the handle.
sendItems :: ToJSON a
          => Configuration
          -> TraceForwarderTVar a
          -> Handle
          -> [LogObject a]
          -> IO ()
sendItems _ _ _ [] = return ()
sendItems config tfTVar h items@(lo:_) =
  try (BSC.hPutStrLn h $! encodedHostname) >>= \case
    Right _ ->
      -- Hostname was written to the handler successfully,
      -- try to write serialized list of LogObjects.
      try (BSC.hPutStrLn h $! bs) >>= \case
        Right _ ->
          return () -- Everything is ok, LogObjects were written to the handler.
        Left (_e :: IOException) -> do
          -- Handle is bad, it looks like the connection is already broken.
          -- Remove bad handle to initiate reconnection, please see Note [Handle and GC].
          atomically $ modifyTVar' tfTVar $ \be -> be { tfHandle = Nothing }
          -- Spawn new thread to establish new connection.
          void $ Async.async $ establishConnection tfTVar
    Left (_e :: IOException) -> do
      -- Handle is bad, it looks like the connection is already broken.
      -- Remove bad handle, please see Note [Handle and GC].
      atomically $ modifyTVar' tfTVar $ \be -> be { tfHandle = Nothing }
      -- Spawn new thread to establish new connection.
      void $ Async.async $ establishConnection tfTVar
 where
  encodedHostname = encodeUtf8 (hostname . loMeta $ lo)

  (_, bs) = jsonToBS items

  jsonToBS :: ToJSON b => b -> (Int, BS.ByteString)
  jsonToBS a =
    let bs' = BL.toStrict $ encode a
    in (BS.length bs', bs')

queueMaxSize :: Natural
queueMaxSize = 2500

establishConnection :: TraceForwarderTVar a -> IO ()
establishConnection tfTVar = do
  currentTF <- readTVarIO tfTVar
  case tfHandle currentTF of
    Nothing -> do
      -- Ok, there's no handle yet, as we expected, so try to establish the connection.
      doEstablishConnection 1 1 tfTVar
      -- So, the connection is established, so get the digest objects and put them into the queue.
      stateItems <- tfGetStateDigest currentTF
      atomically $ mapM_ (TBQ.writeTBQueue (tfQueue currentTF)) stateItems
    Just _ ->
      -- The handle is already here, which means that this function
      -- was called from other thread and already established the connection,
      -- do nothing in this case.
      return ()

doEstablishConnection :: Int -> Int -> TraceForwarderTVar a -> IO ()
doEstablishConnection delayInSec delayInSec' tfTVar = withIOManager $ \iomgr -> do
  addr <- tfRemoteAddr <$> readTVarIO tfTVar
  try (connectForwarder iomgr addr) >>= \case
    Right h ->
      -- Connection is established, update tfHandle.
      atomically $ modifyTVar' tfTVar $ \be -> be { tfHandle = Just h }
    Left (_e :: IOException) -> do
      -- Cannot establish it, let's try again..
      threadDelay $ 1000000 * delayInSec'
      if delayInSec' < 60
        then
          -- Next attempt to re-establish the connection will be perform after Fibonacci-calculated delay.
          doEstablishConnection delayInSec' (delayInSec + delayInSec') tfTVar
        else
          -- Next attempt to re-establish the connection will be perform after fixed delay (1 minute).
          doEstablishConnection 1 60 tfTVar

\end{code}
