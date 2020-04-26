
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
import           Control.Monad (when)
import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import           Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe (fromMaybe)
import           Data.Text (Text, unpack)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Typeable (Typeable)

import qualified Network.Socket as Socket
import           System.IO (Handle, IOMode (..), hClose)
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
    -- Currently there's no connection with TraceAcceptor yet, the first attempt to establish it...
    connectorThr <- Async.async $ establishConnection (getTF be)
    modifyMVar_ (getTF be) $ \initialBE ->
      return $ initialBE
                 { tfFilter    = minsev
                 , tfConnector = Just connectorThr
                 }
    return $ BackendPlugin
               (MkBackend { bEffectuate = effectuate be, bUnrealize = unrealize be })
               (bekind be)

\end{code}

\subsubsection{Structure of TraceForwarder}\label{code:TraceForwarder}\index{TraceForwarder}
Contains the handler to the pipe or to the socket.
\begin{code}
newtype TraceForwarder a = TraceForwarder
    { getTF :: TraceForwarderMVar }

type TraceForwarderMVar = MVar TraceForwarderInternal

data TraceForwarderInternal = TraceForwarderInternal
    { tfHandle     :: Maybe Handle
    , tfRemoteAddr :: RemoteAddr
    , tfFilter     :: Severity
    , tfConnector  :: Maybe (Async.Async ())
    }
\end{code}

\subsubsection{TraceForwarder is an effectuator}\index{TraceForwarder!instance of IsEffectuator}
Every |LogObject| before being written to the given handler is converted to
|ByteString| through its |JSON| represantation.
\begin{code}
instance (ToJSON a) => IsEffectuator TraceForwarder a where
    effectuate tf lo = do
        let currentMVar = getTF tf
        currentTF <- readMVar currentMVar
        when (severity (loMeta lo) >= tfFilter currentTF) $
          traceForwarderWriter currentMVar lo

    handleOverflow _ = return ()

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
        tfMVar <- newMVar $ TraceForwarderInternal
                              { tfHandle     = Nothing
                              , tfRemoteAddr = addr
                              , tfFilter     = Debug
                              , tfConnector  = Nothing
                              }
        return $ TraceForwarder tfMVar

    unrealize tf = do
      currentTF <- readMVar (getTF tf)
      --If connector thread is active - cancel it.
      case tfConnector currentTF of
        Nothing  -> return ()
        Just thr -> Async.uninterruptibleCancel thr
      -- If there's a handle - close it.
      case tfHandle currentTF of
        Nothing -> return ()
        Just h  -> hClose h

establishConnection :: TraceForwarderMVar -> IO ()
establishConnection tfMVar = withIOManager $ \iomgr -> do
  addr <- tfRemoteAddr <$> readMVar tfMVar
  try (connectForwarder iomgr addr) >>= \case
    Right h ->
      modifyMVar_ tfMVar $ \be -> return $ be { tfHandle = Just h }
    Left (_e :: IOException) -> do
      -- Cannot establish it, let's try again..
      threadDelay 1000000 -- 1 s
      establishConnection tfMVar

traceForwarderWriter :: ToJSON a => TraceForwarderMVar -> LogObject a -> IO ()
traceForwarderWriter tfMVar lo =
  tfHandle <$> readMVar tfMVar >>= \case
    Nothing ->
      -- There's no handle, connection wasn't established yet.
      return ()
    Just h ->
      try (BSC.hPutStrLn h $! encodeUtf8 (hostname . loMeta $ lo)) >>= \case
        Right _ ->
          -- Hostname was written to the handler successfully,
          -- try to write serialized LogObject.
          try (BSC.hPutStrLn h $! bs) >>= \case
            Right _ ->
              -- Everything is ok, LogObject was written to the handler.
              return ()
            Left (_e :: IOException) -> tryToReEstablishConnection
        Left (_e :: IOException) -> tryToReEstablishConnection
 where
  (_, bs) = jsonToBS lo

  jsonToBS :: ToJSON b => b -> (Int, BS.ByteString)
  jsonToBS a =
    let bs' = BL.toStrict $ encode a
    in (BS.length bs', bs')

  -- Handler is here, but it's broken, it looks like the connection with TraceAcceptor
  -- was interrupted, try to establish it again.
  tryToReEstablishConnection = do
    connectorThr <- Async.async $ establishConnection tfMVar
    modifyMVar_ tfMVar $ \be -> return $ be { tfHandle = Nothing, tfConnector = Just connectorThr } 

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
