
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
import           Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe (fromMaybe)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Typeable (Typeable)

import           GHC.IO.Handle (hDuplicate)
import qualified Network.Socket as Socket
import           System.IO (IOMode (..), openFile, BufferMode (NoBuffering),
                     Handle, hClose, hSetBuffering, openFile, stderr,
                     hPutStrLn)
import           System.IO.Error (mkIOError, doesNotExistErrorType,
                     userErrorType)
import           System.PosixCompat.Files (fileExist, getFileStatus,
                     isNamedPipe, stdFileMode)

import           Cardano.BM.Configuration
import           Cardano.BM.Configuration.Model
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.Configuration (RemoteAddr(..))
import           Cardano.BM.Data.LogItem (LOMeta (..), LogObject (..))
import           Cardano.BM.Plugin
import qualified Cardano.BM.Trace as Trace

\end{code}
%endif

|TraceForwarder| is a backend responsible for redirecting logs to a
different process (running a |TraceAcceptor| backend), by means of
either a pipe or a socket.

Note that if the connection to the trace acceptor is broken,
the application is terminated.

\subsubsection{Plugin definition}
\begin{code}
plugin :: (IsEffectuator s a, ToJSON a, FromJSON a)
       => Configuration -> Trace.Trace IO a -> s a -> IO (Plugin a)
plugin config _trace _sb = do
    be :: Cardano.BM.Backend.TraceForwarder.TraceForwarder a <- realize config
    return $ BackendPlugin
               (MkBackend { bEffectuate = effectuate be, bUnrealize = unrealize be })
               (bekind be)

\end{code}

\subsubsection{Structure of TraceForwarder}\label{code:TraceForwarder}\index{TraceForwarder}
Contains the handler to the pipe or to the socket.
\begin{code}
data TraceForwarder a = TraceForwarder
  { tfWrite   :: BSC.ByteString -> IO ()
  , tfClose   :: IO ()
  }

\end{code}

\subsubsection{TraceForwarder is an effectuator}\index{TraceForwarder!instance of IsEffectuator}
Every |LogObject| before being written to the given handler is converted to
|ByteString| through its |JSON| represantation.
\begin{code}
instance (ToJSON a) => IsEffectuator TraceForwarder a where
    effectuate tf lo = do
        tfWrite tf $ encodeUtf8 (hostname $ loMeta lo)
        tfWrite tf bs
      where
        (_, bs) = jsonToBS lo

        jsonToBS :: ToJSON b => b -> (Int, BS.ByteString)
        jsonToBS a =
          let bs = BL.toStrict $ encode a
          in (BS.length bs, bs)

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
      Just addr -> handleError TraceForwarderConnectionError $ do
        sock <- connectForwarder addr
        handle <- Socket.socketToHandle sock ReadWriteMode
        pure TraceForwarder
          { tfClose = Socket.close sock
          , tfWrite = \bs -> BSC.hPutStrLn handle $! bs
          }

    unrealize tf = tfClose tf

handleError :: (String -> BackendFailure TraceForwarder) -> IO a -> IO a
handleError ctor = flip catch $ \(e :: IOException) -> throwIO . ctor . show $ e

connectForwarder :: RemoteAddr -> IO Socket.Socket
connectForwarder (RemotePipe pipePath) = do
  sock <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
  Socket.connect sock (Socket.SockAddrUnix pipePath)
  pure sock
connectForwarder (RemoteSocket host port) = do
  addrs <- Socket.getAddrInfo Nothing (Just host) (Just port)
  addr :: Socket.AddrInfo <- case addrs of
    [] -> throwIO (TraceForwarderSocketError ("bad socket address: " <> host <> ":" <> port))
    a:_ -> pure a
  sock <- Socket.socket (Socket.addrFamily addr) (Socket.addrSocketType addr) (Socket.addrProtocol addr)
  Socket.connect sock (Socket.addrAddress addr)
  pure sock

data TraceForwarderBackendFailure
  = TraceForwarderConnectionError String
  | TraceForwarderSocketError String
  deriving (Show, Typeable)

instance Exception TraceForwarderBackendFailure

\end{code}
