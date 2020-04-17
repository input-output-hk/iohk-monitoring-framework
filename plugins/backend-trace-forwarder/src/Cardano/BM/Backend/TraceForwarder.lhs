
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

Note that if the connection to the trace acceptor is broken,
the application is terminated. (TODO)

The |TraceForwarder| is looking up a minimum |Severity| in the options
section of the configuration. This filters out all messages that have not at
least the |Severity|.
\subsubsection{Plugin definition}
\begin{code}
plugin :: forall a s . (IsEffectuator s a, ToJSON a, FromJSON a)
       => Configuration -> Trace.Trace IO a -> s a -> Text -> IO (Plugin a)
plugin config _trace _sb tfid = do
    be0 :: Cardano.BM.Backend.TraceForwarder.TraceForwarder a <- realize config
    opts <- getTextOption config tfid
    let minsev = case opts of
                   Nothing -> Debug
                   Just sevtext -> fromMaybe Debug (readMaybe $ unpack sevtext)
    let be = be0 { tfFilter = minsev }
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
  , tfFilter  :: Severity
  }

\end{code}

\subsubsection{TraceForwarder is an effectuator}\index{TraceForwarder!instance of IsEffectuator}
Every |LogObject| before being written to the given handler is converted to
|ByteString| through its |JSON| represantation.
\begin{code}
instance (ToJSON a) => IsEffectuator TraceForwarder a where
    effectuate tf lo =
        when (severity meta >= tfFilter tf) $ do
          tfWrite tf $ encodeUtf8 (hostname meta)
          tfWrite tf bs
      where
        meta = loMeta lo
        (_, bs) = jsonToBS lo

        jsonToBS :: ToJSON b => b -> (Int, BS.ByteString)
        jsonToBS a =
          let bs' = BL.toStrict $ encode a
          in (BS.length bs', bs')

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
      Just addr -> handleError TraceForwarderConnectionError $
        withIOManager $ \iomgr -> do
          h <- connectForwarder iomgr addr
          pure TraceForwarder
            { tfClose = hClose h
            , tfWrite = \bs -> BSC.hPutStrLn h $! bs
            , tfFilter = Debug  -- default
            }

    unrealize = tfClose

handleError :: (String -> BackendFailure TraceForwarder) -> IO a -> IO a
handleError ctor = flip catch $ \(e :: IOException) -> throwIO . ctor . show $ e

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
