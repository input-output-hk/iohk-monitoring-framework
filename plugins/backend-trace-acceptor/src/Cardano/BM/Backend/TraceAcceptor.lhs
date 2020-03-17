
\subsection{Cardano.BM.Backend.TraceAcceptor}
\label{module:Cardano.BM.Backend.TraceAcceptor}


%if style == newcode
\begin{code}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -Wextra            #-}

#if !defined(mingw32_HOST_OS)
#define POSIX
#endif

module Cardano.BM.Backend.TraceAcceptor
    ( TraceAcceptor
    , plugin
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Exception
import           Control.Monad (forever, unless, when)
import           Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict)
import qualified Data.ByteString as BS
import           Data.Text (pack)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Typeable (Typeable)
import qualified Network.Socket as Socket
import qualified System.Directory as Dir
import           System.IO (Handle)
import qualified System.IO as IO

import           Cardano.BM.Configuration
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.BackendKind (BackendKind(TraceAcceptorBK))
import           Cardano.BM.Data.Configuration (RemoteAddr(..))
import           Cardano.BM.Data.LogItem
                   ( LOContent (LogError), LOMeta (..)
                   , PrivacyAnnotation (Public), loName, mkLOMeta
                   )
import           Cardano.BM.Data.Severity (Severity (..))
import           Cardano.BM.Data.Tracer (traceWith)
import           Cardano.BM.Plugin
import qualified Cardano.BM.Trace as Trace

\end{code}
%endif
|TraceAcceptor| is a backend responsible for processing |LogObject|s of an
external process captured by a pipe or socket. At the time being it redirects
the |LogObject|s to the |SwitchBoard|.

\subsubsection{Plugin definition}
\begin{code}
plugin :: forall s a
       . (IsEffectuator s a, ToJSON a, FromJSON a)
       => Configuration -> Trace.Trace IO a -> s a -> IO (Plugin a)
plugin cf trace _ = getAcceptAt cf >>= \case
  Nothing -> fail "TraceAcceptor not configured:  no traceAcceptAt option"
  Just addr -> do
    sock <- mkAcceptor addr
    server <- Async.async . forever $ acceptConnection trace sock
    Async.link server
    let be :: (Cardano.BM.Backend.TraceAcceptor.TraceAcceptor a)
        be = TraceAcceptor
             { taServer    = server
             , taShutdown  = Socket.close sock
             }
    IO.hPutStrLn IO.stderr $ "Activating trace acceptor on: " <> show addr
    return $ BackendPlugin
               (MkBackend { bEffectuate = effectuate be
                          , bUnrealize = unrealize be })
               (bekind be)

\end{code}

\subsubsection{Structure of TraceAcceptor}\label{code:TraceAcceptor}\index{TraceAcceptor}
\begin{code}
data TraceAcceptor a = TraceAcceptor
    { taServer   :: Async.Async ()
    , taShutdown :: IO ()
    }

instance IsEffectuator TraceAcceptor a where
    effectuate _ta _item = pure ()
    handleOverflow _ta = pure ()

instance (ToJSON a, FromJSON a) => IsBackend TraceAcceptor a where
    type BackendFailure TraceAcceptor = TraceAcceptorBackendFailure

    bekind _ = TraceAcceptorBK

    realize _ = fail "TraceAcceptor cannot be instantiated by 'realize'"

    realizefrom _ _ _ = fail "TraceAcceptor cannot be instantiated by 'realizefrom'"

    unrealize ta = do
      Async.cancel $ taServer ta
      taShutdown ta

handleError :: (String -> BackendFailure TraceAcceptor) -> IO a -> IO a
handleError ctor = handle $ \(e :: IOException) -> throwIO . ctor . show $ e

data TraceAcceptorBackendFailure
  = TraceAcceptorPipeError String
  | TraceAcceptorSocketError String
  | TraceAcceptorServerError String
  | TraceAcceptorClientThreadError String
  deriving (Show, Typeable)

instance Exception TraceAcceptorBackendFailure

mkAcceptor :: RemoteAddr -> IO Socket.Socket
mkAcceptor (RemotePipe pipePath) = handleError TraceAcceptorPipeError $ mdo
  sock <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
  exists <- Dir.doesFileExist pipePath
  when exists $
    Dir.removeFile pipePath
  bindSocket sock (Socket.SockAddrUnix pipePath)
  Socket.listen sock 1
  pure sock

mkAcceptor (RemoteSocket host port) = handleError TraceAcceptorSocketError $ mdo
  addrs <- Socket.getAddrInfo Nothing (Just host) (Just port)
  addr :: Socket.AddrInfo <- case addrs of
    [] -> throwIO (TraceAcceptorSocketError ("bad socket address: " <> host <> ":" <> port))
    a:_ -> pure a
  sock <- Socket.socket (Socket.addrFamily addr) (Socket.addrSocketType addr) (Socket.addrProtocol addr)
  bindSocket sock (Socket.addrAddress addr)
  Socket.listen sock 7
  pure sock

acceptConnection :: FromJSON a => Trace.Trace IO a -> Socket.Socket -> IO ()
acceptConnection baseTrace  sock = handleError TraceAcceptorServerError $ do
  (client, _sa) <- Socket.accept sock
  h <- Socket.socketToHandle client IO.ReadWriteMode
  _client <- Async.async $ clientThread baseTrace h
  pure ()

bindSocket :: Socket.Socket -> Socket.SockAddr -> IO ()
bindSocket sd addr = do
  let fml = socketAddrFamily addr
  when (fml == Socket.AF_INET ||
        fml == Socket.AF_UNIX ||
        fml == Socket.AF_INET6) $ do
    Socket.setSocketOption sd Socket.ReuseAddr 1
#if defined(POSIX)
        -- only supported on POSIX, not Windows
    Socket.setSocketOption sd Socket.ReusePort 1
#endif
  when (fml == Socket.AF_INET6)
    -- An AF_INET6 socket can be used to talk to both IPv4 and IPv6 end points, and
    -- it is enabled by default on some systems. Disabled here since we run a separate
    -- IPv4 server instance if configured to use IPv4.
    $ Socket.setSocketOption sd Socket.IPv6Only 1
  Socket.bind sd addr
 where
   socketAddrFamily
     :: Socket.SockAddr
     -> Socket.Family
   socketAddrFamily Socket.SockAddrInet{}  = Socket.AF_INET
   socketAddrFamily Socket.SockAddrInet6{} = Socket.AF_INET6
   socketAddrFamily Socket.SockAddrUnix{}  = Socket.AF_UNIX

\end{code}

\subsubsection{Reading log items from the client}
\begin{code}
clientThread
  :: forall a. (FromJSON a)
  => Trace.Trace IO a
  -> Handle
  -> IO ()
clientThread sbtrace h = handleError TraceAcceptorClientThreadError pProc
  where
    {-@ lazy pProc @-}
    pProc :: IO ()
    pProc = do
      hn <- BS.hGetLine h -- hostname
      bs <- BS.hGetLine h -- payload
      unless (BS.null bs) $ do
        let hname = decodeUtf8 hn
        case eitherDecodeStrict bs of
          Right lo ->
            traceWith sbtrace (loName lo, lo)
          Left e -> do
              lometa0 <- mkLOMeta Warning Public
              let trace :: Trace.Trace IO a
                  trace = Trace.appendName "#external" sbtrace
                  lometa = lometa0 { hostname = hname }
              Trace.traceNamedObject trace =<<
                  (,) <$> pure lometa
                      <*> pure (LogError $ "Could not parse external log objects: " <> pack e)
        pProc

\end{code}
