
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
import           Control.Monad (forM, unless)
import           Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict)
import qualified Data.ByteString as BS
import           Data.Text (pack)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Typeable (Typeable)
import qualified Network.Socket as Socket
import           System.IO (Handle)
import qualified System.IO as IO

import           Cardano.BM.Configuration
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.BackendKind (BackendKind(TraceAcceptorBK))
import           Cardano.BM.Data.Configuration (RemoteAddr(..), RemoteAddrNamed(..))
import           Cardano.BM.Data.LogItem
                   ( LOContent (LogError), LOMeta (..)
                   , PrivacyAnnotation (Public), loName, mkLOMeta
                   )
import           Cardano.BM.Data.Severity (Severity (..))
import           Cardano.BM.Data.Tracer (traceWith)
import           Cardano.BM.IOManager
import           Cardano.BM.Plugin
import qualified Cardano.BM.Snocket as Snocket
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
       => IOManager -> Configuration -> Trace.Trace IO a -> s a -> IO (Plugin a)
plugin iomgr cf basicTrace _ = getAcceptAt cf >>= \case
  Just acceptors -> do
    socketsNServers <- forM acceptors $ \(RemoteAddrNamed nodeName addr) -> do
      let trace = Trace.appendName nodeName basicTrace
      (serverCleanup, serverThr) <- acceptorForAddress trace iomgr addr
      Async.link serverThr
      -- IO.hPutStrLn IO.stderr $ "Activating trace acceptor on: " <> show addr
      return (serverCleanup, serverThr)

    let (cleanups, servers) = unzip socketsNServers
        be :: (Cardano.BM.Backend.TraceAcceptor.TraceAcceptor a)
        be = TraceAcceptor
             { taServers   = servers
             , taShutdown  = sequence_ cleanups
             }
    return $ BackendPlugin
               (MkBackend { bEffectuate = effectuate be
                          , bUnrealize = unrealize be })
               (bekind be)
  Nothing -> fail "TraceAcceptor not configured: no traceAcceptAt option"

\end{code}

\subsubsection{Structure of TraceAcceptor}\label{code:TraceAcceptor}\index{TraceAcceptor}
\begin{code}
data TraceAcceptor a = TraceAcceptor
    { taServers  :: [Async.Async ()]
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
      mapM_ Async.cancel $ taServers ta
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

acceptorForAddress
  :: FromJSON a
  => Trace.Trace IO a
  -> IOManager
  -> RemoteAddr
  -> IO (IO (), Async.Async ())
acceptorForAddress trace iomgr (RemotePipe pipePath) =
  handleError TraceAcceptorPipeError $
  acceptorForSnocket
    trace
    Snocket.localFDToHandle
    (Snocket.localSnocket iomgr pipePath)
    (Snocket.localAddressFromPath pipePath)

acceptorForAddress trace iomgr (RemoteSocket host port) = handleError TraceAcceptorSocketError $ do
  let sn = Snocket.socketSnocket iomgr
  ainfos <- Socket.getAddrInfo Nothing (Just host) (Just port)
  case ainfos of
    [] -> throwIO (TraceAcceptorSocketError ("bad socket address: " <> host <> ":" <> port))
    a:_ -> acceptorForSnocket
             trace
             (flip Socket.socketToHandle IO.ReadWriteMode)
             sn
             (Socket.addrAddress a)

acceptorForSnocket
  :: forall a fd addr. (FromJSON a)
  => Trace.Trace IO a
  -> (fd -> IO Handle)
  -> Snocket.Snocket IO fd addr
  -> addr
  -> IO (IO (), Async.Async ())
acceptorForSnocket trace toHandle sn addr = do
  sock <- Snocket.mkListeningSocket sn (Just addr) (Snocket.addrFamily sn addr)
  server <- Async.async $
    bracket (pure sock) (Snocket.close sn) $
      \sock -> acceptLoop $ Snocket.accept sn sock
  pure (Snocket.close sn sock, server)
 where
   acceptLoop :: Snocket.Accept addr fd -> IO ()
   acceptLoop (Snocket.Accept accept) = do
      (cfd, _caddr, k) <- accept
      h <- toHandle cfd
      _client <- Async.async $ clientThread trace h
      acceptLoop k

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
