
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

#if !defined(mingw32_HOST_OS)
#define POSIX
#endif

module Cardano.BM.Backend.TraceAcceptor
    ( TraceAcceptor
    , plugin
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (readMVar)
import           Control.Exception
import           Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, decodeStrict)
import qualified Data.ByteString as BS
import           Data.Text (Text, pack)
import           Data.Text.Encoding (decodeUtf8)
import qualified Network.Socket as Socket
-- import           System.IO (Handle, IOMode(..), hClose, openFile)
import qualified System.IO as IO
import qualified System.IO.Error as IO
#ifdef POSIX
import           System.Posix.Files (createNamedPipe)
#endif
import           System.PosixCompat.Files
                   ( fileExist
                   , isNamedPipe
                   , getFileStatus
                   , stdFileMode)

import           Cardano.BM.Configuration
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.BackendKind (BackendKind(TraceAcceptorBK))
import           Cardano.BM.Data.Configuration (RemoteAddr(..))
import           Cardano.BM.Data.LogItem
                   ( LOContent (LogError), LoggerName, LogObject, LOMeta (..)
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
plugin :: (IsEffectuator s a, ToJSON a, FromJSON a)
       => Configuration -> Trace.Trace IO a -> s a -> IO (Plugin a)
plugin cf trace _ = getAcceptAt cf >>= \case
  Nothing -> fail "TraceAcceptor not configured:  no traceAcceptAt option"
  Just addr -> do
    be :: (Cardano.BM.Backend.TraceAcceptor.TraceAcceptor a) <- realizeAcceptor trace addr
    return $ BackendPlugin
               (MkBackend { bEffectuate = effectuate be
                          , bUnrealize = unrealize be })
               (bekind be)

\end{code}

\subsubsection{Structure of TraceAcceptor}\label{code:TraceAcceptor}\index{TraceAcceptor}
\begin{code}
data TraceAcceptor a = TraceAcceptor
    { taDispatch :: Async.Async ()
    , taClose    :: IO ()
    , taGetLine  :: IO BS.ByteString
    }

instance IsEffectuator TraceAcceptor a where
    effectuate _ta _item = pure ()
    handleOverflow _ta = pure ()

instance (ToJSON a, FromJSON a) => IsBackend TraceAcceptor a where
    bekind _ = TraceAcceptorBK

    realize _ = fail "TraceAcceptor cannot be instantiated by 'realize'"

    realizefrom _ _ _ = fail "TraceAcceptor cannot be instantiated by 'realizefrom'"

    unrealize ta = do
      Async.cancel $ taDispatch ta
      taClose ta

realizeAcceptor :: (FromJSON a)
            => Trace.Trace IO a -> RemoteAddr -> IO (TraceAcceptor a)
realizeAcceptor baseTrace ra@(RemotePipe pipePath) = mdo
    h <- create pipePath
    dispatcher <- spawnDispatcher ta baseTrace
    -- link the given Async to the current thread, such that if the Async
    -- raises an exception, that exception will be re-thrown in the current
    -- thread, wrapped in ExceptionInLinkedThread.
    Async.link dispatcher
    let ta = TraceAcceptor
             { taDispatch    = dispatcher
             , taGetLine     = BS.hGetLine h
             , taClose       = IO.hClose h
             }
    IO.hPutStrLn IO.stderr $ "Activating trace acceptor on: " <> show ra
    pure ta
 where
   create :: FilePath -> IO IO.Handle
   create pipePath = do
       -- use of ReadWriteMode instead of ReadMode in order
       -- EOF not to be written at the end of file
       let openPipe   = IO.openFile pipePath IO.ReadWriteMode

       createNamedPipe pipePath stdFileMode
         `catch` \(_ :: IOException) -> pure ()

       let retryPipeCreation :: FilePath -> IO IO.Handle
           retryPipeCreation pipePath' = do
               exists  <- fileExist pipePath'
               fstatus <- getFileStatus pipePath'

               if exists && isNamedPipe fstatus
                   then openPipe
                   else throwIO $ IO.mkIOError IO.userErrorType "cannot open pipe; it's not a pipe" Nothing (Just pipePath')

       -- We open up a pipe and catch @IOException@ __only__.
       -- The current things we need to watch out for when opening a pipe are:
       --     - if the file is already open and cannot be reopened
       --     - if the file does not exist
       --     - if the user does not have permission to open the file.
       openPipe `catch` \(_ :: IOException) -> retryPipeCreation pipePath

realizeAcceptor sbtrace (RemoteSocket addr serv) = do
    undefined

\end{code}

\subsubsection{Reading log items from the pipe}
\begin{code}
spawnDispatcher
  :: forall a b. (FromJSON a)
  => TraceAcceptor b
  -> Trace.Trace IO a
  -> IO (Async.Async ())
spawnDispatcher ta sbtrace =
    Async.async pProc
  where
    {-@ lazy pProc @-}
    pProc :: IO ()
    pProc = do
      hn <- taGetLine ta -- hostname
      bs <- taGetLine ta -- payload
      if not (BS.null bs)
      then do
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
      else return ()  -- stop here

\end{code}
