
\subsection{Cardano.BM.Backend.TraceForwarder}
\label{module:Cardano.BM.Backend.TraceForwarder}


%if style == newcode
\begin{code}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
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

import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar,
                     withMVar)
import           Control.Exception (IOException, Exception, SomeException, catch, throwIO)
import           Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe (fromMaybe)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Typeable (Typeable)

import qualified Cardano.BM.Backend.ExternalAbstraction as CH
import           Cardano.BM.Backend.ExternalAbstraction (Pipe (..))
import           Cardano.BM.Configuration (Configuration)
import           Cardano.BM.Configuration.Model
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.LogItem (LOMeta (..), LogObject (..))
import           Cardano.BM.Plugin
import qualified Cardano.BM.Trace as Trace

\end{code}
%endif

|TraceForwarder| is a new backend responsible for redirecting the logs into a pipe
or a socket to be used from another application. It puts |LogObject|s as
|ByteString|s in the provided handler.

\subsubsection{Plugin definition}
\begin{code}
plugin :: (IsEffectuator s a, ToJSON a, FromJSON a)
       => Configuration -> Trace.Trace IO a -> s a -> IO (Plugin a)
plugin config _trace _sb = do
    be :: Cardano.BM.Backend.TraceForwarder.TraceForwarder PipeType a <- realize config
    return $ BackendPlugin
               (MkBackend { bEffectuate = effectuate be, bUnrealize = unrealize be })
               (bekind be)

type PipeType =
#ifdef POSIX
    CH.UnixNamedPipe
#else
    CH.NoPipe
#endif

\end{code}

\subsubsection{Structure of TraceForwarder}\label{code:TraceForwarder}\index{TraceForwarder}
Contains the handler to the pipe or to the socket.
\begin{code}
newtype TraceForwarder p a = TraceForwarder
    { getTF :: TraceForwarderMVar p a }

type TraceForwarderMVar p a = MVar (TraceForwarderInternal p a)

newtype Pipe p => TraceForwarderInternal p a =
    TraceForwarderInternal
        { tfPipeHandler :: PipeHandler p
        }

\end{code}

\subsubsection{TraceForwarder is an effectuator}\index{TraceForwarder!instance of IsEffectuator}
Every |LogObject| before being written to the given handler is converted to
|ByteString| through its |JSON| represantation.
\begin{code}
instance (Pipe p, ToJSON a) => IsEffectuator (TraceForwarder p) a where
    effectuate tf lo =
        withMVar (getTF tf) $ \(TraceForwarderInternal hdl) ->
            let (_, bs) = jsonToBS lo
                hn = hostname $ loMeta lo
            in do
                write hdl $ encodeUtf8 hn
                write hdl bs
    handleOverflow _ = return ()

jsonToBS :: ToJSON a => a -> (Int, BS.ByteString)
jsonToBS a =
    let bs = BL.toStrict $ encode a
    in (BS.length bs, bs)

\end{code}


\subsubsection{|TraceForwarder| implements |Backend| functions}\index{TraceForwarder!instance of IsBackend}

|TraceForwarder| is an |IsBackend|
\begin{code}
instance (Pipe p, FromJSON a, ToJSON a, Typeable p) => IsBackend (TraceForwarder p) a where
    type BackendFailure (TraceForwarder p) = TraceForwarderBackendFailure

    bekind _ = TraceForwarderBK

    realize cfg = do
        ltpref <- newEmptyMVar
        let logToPipe = TraceForwarder ltpref
        pipePath <- fromMaybe "log-pipe" <$> getLogOutput cfg
        h <- open pipePath
          `catch` (\(e :: IOException) ->
                     throwIO
                     . (TraceForwarderPipeError
                        :: String -> BackendFailure (TraceForwarder p))
                     . show $ e)
        putMVar ltpref $ TraceForwarderInternal
                            { tfPipeHandler = h
                            }
        return logToPipe

    unrealize tf = withMVar (getTF tf) (\(TraceForwarderInternal h) -> close h)

newtype TraceForwarderBackendFailure
  = TraceForwarderPipeError String
  deriving (Show, Typeable)

instance Exception TraceForwarderBackendFailure

\end{code}
