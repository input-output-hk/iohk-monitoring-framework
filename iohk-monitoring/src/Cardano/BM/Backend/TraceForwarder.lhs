
\subsection{Cardano.BM.Backend.TraceForwarder}
\label{module:Cardano.BM.Backend.TraceForwarder}


%if style == newcode
\begin{code}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}


module Cardano.BM.Backend.TraceForwarder
    ( TraceForwarder (..)
    , effectuate
    , realizefrom
    , unrealize
    ) where

import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar,
                     withMVar)
import           Control.Exception (SomeException, catch)
import           Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe (fromMaybe)

import           Cardano.BM.Backend.ExternalAbstraction (Pipe (..))
import           Cardano.BM.Configuration.Model (getLogOutput)
import           Cardano.BM.Data.Backend

\end{code}
%endif

|TraceForwarder| is a new backend responsible for redirecting the logs into a pipe
or a socket to be used from another application. It puts |LogObject|s as
|ByteString|s in the provided handler.

\subsubsection{Structure of TraceForwarder}\label{code:TraceForwarder}\index{TraceForwarder}
Contains the handler to the pipe or to the socket.
\begin{code}
newtype TraceForwarder p a = TraceForwarder
    { getTF :: TraceForwarderMVar p a }

type TraceForwarderMVar p a = MVar (TraceForwarderInternal p a)

data Pipe p => TraceForwarderInternal p a =
    TraceForwarderInternal
        { tfPipeHandler :: PipeHandler p
        }

\end{code}

\subsubsection{TraceForwarder is an effectuator}\index{TraceForwarder!instance of IsEffectuator}
Every |LogObject| before being written to the given handler is converted to
|ByteString| through its |JSON| represantation.
\begin{code}
instance (Pipe p, ToJSON a) => IsEffectuator (TraceForwarder p) a where
    effectuate tf lo  =
        withMVar (getTF tf) $ \(TraceForwarderInternal h) ->
            let (_, bs) = jsonToBS lo
            in
                write h bs
    handleOverflow _ = return ()

jsonToBS :: ToJSON a => a -> (Int, BS.ByteString)
jsonToBS a =
    let bs = BL.toStrict $ encode a
    in (BS.length bs, bs)

\end{code}


\subsubsection{|TraceForwarder| implements |Backend| functions}\index{TraceForwarder!instance of IsBackend}

|TraceForwarder| is an |IsBackend|
\begin{code}
instance (Pipe p, FromJSON a, ToJSON a) => IsBackend (TraceForwarder p) a where
    typeof _ = TraceForwarderBK

    realize _ = fail "ExternalLog cannot be instantiated by 'realize'"

    realizefrom cfg sbtrace _ = do
        ltpref <- newEmptyMVar
        let logToPipe = TraceForwarder ltpref
        pipePath <- fromMaybe "log-pipe" <$> getLogOutput cfg
        h <- open pipePath sbtrace
        putMVar ltpref $ TraceForwarderInternal
                            { tfPipeHandler = h
                            }
        return logToPipe

    unrealize tf = withMVar (getTF tf) (\(TraceForwarderInternal h) ->
        -- close the pipe
        close h
            `catch` (\(_ :: SomeException) -> pure ()))

\end{code}
