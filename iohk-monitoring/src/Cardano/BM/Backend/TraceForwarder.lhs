
\subsection{Cardano.BM.Backend.TraceForwarder}
\label{module:Cardano.BM.Backend.TraceForwarder}


%if style == newcode
\begin{code}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}


module Cardano.BM.Backend.TraceForwarder
    (
      TraceForwarder (..)
    , effectuate
    , realize
    , unrealize
    ) where

import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar,
                     withMVar)
import           Control.Exception (SomeException, catch)
import           Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BSC
import           Data.Maybe (fromMaybe)
import           System.IO (IOMode (..), openFile, BufferMode (NoBuffering),
                     Handle, hClose, hSetBuffering, openFile, stderr)

import           Cardano.BM.Configuration.Model (getLogOutput)
import           Cardano.BM.Data.Backend

\end{code}
%endif

\subsubsection{Structure of TraceForwarder}\label{code:TraceForwarder}\index{TraceForwarder}
\begin{code}
newtype TraceForwarder a = TraceForwarder
    { getTF :: TraceForwarderMVar a }

type TraceForwarderMVar a = MVar (TraceForwarderInternal a)

data TraceForwarderInternal a = TraceForwarderInternal
    { tfPipeHandler :: Handle
    }

\end{code}

\subsubsection{TraceForwarder is an effectuator}\index{TraceForwarder!instance of IsEffectuator}
\begin{code}
instance ToJSON a => IsEffectuator TraceForwarder a where
    effectuate tf lo  =
        withMVar (getTF tf) $ \(TraceForwarderInternal h) ->
            let (_, bs) = jsonToBS lo
            in
                BSC.hPutStrLn h $! bs
    handleOverflow _ = return ()

jsonToBS :: ToJSON a => a -> (Int, BS.ByteString)
jsonToBS a =
    let bs = BL.toStrict $ encode a
    in (BS.length bs, bs)

\end{code}


\subsubsection{|TraceForwarder| implements |Backend| functions}\index{TraceForwarder!instance of IsBackend}

|TraceForwarder| is an |IsBackend|
\begin{code}
instance (FromJSON a, ToJSON a) => IsBackend TraceForwarder a where
    typeof _ = TraceForwarderBK

    realize _cfg = do
        ltpref <- newEmptyMVar
        let logToPipe = TraceForwarder ltpref
        pipePath <- fromMaybe "log-pipe" <$> getLogOutput _cfg
        h <- openFile pipePath WriteMode
                `catch` (\(_ :: SomeException) -> pure stderr)
        hSetBuffering h NoBuffering
        putMVar ltpref $ TraceForwarderInternal
                            { tfPipeHandler = h
                            }
        return logToPipe

    realizefrom cfg _ _ = realize cfg

    unrealize tf = withMVar (getTF tf) (\(TraceForwarderInternal h) ->
        -- Close the handle of pipe
        hClose h
            `catch` (\(_ :: SomeException) -> pure ()))

\end{code}
