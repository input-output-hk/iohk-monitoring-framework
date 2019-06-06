
\subsection{Cardano.BM.Backend.TraceAcceptor}
\label{module:Cardano.BM.Backend.TraceAcceptor}


%if style == newcode
\begin{code}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}


module Cardano.BM.Backend.TraceAcceptor
    ( TraceAcceptor (..)
    , effectuate
    , realizefrom
    , unrealize
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar,
                     withMVar)
import           Control.Exception (SomeException, catch)
import           Data.Aeson (FromJSON, decodeStrict)
import qualified Data.ByteString as BS
import           System.Directory (removeFile)
import           System.IO (IOMode (..), openFile)

import           System.Posix.Files (createNamedPipe, stdFileMode)

import           Cardano.BM.Data.Tracer (traceWith)
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.LogItem (LOContent (LogError),
                     PrivacyAnnotation (Public),mkLOMeta)
import           Cardano.BM.Data.Severity (Severity (..))
import qualified Cardano.BM.Trace as Trace

\end{code}
%endif
|TraceAcceptor| is a backend responsible for processing |LogObject|s of an
external process captured by a pipe or socket. At the time being it redirects
the |LogObject|s to the |SwitchBoard|.

\subsubsection{Structure of TraceAcceptor}\label{code:TraceAcceptor}\index{TraceAcceptor}
\begin{code}
newtype TraceAcceptor a = TraceAcceptor
    { getTA :: TraceAcceptorMVar a }

type TraceAcceptorMVar a = MVar (TraceAcceptorInternal a)

data TraceAcceptorInternal a = TraceAcceptorInternal
    { accPipe :: FilePath
    }

\end{code}

\subsubsection{TraceAcceptor is an effectuator}\index{TraceAcceptor!instance of IsEffectuator}
Must be an effectuator to be a Backend.
\begin{code}
instance IsEffectuator TraceAcceptor a where
    effectuate _ _   = return ()
    handleOverflow _ = return ()

\end{code}

\subsubsection{|TraceAcceptor| implements |Backend| functions}\index{TraceAcceptor!instance of IsBackend}
|TraceAcceptor| is an |IsBackend|
\begin{code}
instance FromJSON a => IsBackend TraceAcceptor a where
    typeof _ = TraceAcceptorBK

    realize _ = fail "TraceAcceptor cannot be instantiated by 'realize'"

    realizefrom _ sbtrace _ = do
        elref <- newEmptyMVar
        let externalLog = TraceAcceptor elref
            pipePath = "log-pipe"
        createNamedPipe pipePath stdFileMode
            `catch` (\(_ :: SomeException) -> pure ())
        dispatcher <- spawnDispatcher pipePath sbtrace
        -- link the given Async to the current thread, such that if the Async
        -- raises an exception, that exception will be re-thrown in the current
        -- thread, wrapped in ExceptionInLinkedThread.
        Async.link dispatcher
        putMVar elref $ TraceAcceptorInternal
                            { accPipe = pipePath
                            }
        return externalLog

    unrealize accView = withMVar (getTA accView) (\acc -> do
        let pipeName = accPipe acc
        -- Destroy the pipe
        removeFile pipeName
            `catch` (\(_ :: SomeException) -> pure ()))

\end{code}

\subsubsection{Reading log items from the pipe}
\begin{code}
spawnDispatcher :: FromJSON a
                => FilePath
                -> Trace.Trace IO a
                -> IO (Async.Async ())
spawnDispatcher pipe sbtrace = do
    -- Use of ReadWriteMode instead of ReadMode in order EOF not to
    -- be written at the end of file
    hPipe <- openFile pipe ReadWriteMode
    Async.async $ pProc hPipe
  where
    {-@ lazy pProc @-}
    pProc h = do
        bs <- BS.hGetLine h
        if not (BS.null bs)
        then do
            case decodeStrict bs of
                Just lo ->
                    traceWith sbtrace lo
                Nothing -> do
                    trace <- Trace.appendName "#external" sbtrace
                    Trace.traceNamedObject trace =<<
                        (,) <$> (mkLOMeta Warning Public)
                            <*> pure (LogError "Could not parse external log objects.")
            pProc h
        else return ()  -- stop here

\end{code}
