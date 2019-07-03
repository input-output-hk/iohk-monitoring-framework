
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
import           Data.Aeson (FromJSON, decodeStrict)
import qualified Data.ByteString as BS

import qualified Cardano.BM.Backend.ExternalAbstraction as CH
import           Cardano.BM.Backend.ExternalAbstraction (Pipe (..))
import           Cardano.BM.Data.Tracer (traceWith)
import           Cardano.BM.Data.LogItem (LOContent (LogError),
                     LogObject (..), PrivacyAnnotation (Public),mkLOMeta)
import           Cardano.BM.Data.Severity (Severity (..))
import qualified Cardano.BM.Trace as Trace

\end{code}
%endif
|TraceAcceptor| is a backend responsible for processing |LogObject|s of an
external process captured by a pipe or socket. At the time being it redirects
the |LogObject|s to the |SwitchBoard|.

\subsubsection{Structure of TraceAcceptor}\label{code:TraceAcceptor}\index{TraceAcceptor}
\begin{code}
newtype TraceAcceptor p a = TraceAcceptor
    { getTA :: TraceAcceptorMVar p a }

type TraceAcceptorMVar p a = MVar (TraceAcceptorInternal p a)

data TraceAcceptorInternal p a = TraceAcceptorInternal
    { accPipe     :: PipeHandler p
    , accDispatch :: Async.Async ()
    }

effectuate :: LogObject a -> IO ()
effectuate = \_ -> return ()

realizefrom :: (Pipe p, FromJSON a)
            => Trace.Trace IO a -> FilePath -> IO (TraceAcceptor p a)
realizefrom sbtrace pipePath = do
    elref <- newEmptyMVar
    let externalLog = TraceAcceptor elref
    h <- create pipePath
    dispatcher <- spawnDispatcher h sbtrace
    -- link the given Async to the current thread, such that if the Async
    -- raises an exception, that exception will be re-thrown in the current
    -- thread, wrapped in ExceptionInLinkedThread.
    Async.link dispatcher
    putMVar elref $ TraceAcceptorInternal
                        { accPipe = h
                        , accDispatch = dispatcher
                        }
    return externalLog

unrealize :: Pipe p => TraceAcceptor p a -> IO ()
unrealize accView = withMVar (getTA accView) (\acc -> do
    Async.cancel $ accDispatch acc
    let hPipe = accPipe acc
    -- close the pipe
    close hPipe)

\end{code}

\subsubsection{Reading log items from the pipe}
\begin{code}
spawnDispatcher :: (Pipe p, FromJSON a)
                => PipeHandler p
                -> Trace.Trace IO a
                -> IO (Async.Async ())
spawnDispatcher hPipe sbtrace = do
    Async.async $ pProc hPipe
  where
    {-@ lazy pProc @-}
    pProc h = do
        bs <- CH.getLine h
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
