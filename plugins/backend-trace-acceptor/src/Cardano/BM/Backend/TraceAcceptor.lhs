
\subsection{Cardano.BM.Backend.TraceAcceptor}
\label{module:Cardano.BM.Backend.TraceAcceptor}


%if style == newcode
\begin{code}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

#if !defined(mingw32_HOST_OS)
#define POSIX
#endif

module Cardano.BM.Backend.TraceAcceptor
    ( TraceAcceptor (..)
    -- * Plugin
    , plugin
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar,
                     withMVar)
import           Data.Aeson (FromJSON, ToJSON, decodeStrict)
import qualified Data.ByteString as BS
import           Data.Text.Encoding (decodeUtf8)

import qualified Cardano.BM.Backend.ExternalAbstraction as CH
import           Cardano.BM.Backend.ExternalAbstraction (Pipe (..))
import           Cardano.BM.Configuration (Configuration)
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.BackendKind (BackendKind(TraceAcceptorBK))
import           Cardano.BM.Data.LogItem (LOContent (LogError), LOMeta (..),
                     PrivacyAnnotation (Public), mkLOMeta)
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
       => Configuration -> Trace.Trace IO a -> s a -> FilePath -> IO (Plugin a)
plugin _ trace _ fp = do
    be :: (Cardano.BM.Backend.TraceAcceptor.TraceAcceptor PipeType a) <- realizepipe trace fp
    return $ BackendPlugin
               (MkBackend { bEffectuate = effectuate be
                          , bUnrealize = unrealize be })
               (bekind be)

type PipeType =
#ifdef POSIX
    CH.UnixNamedPipe
#else
    CH.NoPipe
#endif


\end{code}

\subsubsection{Structure of TraceAcceptor}\label{code:TraceAcceptor}\index{TraceAcceptor}
\begin{code}
newtype TraceAcceptor p a = TraceAcceptor
    { getTA :: TraceAcceptorMVar p a }

type TraceAcceptorMVar p a = MVar (TraceAcceptorInternal p a)

data TraceAcceptorInternal p a = TraceAcceptorInternal
    { accPipe     :: PipeHandler p
    , accDispatch :: Async.Async ()
    }

instance IsEffectuator (TraceAcceptor p) a where
    effectuate _ta _item = pure ()
    handleOverflow _ta = pure ()

instance (Pipe p, ToJSON a, FromJSON a) => IsBackend (TraceAcceptor p) a where
    bekind _ = TraceAcceptorBK

    realize _ = fail "TraceAcceptor cannot be instantiated by 'realize'"

    realizefrom _ _ _ = fail "TraceAcceptor cannot be instantiated by 'realizefrom'"

    unrealize accView = withMVar (getTA accView) $
        \acc -> do
            Async.cancel $ accDispatch acc
            close (accPipe acc)

realizepipe :: (Pipe p, FromJSON a)
            => Trace.Trace IO a -> FilePath -> IO (TraceAcceptor p a)
realizepipe sbtrace pipePath = do
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

\end{code}

\subsubsection{Reading log items from the pipe}
\begin{code}
spawnDispatcher :: (Pipe p, FromJSON a)
                => PipeHandler p
                -> Trace.Trace IO a
                -> IO (Async.Async ())
spawnDispatcher hPipe sbtrace =
    Async.async $ pProc hPipe
  where
    {-@ lazy pProc @-}
    pProc h = do
        hn <- CH.getLine h -- hostname
        bs <- CH.getLine h -- payload
        if not (BS.null bs)
        then do
            let hname = decodeUtf8 hn
            case decodeStrict bs of
                Just lo ->
                    traceWith sbtrace lo
                Nothing -> do
                    lometa0 <- mkLOMeta Warning Public
                    let trace = Trace.appendName "#external" sbtrace
                        lometa = lometa0 { hostname = hname }
                    Trace.traceNamedObject trace =<<
                        (,) <$> pure lometa
                            <*> pure (LogError "Could not parse external log objects.")
            pProc h
        else return ()  -- stop here

\end{code}
