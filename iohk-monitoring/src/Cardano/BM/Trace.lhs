
\subsection{Cardano.BM.Trace}
\label{code:Cardano.BM.Trace}

%if style == newcode
\begin{code}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.BM.Trace
    (
      Trace
    , stdoutTrace
    , nullTracer
    , traceInTVar
    , traceInTVarIO
    -- * context naming
    , appendName
    , modifyName
    -- * utils
    , natTrace
    -- * log functions
    , traceLogObject
    , traceNamedObject
    , traceNamedItem
    , logAlert,     logAlertS
    , logCritical,  logCriticalS
    , logDebug,     logDebugS
    , logEmergency, logEmergencyS
    , logError,     logErrorS
    , logInfo,      logInfoS
    , logNotice,    logNoticeS
    , logWarning,   logWarningS
    ) where

import           Control.Concurrent.MVar (MVar, newMVar, withMVar)
import qualified Control.Concurrent.STM.TVar as STM
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.STM as STM
import           Data.Aeson.Text (encodeToLazyText)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Text.Lazy (toStrict)
import           System.IO.Unsafe (unsafePerformIO)

import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.Trace
import           Cardano.BM.Data.Tracer (Tracer (..), natTracer,
                     nullTracer, traceWith)

\end{code}
%endif

\subsubsection{Utilities}
Natural transformation from monad |m| to monad |n|.
\begin{code}
natTrace
  :: (forall x . m x -> n x)
  -> Trace m a -> Trace n a
natTrace f t@Trace{traceTracer} = t { traceTracer = natTracer f traceTracer }

\end{code}

\subsubsection{Enter new named context}\label{code:appendName}\index{appendName}
A new context name is added.
\begin{code}
appendName :: T.Text -> Trace m a -> Trace m a
appendName name = modifyName $ \loggerName ->
  if loggerNameText loggerName == ""
  then unitLoggerName name
  else loggerName @:> name

\end{code}

\subsubsection{Change named context}\label{code:modifyName}\index{modifyName}
The context name is overwritten.
\begin{code}
modifyName
    :: (LoggerName -> LoggerName)
    -> Trace m a
    -> Trace m a
modifyName f = mapStatic $ \ts@TraceStatic{loggerName} ->
                             ts { loggerName = f loggerName }

\end{code}

\subsubsection{Trace constituents of |LogObject| through}
\label{code:traceNamedObject}\index{traceNamedObject}
\begin{code}
-- TODO:  the name doesn't reflect actual meaning.
traceNamedObject
    :: MonadIO m
    => Trace m a
    -- TODO:  no need to have this in uncurried form
    -> (LOMeta, LOContent a)
    -> m ()
traceNamedObject Trace{traceStatic = st@TraceStatic {loggerName}, traceTracer}
                 (meta, loc) =
    traceWith traceTracer (st, LogObject loggerName meta loc)

\end{code}

\subsubsection{Enter message into a trace}\label{code:traceNamedItem}\index{traceNamedItem}
The function |traceNamedItem| creates a |LogObject| and threads this through
the action defined in the |Trace|.

\begin{code}
traceNamedItem
    :: MonadIO m
    => Trace m a
    -> PrivacyAnnotation
    -> Severity
    -> a
    -> m ()
traceNamedItem logTrace p s m =
    traceNamedObject logTrace =<<
        (,) <$> liftIO (mkLOMeta s p)
            <*> pure (LogMessage m)

\end{code}

\subsubsection{Trace a |LogObject| through}
\label{code:traceLogObject}\index{traceLogObject}
\begin{code}
traceLogObject
    :: MonadIO m
    => Trace m a
    -> LogObject a
    -> m ()
traceLogObject Trace{traceStatic, traceTracer} lo =
    traceWith traceTracer (traceStatic, lo)

\end{code}

\subsubsection{Concrete Trace on stdout}\label{code:stdoutTrace}\index{stdoutTrace}

This function returns a trace with an action of type "|LogObject a -> IO ()|"
which will output a text message as text and all others as JSON encoded representation
to the console.

\todo[inline]{TODO remove |locallock|}
%if style == newcode
\begin{code}
{-# NOINLINE locallock #-}
\end{code}
%endif
\begin{code}
locallock :: MVar ()
locallock = unsafePerformIO $ newMVar ()

{-# NOINLINE textToStdout #-}
textToStdout :: T.Text -> IO ()
textToStdout msg = withMVar locallock $ \_ -> TIO.putStrLn msg
\end{code}

\begin{code}
stdoutTrace :: Trace IO T.Text
stdoutTrace =
  mkBaseTrace $ Tracer $
   \(TraceStatic{loggerName}, LogObject{loContent}) ->
        textToStdout $!
          -- TODO:  not particularly efficient, a piecemeal write,
          -- or a Data.Builder-based approach might be faster.
          ((loggerNameText loggerName <> " :: ") <>) $
          case loContent of
            LogMessage txt -> txt
            -- TODO: not particularly efficient, because of recoding,
            -- see the implementation of 'encodeToLazyText'.
            -- We should convert to a ByteString and write that, instead.
            obj            -> toStrict (encodeToLazyText obj)

\end{code}

\subsubsection{Concrete Trace into a |TVar|}\label{code:traceInTVar}\label{code:traceInTVarIO}\index{traceInTVar}\index{traceInTVarIO}

\begin{code}
traceInTVar :: STM.TVar [a] -> Tracer STM.STM a
traceInTVar tvar = Tracer $ \a -> STM.modifyTVar tvar ((:) a)

traceInTVarIO :: STM.TVar [a] -> Tracer IO a
traceInTVarIO tvar = Tracer $ \a ->
                         STM.atomically $ STM.modifyTVar tvar ((:) a)

\end{code}

\subsubsection{Logging functions}
\label{code:logDebug}\index{logDebug}
\label{code:logDebugS}\index{logDebugS}
\label{code:logInfo}\index{logInfo}
\label{code:logInfoS}\index{logInfoS}
\label{code:logNotice}\index{logNotice}
\label{code:logNoticeS}\index{logNoticeS}
\label{code:logWarning}\index{logWarning}
\label{code:logWarningS}\index{logWarningS}
\label{code:logError}\index{logError}
\label{code:logErrorS}\index{logErrorS}
\label{code:logCritical}\index{logCritical}
\label{code:logCriticalS}\index{logCriticalS}
\label{code:logAlert}\index{logAlert}
\label{code:logAlertS}\index{logAlertS}
\label{code:logEmergency}\index{logEmergency}
\label{code:logEmergencyS}\index{logEmergencyS}
\begin{code}
logDebug, logInfo, logNotice, logWarning, logError, logCritical, logAlert, logEmergency
    :: MonadIO m => Trace m a -> a -> m ()
logDebug     logTrace = traceNamedItem logTrace Public Debug
logInfo      logTrace = traceNamedItem logTrace Public Info
logNotice    logTrace = traceNamedItem logTrace Public Notice
logWarning   logTrace = traceNamedItem logTrace Public Warning
logError     logTrace = traceNamedItem logTrace Public Error
logCritical  logTrace = traceNamedItem logTrace Public Critical
logAlert     logTrace = traceNamedItem logTrace Public Alert
logEmergency logTrace = traceNamedItem logTrace Public Emergency

logDebugS, logInfoS, logNoticeS, logWarningS, logErrorS, logCriticalS, logAlertS, logEmergencyS
    :: MonadIO m => Trace m a -> a -> m ()
logDebugS     logTrace = traceNamedItem logTrace Confidential Debug
logInfoS      logTrace = traceNamedItem logTrace Confidential Info
logNoticeS    logTrace = traceNamedItem logTrace Confidential Notice
logWarningS   logTrace = traceNamedItem logTrace Confidential Warning
logErrorS     logTrace = traceNamedItem logTrace Confidential Error
logCriticalS  logTrace = traceNamedItem logTrace Confidential Critical
logAlertS     logTrace = traceNamedItem logTrace Confidential Alert
logEmergencyS logTrace = traceNamedItem logTrace Confidential Emergency

\end{code}
