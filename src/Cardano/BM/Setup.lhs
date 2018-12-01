
\subsection{Cardano.BM.Setup}

%if False
\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.Setup
    (
      setupTrace
    , withTrace
    , newContext
    ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text (Text)
import           System.IO (FilePath)

import qualified Cardano.BM.Configuration as Config
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Severity
import qualified Cardano.BM.Output.Switchboard
import           Cardano.BM.Data.Trace
import           Cardano.BM.Trace (Trace, natTrace, mainTrace, subTrace)

\end{code}
%endif

\subsubsection{setupTrace}\label{code:setupTrace}
\begin{code}

setupTrace :: MonadIO m => Either FilePath Config.Configuration -> Text -> m (Trace m)
setupTrace (Left cfgfp) name = do
    c <- liftIO $ Config.setup cfgfp
    setupTrace_ c name
setupTrace (Right c) name = setupTrace_ c name

setupTrace_ :: MonadIO m => Config.Configuration -> Text -> m (Trace m)
setupTrace_ c name = do
    sb <- liftIO $ Cardano.BM.Output.Switchboard.setup c
    sev <- liftIO $ Config.minSeverity c
    ctx <- liftIO $ newContext name c sev

    let logTrace = (ctx, natTrace liftIO (mainTrace sb))
    (_, logTrace') <- subTrace "" logTrace
    return logTrace'

\end{code}

\subsubsection{withTrace}\label{code:withTrace}
\begin{code}
withTrace :: MonadIO m =>  Config.Configuration -> Text -> (Trace m -> m t) -> m t
withTrace cfg name action = do
    logTrace <- setupTrace (Right cfg) name
    action logTrace

\end{code}

\subsubsection{TraceContext}\label{code:TraceContext}
\begin{code}
newContext :: LoggerName -> Config.Configuration -> Severity -> IO TraceContext
newContext name cfg sev = do
    return $ TraceContext {
        loggerName = name
      , configuration = cfg
      , minSeverity = sev
      }

\end{code}

