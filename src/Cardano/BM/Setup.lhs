
\subsection{Cardano.BM.Setup}

\begin{figure}[htp]
\centering{
  \includegraphics[scale=0.54]{SetupProcedure.pdf}
}
\caption{Setup procedure}
\end{figure}

%if style == newcode
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
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.SubTrace
import           Cardano.BM.Data.Trace
import qualified Cardano.BM.Output.Switchboard as Switchboard
import           Cardano.BM.Trace (Trace, natTrace, subTrace)

\end{code}
%endif

\subsubsection{setupTrace}\label{code:setupTrace}\index{setupTrace}
Setup a new |Trace| (\nameref{code:Trace}) with either a given |Configuration| (\nameref{code:Configuration})
or a |FilePath| to a configuration file.
\begin{code}

setupTrace :: MonadIO m => Either FilePath Config.Configuration -> Text -> m (Trace m)
setupTrace (Left cfgFile) name = do
    c <- liftIO $ Config.setup cfgFile
    setupTrace_ c name
setupTrace (Right c) name = setupTrace_ c name

setupTrace_ :: MonadIO m => Config.Configuration -> Text -> m (Trace m)
setupTrace_ c name = do
    sb <- liftIO $ Switchboard.realize c
    sev <- liftIO $ Config.minSeverity c
    ctx <- liftIO $ newContext name c sev sb

    let logTrace = natTrace liftIO (ctx, Switchboard.mainTrace sb)
    logTrace' <- subTrace "" logTrace
    return logTrace'

\end{code}

\subsubsection{withTrace}\label{code:withTrace}\index{withTrace}
\begin{code}
withTrace :: MonadIO m =>  Config.Configuration -> Text -> (Trace m -> m t) -> m t
withTrace cfg name action = do
    logTrace <- setupTrace (Right cfg) name
    action logTrace

\end{code}

\subsubsection{newContext}\label{code:newContext}\index{newContext}
\begin{code}
newContext :: LoggerName
           -> Config.Configuration
           -> Severity
           -> Switchboard.Switchboard
           -> IO TraceContext
newContext name cfg sev sb = do
    return $ TraceContext {
        loggerName = name
      , configuration = cfg
      , minSeverity = sev
      , tracetype = Neutral
      , shutdown = unrealize sb
      }

\end{code}
