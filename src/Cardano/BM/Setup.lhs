
\subsection{Cardano.BM.Setup}
\label{code:Cardano.BM.Setup}

\begin{figure}[ht]
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
    , shutdownTrace
    , withTrace
    , newContext
    ) where

import           Control.Exception.Safe (MonadMask, bracket)
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
or a |FilePath| to a configuration file. After all tracing operations have ended;
|shutdownTrace| must be called.
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
    ctx <- liftIO $ newContext "" c sev sb

    tr <- subTrace name $ natTrace liftIO (ctx, Switchboard.mainTrace sb)
    return tr

\end{code}

\subsubsection{shutdownTrace}\label{code:shutdownTrace}\index{shutdownTrace}
Shut down a Trace and all the |Trace|s related to it.
\begin{code}
shutdownTrace :: MonadIO m => Trace m -> IO ()
shutdownTrace (ctx, _) = shutdown ctx

\end{code}

\subsubsection{withTrace}\label{code:withTrace}\index{withTrace}
\begin{code}
withTrace :: (MonadIO m, MonadMask m) =>  Config.Configuration -> Text -> (Trace m -> m t) -> m t
withTrace cfg name action =
    bracket (setupTrace (Right cfg) name) (liftIO <$> shutdownTrace) action

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
