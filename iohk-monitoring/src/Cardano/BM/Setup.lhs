
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
    , shutdown
    , withTrace
    ) where

import           Control.Exception.Safe (MonadMask, bracket)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson (ToJSON)
import           Data.Text (Text)
import           System.IO (FilePath)

import qualified Cardano.BM.Configuration as Config
import qualified Cardano.BM.Output.Switchboard as Switchboard
import           Cardano.BM.Trace (Trace, appendName, natTrace)

\end{code}
%endif

\subsubsection{setupTrace}\label{code:setupTrace}\index{setupTrace}
Setup a new |Trace| (\nameref{code:Trace}) with either a given |Configuration| (\nameref{code:Configuration})
or a |FilePath| to a configuration file. After all tracing operations have ended;
|shutdownTrace| must be called.
\begin{code}

setupTrace :: (MonadIO m, Show a, ToJSON a) => Either FilePath Config.Configuration -> Text -> m (Trace m a)
setupTrace (Left cfgFile) name = do
    c <- liftIO $ Config.setup cfgFile
    fst <$> setupTrace_ c name
setupTrace (Right c) name = fst <$> setupTrace_ c name

setupTrace_ :: (MonadIO m, Show a, ToJSON a) => Config.Configuration -> Text -> m (Trace m a, Switchboard.Switchboard a)
setupTrace_ c name = do
    sb <- liftIO $ Switchboard.realize c

    tr <- appendName name $ natTrace liftIO (Switchboard.mainTraceConditionally c sb)
    return (tr,sb)

\end{code}

\subsubsection{shutdown}\label{code:shutdown}\index{shutdown}
Shut down the Switchboard and all the |Trace|s related to it.
\begin{code}
shutdown :: (Show a, ToJSON a) => Switchboard.Switchboard a -> IO ()
shutdown = Switchboard.unrealize

\end{code}

\subsubsection{withTrace}\label{code:withTrace}\index{withTrace}
Setup a |Trace| from |Configuration| and pass it to the action. At the end,
shutdown all the components and close the trace.
\begin{code}
withTrace :: (MonadIO m, MonadMask m, Show a, ToJSON a) =>  Config.Configuration -> Text -> (Trace m a -> m t) -> m t
withTrace cfg name action =
    bracket
        (setupTrace_ cfg name)              -- aquire
        (\(_,sb) -> liftIO $ shutdown sb)   -- release
        (\(tr,_) -> action tr)              -- action

\end{code}
