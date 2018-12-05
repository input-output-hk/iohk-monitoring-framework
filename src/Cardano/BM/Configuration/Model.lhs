\subsection{Cardano.BM.Configuration.Model}
\label{module:Cardano.BM.Configuration.Model}

%if style == newcode
\begin{code}
{-# LANGUAGE OverloadedStrings   #-}

module Cardano.BM.Configuration.Model
    (
      Configuration
    , setup
    , empty
    , minSeverity
    , setMinSeverity
    , inspectSeverity
    , setSeverity
    , getBackends
    , registerBackend
    , setDefaultBackends
    , setSetupBackends
    , getScribes
    , setDefaultScribes
    , setSetupScribes
    , getOption
    , findSubTrace
    , setSubTrace
    --, inspectOutput
    --, takedown
    ) where

import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar,
                     takeMVar, withMVar)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text, pack)
import           Data.Yaml as Y

import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.LogItem (LoggerName)
import           Cardano.BM.Data.Output (ScribeDefinition, ScribeId)
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.SubTrace

\end{code}
%endif

\subsubsection{Configuration.Model}\label{code:Configuration}
\begin{figure}[htp]
\centering{
  \includegraphics[scale=0.54]{ConfigurationModel.pdf}
}
\caption{Configuration model}\label{fig:configuration}
\end{figure}

\begin{code}
type ConfigurationMVar = MVar ConfigurationInternal
newtype Configuration = Configuration
    { getCG :: ConfigurationMVar }

-- Our internal state; see {-"\nameref{fig:configuration}"-}
data ConfigurationInternal = ConfigurationInternal
    { cgMinSeverity   :: Severity
    , cgMapSeverity   :: HM.HashMap LoggerName Severity
    , cgMapSubtrace   :: HM.HashMap LoggerName SubTrace
    , cgOptions       :: HM.HashMap LoggerName Object
    , cgMapBackend    :: HM.HashMap LoggerName [Backend]
    , cgDefBackends   :: [Backend]
    , cgSetupBackends :: [BackendKind]
    , cgMapScribe     :: HM.HashMap LoggerName [ScribeId]
    , cgDefScribes    :: [ScribeId]
    , cgSetupScribes  :: [ScribeDefinition]
    }

\end{code}
\todo[inline]{TODO |listOutput   <- o .:? "output_map"   .!= []|}
\todo[inline]{TODO |defaultBackendKinds <- o .:? "default_backends" .!= []|}

\subsubsection{Backends configured in the |Switchboard|}
For a given context name return the list of backends configured,
or, in case no such configuration exists, return the default backends.
\begin{code}
getBackends :: Configuration -> LoggerName -> IO [Backend]
getBackends configuration name =
    withMVar (getCG configuration) $ \cg -> do
        let outs = HM.lookup name (cgMapBackend cg)
        case outs of
            Nothing -> do
                return (cgDefBackends cg)
            Just os -> return $ os

setDefaultBackends :: Configuration -> [Backend] -> IO ()
setDefaultBackends configuration bes = do
    cg <- takeMVar (getCG configuration)
    putMVar (getCG configuration) $ cg { cgDefBackends = bes }

registerBackend :: Configuration -> Text -> Maybe Backend -> IO ()
registerBackend _ _kn _f = pure () -- TODO
  --  registerBackend "some" (Just Backend { pass' = Katip.pass (show StdoutSK) })
  --  registerBackend "severe.error" (Just Backend { pass' = Katip.pass "StdoutSK::severe.log") })

\end{code}

\subsubsection{Backends to be setup by the |Switchboard|}
Defines the list of |Backend|s that need to be setup by the |Switchboard|.
\begin{code}
setSetupBackends :: Configuration -> [BackendKind] -> IO ()
setSetupBackends configuration bes = do
    cg <- takeMVar (getCG configuration)
    putMVar (getCG configuration) $ cg { cgSetupBackends = bes }

\end{code}


\subsubsection{Scribes configured in the |Log| backend}
For a given context name return the list of scribes to output to,
or, in case no such configuration exists, return the default scribes to use.
\begin{code}
getScribes :: Configuration -> LoggerName -> IO [ScribeId]
getScribes configuration name =
    withMVar (getCG configuration) $ \cg -> do
        let outs = HM.lookup name (cgMapScribe cg)
        case outs of
            Nothing -> do
                return (cgDefScribes cg)
            Just os -> return $ os

setDefaultScribes :: Configuration -> [ScribeId] -> IO ()
setDefaultScribes configuration scs = do
    cg <- takeMVar (getCG configuration)
    putMVar (getCG configuration) $ cg { cgDefScribes = scs }

\end{code}

\subsubsection{Scribes to be setup in the |Log| backend}
Defines the list of |Scribe|s that need to be setup in the |Log| backend.
\begin{code}
setSetupScribes :: Configuration -> [ScribeDefinition] -> IO ()
setSetupScribes configuration sds = do
    cg <- takeMVar (getCG configuration)
    putMVar (getCG configuration) $ cg { cgSetupScribes = sds }

\end{code}

\subsubsection{Options}
\begin{code}
getOption :: Configuration -> Text -> IO (Maybe Text)
getOption configuration name = do
    withMVar (getCG configuration) $ \cg ->
        case HM.lookup name (cgOptions cg) of
            Nothing -> return Nothing
            Just o -> return $ Just $ pack $ show o

\end{code}

\subsubsection{Global setting of minimum severity}
\begin{code}
minSeverity :: Configuration -> IO Severity
minSeverity configuration = withMVar (getCG configuration) $ \cg ->
    return $ cgMinSeverity cg

setMinSeverity :: Configuration -> Severity -> IO ()
setMinSeverity configuration sev = do
    cg <- takeMVar (getCG configuration)
    putMVar (getCG configuration) $ cg { cgMinSeverity = sev }

\end{code}

\subsubsection{Relation of context name to minimum severity}
\begin{code}
inspectSeverity :: Configuration -> Text -> IO (Maybe Severity)
inspectSeverity configuration name = do
    withMVar (getCG configuration) $ \cg ->
        return $ HM.lookup name (cgMapSeverity cg)

-- if Maybe Severity given is Nothing then the entry for this name is deleted.
setSeverity :: Configuration -> Text -> Maybe Severity -> IO ()
setSeverity configuration name sev = do
    cg <- takeMVar (getCG configuration)
    putMVar (getCG configuration) $ cg { cgMapSeverity = HM.alter (\_ -> sev) name (cgMapSeverity cg) }

\end{code}

\subsubsection{Relation of context name to SubTrace}
A new context may contain a different type of |Trace|.
The function |appendName| (\nameref{code:appendName}) will look up the |SubTrace| for the context's name.
\begin{code}
findSubTrace :: Configuration -> Text -> IO (Maybe SubTrace)
findSubTrace configuration name = do
    withMVar (getCG configuration) $ \cg ->
        return $ HM.lookup name (cgMapSubtrace cg)

setSubTrace :: Configuration -> Text -> Maybe SubTrace -> IO ()
setSubTrace configuration name trafo = do
    cg <- takeMVar (getCG configuration)
    putMVar (getCG configuration) $ cg { cgMapSubtrace = HM.alter (\_ -> trafo) name (cgMapSubtrace cg) }

\end{code}

\subsubsection{Configuration.Model.setup}
\begin{code}
setup :: FilePath -> IO Configuration
setup _fp = do
    c <- empty
    -- r <- parseRepresentation
    return c

empty :: IO Configuration
empty = do
    cgref <- newEmptyMVar
    putMVar cgref $ ConfigurationInternal Debug HM.empty HM.empty HM.empty HM.empty [] [] HM.empty [] []
    return $ Configuration cgref

\end{code}
