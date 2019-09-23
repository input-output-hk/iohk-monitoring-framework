
\subsection{Cardano.BM.Configuration}
\label{code:Cardano.BM.Configuration}

%if style == newcode
\begin{code}
{-# LANGUAGE CPP #-}

module Cardano.BM.Configuration
    (
      CM.Configuration
    , CM.setup
    , CM.minSeverity
    , CM.setMinSeverity
    , CM.inspectSeverity
    , CM.setSeverity
    , CM.getBackends
    , CM.getOption
    , CM.findSubTrace
    , CM.setSubTrace
    , CM.getEKGport
    , CM.getGraylogPort
    , CM.getPrometheusPort
    , CM.getGUIport
    , CM.getMonitors
    , getOptionOrDefault
    , testSeverity
    , CM.evalFilters
    , CM.testSubTrace
    ) where

import           Data.Text (Text)
import           Data.Maybe (fromMaybe)

import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Severity (Severity (..))

\end{code}
%endif

see |Cardano.BM.Configuration.Model| for the implementation.

\label{code:getOptionOrDefault}\index{getOptionOrDefault}
\begin{code}
getOptionOrDefault :: CM.Configuration -> Text -> Text -> IO Text
getOptionOrDefault cg name def = do
    opt <- CM.getOption cg name
    case opt of
        Nothing -> return def
        Just o -> return o

\end{code}

\subsubsection{Test severities}\label{code:testSeverity}\index{testSeverity}
Test severity of the given |LOMeta| to be greater or equal to those of the specific |LoggerName|.

\begin{code}
testSeverity :: CM.Configuration -> LoggerName -> LOMeta -> IO Bool
testSeverity config loggername meta = do
    globminsev  <- CM.minSeverity config
    globnamesev <- CM.inspectSeverity config loggername
    let minsev = max globminsev $ fromMaybe Debug globnamesev
    return $ (severity meta) >= minsev

\end{code}
