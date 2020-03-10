
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
    , CM.getAcceptAt
    , CM.getBackends
    , CM.getForwardTo
    , CM.setForwardTo
    , CM.getOption
    , CM.getMapOption
    , CM.getTextOption
    , CM.setOption
    , CM.setTextOption
    , CM.updateOption
    , CM.findSubTrace
    , CM.setSubTrace
    , CM.getEKGport
    , CM.getGraylogPort
    , CM.getPrometheusBindAddr
    , CM.getGUIport
    , CM.getMonitors
    , getTextOptionOrDefault
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
getTextOptionOrDefault :: CM.Configuration -> Text -> Text -> IO Text
getTextOptionOrDefault cg name def = fromMaybe def <$> CM.getTextOption cg name

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
