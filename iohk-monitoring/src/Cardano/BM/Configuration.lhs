
\subsection{Cardano.BM.Configuration}
\label{code:Cardano.BM.Configuration}

%if style == newcode
\begin{code}
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
    , CM.getGUIport
    , CM.getMonitors
    , getOptionOrDefault
    , evalFilters
    , testSubTrace
    , testSeverity
    ) where

import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T

import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.SubTrace
import           Cardano.BM.Data.Severity (Severity (..))

\end{code}
%endif

see |Cardano.BM.Configuration.Model| for the implementation.

\label{code:getOptionOrDefault}\index{getOptionOrDefault}
\begin{code}
getOptionOrDefault :: CM.Configuration -> Text -> Text -> IO (Text)
getOptionOrDefault cg name def = do
    opt <- CM.getOption cg name
    case opt of
        Nothing -> return def
        Just o -> return o

\end{code}

\subsubsection{Evaluation of |FilterTrace|}\label{code:evalFilters}\index{evalFilters}\label{code:testSubTrace}\index{testSubTrace}

A filter consists of a |DropName| and a list of |UnhideNames|. If the context name matches
the |DropName| filter, then at least one of the |UnhideNames| must match the name to have
the evaluation of the filters return |True|.

\begin{code}
testSubTrace :: CM.Configuration -> LoggerName -> LogObject a -> IO (Maybe (LogObject a))
testSubTrace config loggername lo = do
    subtrace <- fromMaybe Neutral <$> CM.findSubTrace config loggername
    return $ testSubTrace' lo subtrace
  where
    testSubTrace' :: LogObject a -> SubTrace -> Maybe (LogObject a)
    testSubTrace' _ NoTrace = Nothing
    testSubTrace' (LogObject _ _ (ObserveOpen _)) DropOpening = Nothing
    testSubTrace' o@(LogObject loname _ (LogValue vname _)) (FilterTrace filters) =
        if evalFilters filters (loname <> "." <> vname)
        then Just o
        else Nothing
    testSubTrace' o (FilterTrace filters) =
        if evalFilters filters (loName o)
        then Just o
        else Nothing
    testSubTrace' o (SetSeverity sev) = Just $ o{ loMeta = (loMeta o){ severity = sev } }
    testSubTrace' o _ = Just o -- fallback: all pass

evalFilters :: [(DropName, UnhideNames)] -> LoggerName -> Bool
evalFilters fs nm =
    all (\(no, yes) -> if (dropFilter nm no) then (unhideFilter nm yes) else True) fs
  where
    dropFilter :: LoggerName -> DropName -> Bool
    dropFilter name (Drop sel) = {-not-} (matchName name sel)
    unhideFilter :: LoggerName -> UnhideNames -> Bool
    unhideFilter _ (Unhide []) = False
    unhideFilter name (Unhide us) = any (\sel -> matchName name sel) us
    matchName :: LoggerName -> NameSelector -> Bool
    matchName name (Exact name') = name == name'
    matchName name (StartsWith prefix) = T.isPrefixOf prefix name
    matchName name (EndsWith postfix) = T.isSuffixOf postfix name
    matchName name (Contains name') = T.isInfixOf name' name

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
