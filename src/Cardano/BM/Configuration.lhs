
\subsection{Cardano.BM.Configuration}

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
    , getOptionOrDefault
    ) where

import           Data.Text (Text)

import qualified Cardano.BM.Configuration.Model as CM
\end{code}
%endif

see \nameref{module:Cardano.BM.Configuration.Model} for the implementation.

\label{code:getOptionOrDefault}\index{getOptionOrDefault}
\begin{code}
getOptionOrDefault :: CM.Configuration -> Text -> Text -> IO (Text)
getOptionOrDefault cg name def = do
    opt <- CM.getOption cg name
    case opt of
        Nothing -> return def
        Just o -> return o

\end{code}
