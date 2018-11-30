\subsection{Cardano.BM.Configuration}

%if False
\begin{code}
module Cardano.BM.Configuration
    (
      CM.Configuration
    , CM.setup
    , CM.minSeverity
    , CM.inspectSeverity
    , CM.setSeverity
    , CM.getBackends
    , CM.registerBackend
    , CM.getOption
    , CM.findTransformer
    , getOptionOrDefault
    , parseFrom
    ) where

import           Data.Text (Text)

import qualified Cardano.BM.Configuration.Model as CM
\end{code}
%endif

see \nameref{module:Cardano.BM.Configuration.Model} for the implementation.


\begin{code}
getOptionOrDefault :: CM.Configuration -> Text -> Text -> IO (Text)
getOptionOrDefault cg name def = do
    opt <- CM.getOption cg name
    case opt of
        Nothing -> return def
        Just o -> return o

\end{code}

\begin{code}
parseFrom ::  FilePath -> IO (CM.Configuration)
parseFrom fp = CM.setup fp

\end{code}