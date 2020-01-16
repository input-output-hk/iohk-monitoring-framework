\subsection{Cardano.BM.Data.Transformers}
\label{code:Cardano.BM.Data.Transformers}

%if style == newcode
\begin{code}

module Cardano.BM.Data.Transformers
  ( setHostname
  )
  where

import           Data.Text (Text)
import           Cardano.BM.Data.LogItem (LogObject (..), LOMeta (..))
import           Cardano.BM.Data.Tracer (Tracer (..), traceWith)
import           Cardano.BM.Data.Trace (Trace)

\end{code}

\subsubsection{Transformer for setting hostname annotation}
\label{code:setHostname}
\index{setHostname}
The hostname annotation of the |LogObject| can be altered.
\begin{code}
setHostname :: Text -> Trace m a -> Trace m a
setHostname hn tr = Tracer $ \(ctx, lo@(LogObject _ln meta _lc)) ->
    traceWith tr (ctx, lo { loMeta = meta { hostname = hn }})

\end{code}
