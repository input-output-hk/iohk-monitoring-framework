\subsection{Cardano.BM.Data.Transformers}
\label{code:Cardano.BM.Data.Transformers}

%if style == newcode
\begin{code}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.BM.Data.Transformers
  (
    -- liftCounting,
    setHostname
  )
  where

import           Data.Text (Text)

-- import           Cardano.BM.Data.Aggregated (Measurable (..))
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Tracer (mapTraceMeta)
import           Cardano.BM.Data.Trace

-- import           Control.Tracer.Transformers
\end{code}

\subsubsection{Transformer for counting events}
\label{code:liftCounting}
\index{liftCounting}
Lift a 'Counting' tracer into a 'Trace' of 'PureI' messages.
\begin{code}

-- liftCounting
--   :: forall m a
--   .  LOMeta -> LoggerName -> Text -> Trace m a
--   -> Tracer m (Counting (LoggerName, LogObject a))
-- liftCounting meta name desc = Tracer . traceIncrement
--  where
--    traceIncrement :: Trace m a -> Counting (LoggerName, LogObject a) -> m ()
--    traceIncrement t (Counting n) =
--      traceWith t $ (name, LogObject name meta . LogValue desc . PureI $ fromIntegral n)

\end{code}

\subsubsection{Transformer for setting hostname annotation}
\label{code:setHostname}
\index{setHostname}
The hostname annotation of the |LogObject| can be altered.
\begin{code}
setHostname :: Text -> Trace m a -> Trace m a
setHostname x = mapTraceMeta (\lo -> lo { hostname = x })

\end{code}
