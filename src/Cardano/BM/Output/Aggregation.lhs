
\subsection{Cardano.BM.Output.Aggregation}
\label{module:Cardano.BM.Output.Aggregation}

%if style == newcode
\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.Output.Aggregation
    (
      setup
    , pass
    , inspect
    --, takedown
    ) where

import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar,
                     takeMVar, withMVar)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)

import           Cardano.BM.Aggregated (Aggregated (..), updateAggregation)
import           Cardano.BM.Configuration (Configuration)
import           Cardano.BM.Data.LogItem

\end{code}
%endif

The aggregation is a singleton.
\begin{code}
type AggregationMVar = MVar AggregationInternal
newtype Aggregation = Aggregation
    { getAg :: AggregationMVar }

-- Our internal state
data AggregationInternal = AggregationInternal
    { agMap   :: HM.HashMap Text Aggregated
    , agSome  :: [Int]  -- TODO
    }

\end{code}

\begin{code}
inspect :: Aggregation -> Text -> IO (Maybe Aggregated)
inspect agg name =
    withMVar (getAg agg) $ \ag ->
        return $ HM.lookup name (agMap ag)
\end{code}

\begin{code}
setup :: Configuration -> IO Aggregation
setup _ = do
    aggref <- newEmptyMVar
    -- TODO create thread which will periodically output
    --      aggregated values to the switchboard
    putMVar aggref $ AggregationInternal HM.empty []
    return $ Aggregation aggref

\end{code}

\begin{code}
pass :: Aggregation -> NamedLogItem -> IO ()
pass agg item = do
    ag <- takeMVar (getAg agg)
    putMVar (getAg agg) $ AggregationInternal (updated $ agMap ag) (agSome ag)
  where
    updated agmap = pass' (lnItem item) (lnName item) agmap
    pass' :: LogObject -> LoggerName -> HM.HashMap Text Aggregated -> HM.HashMap Text Aggregated
    pass' (LP (LogValue iname value)) logname agmap =
        let name = logname <> "." <> iname
        in
        HM.alter (\m -> updateAggregation value m) name agmap
    -- TODO for text messages aggregate on delta of timestamps
    pass' _ _ agmap = agmap

\end{code}

