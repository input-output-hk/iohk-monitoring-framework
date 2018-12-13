
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
    , takedown
    ) where

import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar,
                     takeMVar, withMVar, tryTakeMVar)
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import           Control.Monad (void)
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
pass :: Aggregation -> TBQ.TBQueue (Maybe NamedLogItem) -> NamedLogItem -> IO ()
pass agg switchboardQueue item = do
    ag <- takeMVar (getAg agg)
    let (updatedMap, maybeAggregatedMsg) = update (lnItem item) (lnName item) (agMap ag)
    case maybeAggregatedMsg of
        Just aggregatedMsg@(AggregatedMessage _ _) ->
            -- forward the aggregated message to Switchboard
            atomically $ TBQ.writeTBQueue switchboardQueue $
                Just $ LogNamed
                            { lnName = (lnName item) <> ".aggregated"
                            , lnItem = aggregatedMsg
                            }
        _ -> return ()
    putMVar (getAg agg) $ AggregationInternal updatedMap (agSome ag)
  where
    update :: LogObject
           -> LoggerName
           -> HM.HashMap Text Aggregated
           -> (HM.HashMap Text Aggregated, Maybe LogObject)
    update (LP (LogValue iname value)) logname agmap =
        let name = logname <> "." <> iname
            maybeAggregated = updateAggregation value $ HM.lookup name agmap
            aggregatedMessage = case maybeAggregated of
                                    Nothing ->
                                        Nothing
                                    Just aggregated ->
                                        Just $ AggregatedMessage iname aggregated
        in
        -- use of HM.alter so that in future we can clear the Agrregated
        -- by using as alter's arg a function which returns Nothing.
        (HM.alter (const $ maybeAggregated) name agmap, aggregatedMessage)
    -- TODO for text messages aggregate on delta of timestamps
    update _ _ agmap = (agmap, Nothing)

\end{code}

\begin{code}
takedown :: Aggregation -> IO ()
takedown = clearMVar . getAg

clearMVar :: MVar a -> IO ()
clearMVar = void . tryTakeMVar

\end{code}
