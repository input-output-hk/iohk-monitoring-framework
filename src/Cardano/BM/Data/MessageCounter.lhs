
\subsection{Cardano.BM.Data.MessageCounter}
\label{code:Cardano.BM.Data.MessageCounter}

%if style == newcode
\begin{code}
{-# LANGUAGE DeriveAnyClass    #-}

module Cardano.BM.Data.MessageCounter
  ( MessageCounter (..)
  , resetCounters
  , updateMessageCounters
  , sendAndResetAfter
  )
  where

import           Control.Concurrent.Async.Timer (defaultConf, withAsyncTimer,
                     setInterval, wait)
import           Control.Concurrent.MVar (MVar, modifyMVar_)
import           Control.Monad (forM_, forever)
import qualified Data.HashMap.Strict as HM
import           Data.Text (pack)
import           Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import           Data.Word (Word64)

import           Cardano.BM.Data.Aggregated (Measurable (PureI))
import           Cardano.BM.Data.LogItem (LoggerName, LOContent(LogValue),
                    LOMeta (..), LogObject (..), mkLOMeta)
import           Cardano.BM.Data.Severity (Severity (..))
import           Cardano.BM.Data.Trace
import qualified Cardano.BM.Trace as Trace
\end{code}
%endif

\subsubsection{MessageCounter}\label{code:MessageCounter}\index{MessageCounter}
Data structure holding essential info for message counters.
\begin{code}
data MessageCounter = MessageCounter
                        { mcStart       :: {-# UNPACK #-} !UTCTime
                        , mcCountersMap :: HM.HashMap String Word64
                        }
                        deriving (Show)

\end{code}

\subsubsection{Update counters.}
Update counter for specific severity and type of message.
\begin{code}
updateMessageCounters :: MessageCounter -> LogObject -> MessageCounter
updateMessageCounters mc (LogObject meta content) =
    let sev = show $ severity meta
        messageType = head $ words $ show content
        increasedCounter key cmap =
            case HM.lookup key cmap of
                Nothing -> 1 :: Word64
                Just x  -> x + 1
        sevCounter  = increasedCounter sev $ mcCountersMap mc
        typeCounter = increasedCounter messageType $ mcCountersMap mc
    in
    mc { mcCountersMap =
            HM.insert messageType typeCounter $
                HM.insert sev sevCounter $ mcCountersMap mc
        }

\end{code}

\subsubsection{Reset counters}
Reset counters.
\begin{code}
resetCounters :: UTCTime -> MessageCounter
resetCounters time = MessageCounter
                        { mcStart       = time
                        , mcCountersMap = HM.empty
                        }

\end{code}

\subsubsection{Send counters to Switchboard}
Send counters to |Switchboard| and reset them.
\begin{code}
sendAndReset
    :: Trace IO
    -> MessageCounter
    -> Severity
    -> IO MessageCounter
sendAndReset trace counters sev = do
    now <- getCurrentTime
    let start = mcStart counters
        diffTime = round $ diffUTCTime now start

    forM_ (HM.toList $ mcCountersMap counters) $ \(key, count) ->
        Trace.traceConditionally trace =<<
            LogObject
                <$> (mkLOMeta sev)
                <*> pure (LogValue (pack key) (PureI $ toInteger count))
    Trace.traceConditionally trace =<<
        LogObject
            <$> (mkLOMeta sev)
            <*> pure (LogValue "time_interval_(s)" (PureI diffTime))
    return $ resetCounters now

\end{code}

\subsubsection{Send counters to Switchboard after specific amount of time.}
Send counters to |Switchboard| and reset them after a given interval in milliseconds.
\begin{code}
sendAndResetAfter
    :: Trace IO
    -> LoggerName
    -> MVar MessageCounter
    -> Int
    -> Severity
    -> IO ()
sendAndResetAfter trace name counters interval sev = do
    let timerConf = setInterval interval defaultConf
    trace' <- Trace.appendName name trace
    withAsyncTimer timerConf $ \ timer -> do
        forever $ do
            wait timer
            modifyMVar_ counters $ \cnt ->
                sendAndReset trace' cnt sev

\end{code}
