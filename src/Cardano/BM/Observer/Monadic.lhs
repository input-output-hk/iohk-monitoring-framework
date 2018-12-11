
\subsection{Cardano.BM.Observer.Monadic}

%if style == newcode
\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.Observer.Monadic
    (
      bracketObserveIO
      -- * observing functions
    , observeOpen
    , observeClose
    ) where

import           Control.Monad (forM_)

import           Data.Monoid ((<>))
import           Data.Text
import           Data.Unique (hashUnique, newUnique)


import           Cardano.BM.Data.Counter
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.SubTrace
import           Cardano.BM.Counters (readCounters)
import           Cardano.BM.Trace (Trace, logInfo, subTrace, traceNamedObject,
                     typeofTrace)
\end{code}
%endif

\subsubsection{Monadic.bracketObserverIO}
Observes an |IO| action and adds a name to the logger
name of the passed in |Trace|. An empty |Text| leaves
the logger name untouched.
\begin{code}
bracketObserveIO :: Trace IO -> Text -> IO t -> IO t
bracketObserveIO logTrace0 name action = do
    logTrace <- subTrace name logTrace0
    bracketObserveIO' (typeofTrace logTrace) logTrace action
  where
    bracketObserveIO' :: SubTrace -> Trace IO -> IO t -> IO t
    bracketObserveIO' NoTrace _ act = act
    bracketObserveIO' subtrace logTrace act = do
        countersid <- observeOpen subtrace logTrace
        -- run action
        t <- act
        observeClose subtrace logTrace countersid []
        pure t

\end{code}

\subsubsection{observerOpen}\label{observeOpen}
\begin{code}
observeOpen :: SubTrace -> Trace IO -> IO CounterState
observeOpen subtrace logTrace = do
    identifier <- newUnique
    logInfo logTrace $ "Opening: " <> pack (show $ hashUnique identifier)

    -- take measurement
    counters <- readCounters subtrace
    let state = CounterState identifier counters
    -- send opening message to Trace
    traceNamedObject logTrace $ ObserveOpen state
    return state

\end{code}

\subsubsection{observeClose}\label{observeClose}
\begin{code}
observeClose :: SubTrace -> Trace IO -> CounterState -> [LogObject] -> IO ()
observeClose subtrace logTrace (CounterState identifier _) logObjects = do
    logInfo logTrace $ "Closing: " <> pack (show $ hashUnique identifier)

    -- take measurement
    counters <- readCounters subtrace
    -- send closing message to Trace
    traceNamedObject logTrace $ ObserveClose (CounterState identifier counters)
    -- trace the messages gathered from inside the action
    forM_ logObjects $ traceNamedObject logTrace

\end{code}
