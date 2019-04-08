
\label{code:Control.Tracer.Observe}

%if style == newcode
\begin{code}
{-|
Module: Control.Tracer.Observe

Functions useful for observing and measuring actions.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

module Control.Tracer.Observe
    (
    -- * observing
      ObserveIndicator (..)
    , Observable (..)
    , matchObservations
    -- * example
    , example
    ) where

import           Control.Concurrent.MVar (MVar, newMVar, modifyMVar_, readMVar)
import           Data.Word (Word64)
import           GHC.Clock (getMonotonicTimeNSec)

import           Control.Tracer (Tracer (..), showTracing, stdoutTracer, traceWith)

\end{code}
%endif

\subsection{Examples}
Observe the duration of an action using the timedBracketObserve:

\begin{code}
data AddSub a = Add a
              | Sub a
              deriving Show

type Time = Word64

example :: IO ()
example = do
    let -- a Tracer handling the observations
        trObserve :: Tracer IO (Observable Time Time Time)
        trObserve = showTracing stdoutTracer
        -- a transformer which enriches observations with time measurement
        transform :: Tracer IO (Observable Time Time Time) -> Tracer IO ObserveIndicator
        transform trace = Tracer $ \observeIndicator -> do
            now <- getMonotonicTimeNSec
            case observeIndicator of
                ObserveBefore -> traceWith trace $ OStart now
                ObserveAfter  -> traceWith trace $ OEnd   now Nothing

    beforeMVarAdd  <- newMVar Nothing
    beforeMVarSub <- newMVar Nothing

    let trObserve'  = transform $ matchObservations beforeMVarAdd (flip (-)) trObserve
        trObserve'' = transform $ matchObservations beforeMVarSub (flip (-)) trObserve

    -- observe add
    traceWith trObserve' ObserveBefore
    _ <- actionAdd tr
    traceWith trObserve' ObserveAfter

    -- observe sub
    traceWith trObserve'' ObserveBefore
    _ <- actionSub tr
    traceWith trObserve'' ObserveAfter

  where
    tr :: Tracer IO (AddSub Int)
    tr = showTracing stdoutTracer
    actionAdd :: Tracer IO (AddSub Int) -> IO Int
    actionAdd trace = do
        let res = 1+2
        traceWith trace $ Add res
        return res
    actionSub :: Tracer IO (AddSub Int) -> IO Int
    actionSub trace = do
        let res = 1-2
        traceWith trace $ Sub res
        return res

instance Show (Observable Time Time Time) where
  show (OStart time)     = "OStart " ++ show time
  show (OEnd time mTime) = "OEnd "   ++ show time ++ ", ODiff " ++ show mTime

\end{code}

\subsection{Observe}
\subsubsection{ObserveIndicator}\label{code:ObserveIndicator}\index{ObserveIndicator}
Data structure that indicates the beginning and the end of an observation.
\begin{code}
data ObserveIndicator = ObserveBefore | ObserveAfter
                      deriving Show

\end{code}

\subsubsection{Observable}\label{code:Observable}\index{Observable}
Data structure which holds the observation along with the indicator
of the observation.
\begin{code}
data Observable s e d = OStart s
                      | OEnd e (Maybe d)
                      --         ^^ holds the difference between start and end

\end{code}

\subsubsection{matchObservations}\label{code:matchObservations}\index{matchObservations}
Match start and end of observations.
\begin{code}
matchObservations :: MVar (Maybe s) -> (s -> e -> d) -> Tracer IO (Observable s e d) -> Tracer IO (Observable s e d)
matchObservations beforeMVar f tr = Tracer $ \case
    obs@(OStart s) -> do
        modifyMVar_ beforeMVar $ const $ return $ Just s
        traceWith tr obs
    (OEnd e _) -> do
        before <- readMVar beforeMVar
        traceWith tr $ OEnd e $ fmap ((flip f) e) before

\end{code}
