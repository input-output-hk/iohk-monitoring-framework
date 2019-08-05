{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Control.Concurrent (ThreadId, myThreadId)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar,
                     modifyMVar_, putMVar, readMVar, tryTakeMVar, withMVar)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Functor.Contravariant (Contravariant (..))
import           Data.Time.Clock (DiffTime)
import           Data.Time.Clock.System (SystemTime, getSystemTime, systemToTAITime)
import           Data.Time.Clock.TAI (AbsoluteTime, diffAbsoluteTime)
import           Data.Word (Word64)
import           GHC.Clock (getMonotonicTimeNSec)

import           Control.Tracer
import           Control.Tracer.Observe

import           Control.Tracer.Transformers.ObserveOutcome
import           Control.Tracer.Transformers.WithThreadAndTime

-- The "top level" invocation of code with different transformations
-- on the tracer

-- | time in nanoseconds stored in 'Word64'
type Time = Word64

main :: IO ()
main = do
  putStrLn "no output:"
  myCode nullTracer
  putStrLn ""
  putStrLn "output to stdout:"
  myCode (showTracing stdoutTracer)
  putStrLn ""
  putStrLn "adds thread id and time to output:"
  myCode (threadAndTimeTracer $ showTracing stdoutTracer)
  putStrLn ""
  putStrLn "outcomes as well:"
  t1 <- mkOutcomeExtractor
  myCode (t1 $ showTracing stdoutTracer)
  t2 <- mkOutcomeExtractor
  myCode (t2 $ threadAndTimeTracer $ showTracing stdoutTracer)
  putStrLn ""

  putStrLn "using Control.Tracer.Observe: "
  localState <- newMVar Nothing
  let tr = showTracing stdoutTracer
  let trObserve :: Tracer IO (Observable Time Time Time)
      trObserve = tr
      transform :: Tracer IO (Observable Time Time Time) -> Tracer IO ObserveIndicator
      transform trace = Tracer $ \observeIndicator -> do
          now <- getMonotonicTimeNSec
          case observeIndicator of
              ObserveBefore -> traceWith trace $ OStart now
              ObserveAfter  -> traceWith trace $ OEnd   now Nothing
  let trObserve'  = transform $ matchObservations
                                  (readMVar localState)
                                  (\x -> modifyMVar_ localState (const $ return $ Just x))
                                  (flip (-))
                                  tr
  -- trace "before"
  traceWith trObserve' ObserveBefore
  myCode (showTracing stdoutTracer)
  -- trace "after" and calculate diff
  traceWith trObserve' ObserveAfter

-- Show instance for displaying on stdout
instance Show (Observable Time Time Time) where
  show (OStart time)     = "OStart " ++ show time
  show (OEnd time mTime) = "OEnd "   ++ show time ++ ", ODiff " ++ show mTime


--- demo code

-- The distict message types that you want to expose. This is the set
-- of observables.

data MyMessages
  = MyStart
  | MyEnd
  | MyNotice String
  deriving Show

-- some events that may be of interest (with some other IO to give
-- visual context)
myCode :: (MonadIO m)
       => Tracer m MyMessages
       -> m ()
myCode tr = do
  liftIO $ putStrLn "<<<< begin"
  traceWith tr   MyStart
  traceWith tr $ MyNotice "the middle"
  traceWith tr   MyEnd
  liftIO $ putStrLn ">>>> end."


--- instances of 'Outcome'

instance (MonadIO m) => Outcome m MyMessages where
  type IntermediateValue MyMessages = AbsoluteTime
  type OutcomeMetric MyMessages  = DiffTime

  classifyObservable = pure . \case
    MyStart -> OutcomeStarts
    MyEnd   -> OutcomeEnds
    _       -> OutcomeOther

  captureObservableValue _   = systemToTAITime <$> liftIO getSystemTime

  computeOutcomeMetric _ x y = pure $ diffAbsoluteTime y x

instance (MonadIO m, Outcome m a) => Outcome m (WithThreadAndTime a) where
  type IntermediateValue (WithThreadAndTime a) = AbsoluteTime
  type OutcomeMetric (WithThreadAndTime a) = DiffTime

  classifyObservable = classifyObservable . event

  captureObservableValue = pure . systemToTAITime . occurredAt

  computeOutcomeMetric _ x y = pure $ diffAbsoluteTime y x
