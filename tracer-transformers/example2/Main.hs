{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Control.Concurrent (ThreadId, myThreadId)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar,
                     modifyMVar_, putMVar, readMVar, tryTakeMVar, withMVar)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Functor.Contravariant (Contravariant (..))
import           Data.Text (Text)

import           Control.Tracer

import           Control.Tracer.Transformers.WithThreadAndTime

-- The "top level" invocation of code with different transformations
-- on the tracer

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

  putStrLn "controlled by verbosity:"
  let condTracer v = threadAndTimeTracer . condVerbosity v
      trEnvironment = showTracing stdoutTracer
  myCode (condTracer Normal trEnvironment)
  putStrLn ""

  putStrLn "benchmarking run of code:"
  otherCode myCodeTracersBenchmarking
  putStrLn ""

  putStrLn "production run of code:"
  otherCode (myCodeTracersProduction stdoutTracer)
  putStrLn ""

--- demo code

-- a level of verbosity
data Verbosity = Silent | Normal | Verbose
                 deriving (Eq, Ord, Show)

verbosityP :: Monad m => Verbosity -> m Bool
verbosityP v = do
    vact <- getVerbosityLimit
    return $ v >= vact

-- fake reading of a state:
getVerbosityLimit :: Monad m => m Verbosity
getVerbosityLimit = return Normal

-- the trace transformer on verbosity
condVerbosity :: Monad m => Verbosity -> Tracer m a -> Tracer m a
condVerbosity v = condTracingM (const <$> verbosityP v)


-- The distict message types that you want to expose. This is the set
-- of observables.

data MyMessages a = MyStart a | MyNormal a | MyWarning (Int, a)
                  deriving Show

-- some events that may be of interest (with some other IO to give
-- visual context)
myCode :: (MonadIO m)
       => Tracer m (MyMessages Int)
       -> m ()
myCode tr = do
    liftIO $ putStrLn "<<<< begin"
    traceWith tr $ MyStart 0
    traceWith tr $ MyWarning (37, (-1))
    traceWith tr $ MyNormal 42
    liftIO $ putStrLn ">>>> end."

otherCode :: (MonadIO m)
    => MyCodeTracers m
    -> m ()
otherCode trs = do
    traceWith (tr1 trs) $ MyStart BlockAction1
    traceWith (tr2 trs) $ MyStart (Timing 0)
    traceWith (tr1 trs) $ MyNormal BlockAction1
    traceWith (tr2 trs) $ MyStart (Timing 17)
    -- ...
    traceWith (tr1 trs) $ MyStart BlockAction2
    traceWith (tr2 trs) $ MyStart (Timing 21)
    traceWith (tr1 trs) $ MyNormal BlockAction2
    traceWith (tr2 trs) $ MyNormal (Timing 99)

  -- collection of Tracers
data BlockActions = BlockAction1 | BlockAction2 deriving (Show)
data Timing = Timing Int deriving (Show)

data MyCodeTracers m = MyCodeTracers 
    { tr1 :: Tracer m (MyMessages BlockActions)
    , tr2 :: Tracer m (MyMessages Timing)
    }  -- and so forth

myCodeTracersBenchmarking = MyCodeTracers
    { tr1 = nullTracer
    , tr2 = threadAndTimeTracer $ showTracing stdoutTracer
    }
myCodeTracersProduction tr = MyCodeTracers
    { tr1 = condVerbosity Verbose $ showTracing tr
    , tr2 = condVerbosity Normal $ aggregationTr $ showTracing tr
    }

aggregationTr :: Tracer m (Text, Int) -> Tracer m a
aggregationTr tr = Tracer $ \arg ->
    traceWith tr ("name", 71)  -- fixed for now
