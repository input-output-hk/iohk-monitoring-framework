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
  let condTracer v tr = threadAndTimeTracer $ condVerbosity v tr
  myCode (condTracer Normal $ showTracing stdoutTracer)
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
condVerbosity v = condTracing (verbosityP v)


-- The distict message types that you want to expose. This is the set
-- of observables.

data MyMessages = MyStart | MyNormal | MyWarning Int
                  deriving Show

-- some events that may be of interest (with some other IO to give
-- visual context)
myCode :: (MonadIO m)
       => Tracer m MyMessages
       -> m ()
myCode tr = do
  liftIO $ putStrLn "<<<< begin"
  traceWith tr   MyStart
  traceWith tr $ MyWarning 37
  traceWith tr   MyNormal
  liftIO $ putStrLn ">>>> end."
