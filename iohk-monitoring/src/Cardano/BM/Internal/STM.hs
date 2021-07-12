{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.BM.Internal.STM
  ( labelledAtomically
  ) where

import           Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import           Control.Concurrent.STM (STM)
import           Control.Exception (SomeException)
import           Control.Monad (void)
import           Control.Monad.Catch (catch, throwM)
import           GHC.Stack (HasCallStack, callStack, getCallStack, withFrozenCallStack)

import qualified Control.Concurrent.STM as STM
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.IO as IO
import qualified System.IO.Unsafe as IO

sem :: MVar ()
sem = IO.unsafePerformIO (newMVar ())
{-# NOINLINE sem #-}

labelledAtomically :: HasCallStack => STM a -> IO a
labelledAtomically f = withFrozenCallStack $ do
  catch (STM.atomically f) $ \(e :: SomeException) -> do
    void $ takeMVar sem
    let cs = getCallStack callStack
    T.hPutStrLn IO.stderr $ "XXX Call to atomically failed: " <> T.pack (show cs)
    void $ putMVar sem ()
    throwM e
