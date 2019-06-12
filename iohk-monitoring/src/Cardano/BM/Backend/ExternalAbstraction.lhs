
\subsection{Cardano.BM.Backend.ExternalAbstraction}
\label{module:Cardano.BM.Backend.ExternalAbstraction}


%if style == newcode
\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}


module Cardano.BM.Backend.ExternalAbstraction
    ( Pipe (..)
    , NoPipe
    , UnixNamedPipe
    ) where

import           Control.Exception (SomeException, catch)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Text (pack)
import           GHC.IO.Handle (hDuplicate)
import           System.IO (IOMode (..), openFile, BufferMode (NoBuffering),
                     Handle, hClose, hSetBuffering, openFile, stderr)
import           System.Posix.Files (createNamedPipe, stdFileMode)

import           Cardano.BM.Data.LogItem (LOContent (LogError),
                     PrivacyAnnotation (Public),mkLOMeta)
import           Cardano.BM.Data.Severity (Severity (..))
import qualified Cardano.BM.Trace as Trace

\end{code}
%endif

Abstraction for the communication between |ExternalLogBK| and |LogToPipeBK| backends.

\begin{code}
class Pipe p where
    data family PipeHandler p

    create  :: FilePath -> Trace.Trace IO a -> IO (PipeHandler p)
    open    :: FilePath -> Trace.Trace IO a -> IO (PipeHandler p)
    close   :: PipeHandler p -> IO ()
    write   :: PipeHandler p -> BS.ByteString -> IO ()
    getLine :: PipeHandler p -> IO BS.ByteString

\end{code}

\begin{code}
data NoPipe
data UnixNamedPipe

instance Pipe NoPipe where
    data PipeHandler NoPipe = NP ()
    create  = \_ _ -> pure $ NP ()
    open    = \_ _ -> pure $ NP ()
    close   = \_ -> pure ()
    write   = \_ _ -> pure ()
    getLine = \_ -> pure ""

instance Pipe UnixNamedPipe where
    data PipeHandler UnixNamedPipe = P Handle
    create pipePath sbtrace =
        (createNamedPipe pipePath stdFileMode >> (P <$> openFile pipePath ReadWriteMode))
        -- use of ReadWriteMode instead of ReadMode in order
        -- EOF not to be written at the end of file
            `catch` (\(e :: SomeException) -> do
                            Trace.traceNamedObject sbtrace =<<
                                (,) <$> (mkLOMeta Warning Public)
                                    <*> pure (LogError $ pack $ show e)
                            P <$> hDuplicate stderr)
    open pipePath sbtrace = do
        h <- openFile pipePath WriteMode
                `catch` (\(e :: SomeException) -> do
                    Trace.traceNamedObject sbtrace =<<
                        (,) <$> (mkLOMeta Warning Public)
                            <*> pure (LogError $ pack $ show e)
                    hDuplicate stderr)
        hSetBuffering h NoBuffering
        return $ P h
    close (P h) = hClose h `catch` (\(_ :: SomeException) -> pure ())
    getLine (P h) = BS.hGetLine h
    write (P h) bs = BSC.hPutStrLn h $! bs

\end{code}
