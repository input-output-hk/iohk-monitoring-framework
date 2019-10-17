
\subsection{Cardano.BM.Backend.ExternalAbstraction}
\label{module:Cardano.BM.Backend.ExternalAbstraction}


%if style == newcode
\begin{code}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}


module Cardano.BM.Backend.ExternalAbstraction
    ( Pipe (..)
    , NoPipe
    , UnixNamedPipe
    ) where

#ifndef mingw32_HOST_OS
import           Control.Monad (when)
import           Control.Exception (SomeException (..), catch, fromException,
                     throw)
#endif
import qualified Data.ByteString as BS
#ifndef mingw32_HOST_OS
import qualified Data.ByteString.Char8 as BSC
import           GHC.IO.Exception (IOException (..), IOErrorType (..))
import           GHC.IO.Handle (hDuplicate)
import           System.IO (IOMode (..), openFile, BufferMode (NoBuffering),
                     Handle, hClose, hSetBuffering, openFile, stderr,
                     hPutStrLn)
import           System.IO.Error (mkIOError, doesNotExistErrorType)
import           System.Posix.Files (createNamedPipe, fileExist, stdFileMode)
#endif

\end{code}
%endif

Abstraction for the communication between |TraceAcceptorBK| and |TraceForwarderBK| backends.

\begin{code}
class Pipe p where
    data family PipeHandler p

    create  :: FilePath -> IO (PipeHandler p)
    open    :: FilePath -> IO (PipeHandler p)
    close   :: PipeHandler p -> IO ()
    write   :: PipeHandler p -> BS.ByteString -> IO ()
    getLine :: PipeHandler p -> IO BS.ByteString

\end{code}

\begin{code}
data NoPipe
data UnixNamedPipe

instance Pipe NoPipe where
    data PipeHandler NoPipe = NP ()
    create  = \_ -> pure $ NP ()
    open    = \_ -> pure $ NP ()
    close   = \_ -> pure ()
    write   = \_ _ -> pure ()
    getLine = \_ -> pure ""

#ifndef mingw32_HOST_OS
instance Pipe UnixNamedPipe where
    data PipeHandler UnixNamedPipe = P Handle
    create pipePath =
        (createNamedPipe pipePath stdFileMode >> (P <$> openFile pipePath ReadWriteMode))
        -- use of ReadWriteMode instead of ReadMode in order
        -- EOF not to be written at the end of file
            `catch` (\(e :: SomeException) ->
                        case fromException e of
                            Just (IOError _ AlreadyExists _ _ _ _) ->
                                P <$> openFile pipePath ReadWriteMode
                            _                                      -> do
                                hPutStrLn stderr $ "Creating pipe threw: " ++ show e
                                throw e)
    open pipePath = do
        exists <- fileExist pipePath
        when (not exists) $
            throw $ mkIOError doesNotExistErrorType "cannot find pipe to open, reason" Nothing (Just pipePath)
        h <- openFile pipePath WriteMode
                `catch` (\(e :: SomeException) -> do
                    hPutStrLn stderr $ "Opening pipe threw: " ++ show e
                                    ++ "\nForwarding its objects to stderr"
                    hDuplicate stderr)
        hSetBuffering h NoBuffering
        return $ P h
    close (P h) = hClose h `catch` (\(_ :: SomeException) -> pure ())
    getLine (P h) = BS.hGetLine h
    write (P h) bs = BSC.hPutStrLn h $! bs

#else
instance Pipe UnixNamedPipe where
    data PipeHandler UnixNamedPipe = P ()
    create _ = error "UnixNamedPipe not supported on Windows"
    open _ = return (P ())
    close _ = error "UnixNamedPipe not supported on Windows"
    getLine _ = error "UnixNamedPipe not supported on Windows"
    write _ = error "UnixNamedPipe not supported on Windows"
#endif

\end{code}
