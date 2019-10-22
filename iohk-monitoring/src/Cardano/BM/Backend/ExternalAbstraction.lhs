
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
import           System.IO.Error (mkIOError, doesNotExistErrorType,
                     userErrorType)
import           System.Posix.Files (createNamedPipe, fileExist, getFileStatus,
                     isNamedPipe, stdFileMode)
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
        let open_pipe = openFile pipePath ReadWriteMode
        in
        (createNamedPipe pipePath stdFileMode >> (P <$> open_pipe))
        -- use of ReadWriteMode instead of ReadMode in order
        -- EOF not to be written at the end of file
            `catch` (\(e :: SomeException) ->
                        case fromException e of
                            Just (IOError _ AlreadyExists _ _ _ _) -> do
                                exists <- fileExist pipePath
                                fstatus <- getFileStatus pipePath
                                if exists && isNamedPipe fstatus
                                then
                                    P <$> open_pipe
                                else
                                    throw $ mkIOError userErrorType "cannot open pipe; it's a file" Nothing (Just pipePath)
                            _                                      -> do
                                hPutStrLn stderr $ "Creating pipe threw: " ++ show e
                                throw e)
    open pipePath = do
        exists <- fileExist pipePath
        when (not exists) $
            throw $ mkIOError doesNotExistErrorType "cannot find pipe to open, reason" Nothing (Just pipePath)
        fstatus <- getFileStatus pipePath
        if isNamedPipe fstatus
        then do
            h <- openFile pipePath WriteMode
                `catch` (\(e :: SomeException) -> do
                    hPutStrLn stderr $ "Opening pipe threw: " ++ show e
                                    ++ "\nForwarding its objects to stderr"
                    hDuplicate stderr)
            hSetBuffering h NoBuffering
            return $ P h
        else
            throw $ mkIOError userErrorType "cannot open pipe; it's a file" Nothing (Just pipePath)
    close (P h) = hClose h `catch` (\(_ :: SomeException) -> pure ())
    getLine (P h) = BS.hGetLine h
    write (P h) bs = BSC.hPutStrLn h $! bs

#endif

\end{code}
