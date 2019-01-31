
\subsection{Cardano.BM.Rotator}

Implementation of rotation of logging files.

\begin{code}

{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

-- | monitor log files for max age and max size

#if !defined(mingw32_HOST_OS)
#define POSIX
#endif

module Cardano.BM.Rotator
       ( cleanupRotator
       , evalRotator
       , initializeRotator
       , latestLogFile
       , prtoutException
       ) where

import           Control.Exception.Safe (Exception (..), catchIO)
import           Data.List (sort)
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty (NonEmpty)
import           Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime,
                     parseTimeM)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           System.Directory (listDirectory, removeFile)
import           System.FilePath ((</>), takeDirectory)
import           System.IO (BufferMode (LineBuffering), Handle,
                     IOMode (AppendMode, WriteMode), hFileSize, hSetBuffering,
                     openFile, stdout)

#ifdef POSIX
import           System.Directory (createFileLink)
import           System.FilePath (takeFileName)
#endif

import           Cardano.BM.Data.Rotation (RotationParameters (..))

\end{code}

\subsubsection{Add current time to name of log file}\label{code:nameLogFile}\index{nameLogFile}
\begin{code}
-- format of a timestamp
tsformat :: String
tsformat = "%Y%m%d%H%M%S"

-- get file path to a log file with current time
nameLogFile :: FilePath -> IO FilePath
nameLogFile filename = do
    now <- getCurrentTime
    let tsnow = formatTime defaultTimeLocale tsformat now
    return $ filename ++ "-" ++ tsnow

\end{code}

\subsubsection{Open a new log file}\label{code:evalRotator}\index{evalRotator}
\begin{code}
evalRotator :: RotationParameters -> FilePath -> IO (Handle, Integer, UTCTime)
evalRotator rotation filename = do
    let maxAge  = toInteger $ rpMaxAgeHours   rotation
        maxSize = toInteger $ rpLogLimitBytes rotation

    -- open new log file
    fpath <- nameLogFile filename
    hdl <- catchIO (openFile fpath WriteMode) $
               \e -> do
                   prtoutException ("error while opening log: " ++ fpath) e
                   return stdout    -- fallback to standard output in case of exception
    hSetBuffering hdl LineBuffering

#ifdef POSIX
    -- restrict symbolic links only for unix-like OS
    let symLinkPath = filename
    let logfilePath = takeFileName fpath
    -- delete a symlink if already exists and create a new
    -- one that points to the correct file.
    (removeFile symLinkPath)
        `catchIO` (prtoutException ("cannot remove symlink: " ++ symLinkPath))
    (createFileLink logfilePath symLinkPath)
        `catchIO` (prtoutException ("cannot create symlink: " ++ symLinkPath))
#endif

    -- compute next rotation time
    now <- getCurrentTime
    let rottime = addUTCTime (fromInteger $ maxAge * 3600) now

    return (hdl, maxSize, rottime)

-- list filenames in prefix dir which match 'file'
listLogFiles :: FilePath -> IO (Maybe (NonEmpty FilePath))
listLogFiles file = do
    -- find files in the same directory which begin with
    -- the same name
    let directoryPath = takeDirectory file

    files <- listDirectory directoryPath
    return $ NE.nonEmpty $ map (directoryPath </> ) $ sort $ filter fpredicate files
  where
    tslen = 14  -- length of a timestamp
    filename = takeFileName  file
    fplen = length filename
    fpredicate path = take fplen path == filename
                      && take 1 (drop fplen path) == "-"
                      && length (drop (fplen + 1) path) == tslen

-- latest log file which matches 'filename'
latestLogFile :: FilePath -> IO (Maybe FilePath)
latestLogFile filename =
    listLogFiles filename >>= \fs -> return $ latestLogFile' fs
  where
    latestLogFile' :: Maybe (NonEmpty FilePath) -> Maybe FilePath
    latestLogFile' Nothing      = Nothing
    latestLogFile' (Just flist) = Just $ NE.last flist

-- initialize log file at startup
-- may append to existing file
initializeRotator :: RotationParameters -> FilePath -> IO (Handle, Integer, UTCTime)
initializeRotator rotation filename = do
    let maxAge  = toInteger $ rpMaxAgeHours   rotation
        maxSize = toInteger $ rpLogLimitBytes rotation

    latest <- latestLogFile filename
    case latest of
        Nothing -> -- no file to append, return new
            evalRotator rotation filename
        Just fname -> do
            -- check date
            now <- getCurrentTime
            tsfp <- parseTimeM True defaultTimeLocale tsformat $ drop (fplen + 1) fname
            if (round $ diffUTCTime now tsfp) > (3600 * maxAge)
               then do  -- file is too old, return new
                  evalRotator rotation filename
               else do
                  hdl <- catchIO (openFile fname AppendMode) $
                             \e -> do
                                 prtoutException fname e
                                 return stdout    -- fallback to standard output in case of exception
                  hSetBuffering hdl LineBuffering
                  cursize <- hFileSize hdl
                  let rotationTime = addUTCTime (fromInteger $ maxAge * 3600) tsfp
                  return (hdl, (maxSize - cursize), rotationTime)
  where
    fplen = length filename

-- remove old files; count them and only keep n (from config)
cleanupRotator :: RotationParameters -> FilePath -> IO ()
cleanupRotator rotation filename = do
    let keepN0 = fromIntegral (rpKeepFilesNum rotation) :: Int
        keepN = max 1 $ min keepN0 99
    listLogFiles filename >>= removeOldFiles keepN
  where
    removeOldFiles :: Int -> Maybe (NonEmpty FilePath) -> IO ()
    removeOldFiles _ Nothing = return ()
    removeOldFiles n (Just flist) =
        mapM_ removeFile $ reverse $ NE.drop n $ NE.reverse flist

-- display message and stack trace of exception on stdout
prtoutException :: Exception e => String -> e -> IO ()
prtoutException msg e = do
    putStrLn msg
    putStrLn ("exception: " ++ displayException e)

\end{code}
