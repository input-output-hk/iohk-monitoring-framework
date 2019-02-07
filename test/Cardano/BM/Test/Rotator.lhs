
\subsection{Rotator}

%if style == newcode
\begin{code}
{-# LANGUAGE LambdaCase #-}

module Cardano.BM.Test.Rotator (
    tests
  ) where

import           Prelude hiding (lookup)

import           Control.Monad (forM_, replicateM)
import qualified Data.List.NonEmpty as NE
import           Data.Time (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           System.Directory (createDirectoryIfMissing, getTemporaryDirectory,
                     removeFile)
import           System.IO (IOMode (WriteMode), openFile)
import           System.FilePath ((</>), takeDirectory)

import           Cardano.BM.Rotator (nameLogFile, cleanupRotator, listLogFiles, tsformat)
import           Cardano.BM.Data.Rotation (RotationParameters (..))

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (Property, testProperty)
import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Arbitrary (Arbitrary)
import           Test.QuickCheck.Modifiers (Positive (..))
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

import           Test.QuickCheck.Property (ioProperty)

\end{code}
%endif

\begin{code}
tests :: TestTree
tests = testGroup "testing Trace" [
        property_tests
      ]

property_tests :: TestTree
property_tests = testGroup "Property tests" [
        testProperty "rotator: name giving" prop_name_giving
      , testProperty "rotator: cleanup" (prop_cleanup (rot n))
      ]
  where
    n = 5
    rot num = RotationParameters
              { rpLogLimitBytes = 10000000 -- 10 MB
              , rpMaxAgeHours   = 24
              , rpKeepFilesNum  = num
              }

\end{code}

\subsubsection{Check that full file name has only added 15 digits to the base name of the file.}\label{code:prop_name_giving}
\begin{code}
prop_name_giving :: FilePath -> Property
prop_name_giving name = monadicIO $ do
    filename <- run $ nameLogFile name
    assert $ length filename == length name + 15

\end{code}

\subsubsection{Test cleanup of rotator.}\label{code:prop_cleanup}
This test creates a random number of files with the same name but with different dates and
afterwards it calls the |cleanupRotator| function which removes old log files keeping only
|rpKeepFilesNum| files and deleting the others.
\begin{code}

data LocalFilePath = Dir FilePath
    deriving (Show)

instance Arbitrary LocalFilePath where
    arbitrary = do
        start <- QC.sized $ \n -> replicateM (n+1) (QC.elements $ ['a'..'z'])
        x     <- QC.sized $ \n -> replicateM n (QC.elements $ ['a'..'z'] ++ "/")
        pure $ Dir $ start ++ x

    shrink _ = []

prop_cleanup :: RotationParameters -> LocalFilePath -> Positive Int -> Positive Int -> Property
prop_cleanup rotationParams (Dir filename) (Positive nFiles) (Positive maxDev) = ioProperty $ do
    tmpDir <- getTemporaryDirectory
    let path = tmpDir </> filename
    -- generate nFiles different dates
    now <- getCurrentTime
    let tsnow = formatTime defaultTimeLocale tsformat now
    deviations <- replicateM nFiles $ QC.generate $ QC.choose (1, maxDev)
    let dates = map show $ scanl (+) (read tsnow) deviations
        files = map (\a -> path ++ ('-' : a)) dates

    createDirectoryIfMissing True $ takeDirectory path
    forM_ (files) $ \f -> openFile f WriteMode

    cleanupRotator rotationParams path

    filesRemained <- listLogFiles path
    let kept = case filesRemained of
            Nothing -> []
            Just l  -> NE.toList l
    -- delete the files
    forM_ kept removeFile
    return $ rpKeepFilesNum rotationParams >= fromIntegral (length kept)

\end{code}
