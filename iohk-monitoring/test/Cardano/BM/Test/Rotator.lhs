
\subsection{Rotator}
\label{code:Cardano.BM.Test.Rotator}

%if style == newcode
\begin{code}
{-# LANGUAGE CPP        #-}

#if !defined(mingw32_HOST_OS)
#define POSIX
#endif

module Cardano.BM.Test.Rotator (
    tests
  ) where

import           Prelude hiding (lookup)

#ifdef POSIX
import           Control.Monad (forM_, replicateM)
import           Data.List (groupBy, intercalate, sort)
import qualified Data.List.NonEmpty as NE
import           Data.List.Split (splitOn)
import           Data.Time (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           System.Directory (createDirectoryIfMissing, getTemporaryDirectory,
                     removeDirectoryRecursive)
import           System.IO (IOMode (WriteMode), openFile)
import           System.FilePath ((</>), takeDirectory)
#endif

#ifdef POSIX
import           Cardano.BM.Rotator (nameLogFile, cleanupRotator, listLogFiles, tsformat)
import           Cardano.BM.Data.Rotation (RotationParameters (..))
#else
import           Cardano.BM.Rotator (nameLogFile)
#endif

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (Property, testProperty)
import           Test.QuickCheck ((===))
import           Test.QuickCheck.Property (ioProperty)
#ifdef POSIX
import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Arbitrary (Arbitrary)
#endif

\end{code}
%endif

\begin{code}
tests :: TestTree
tests = testGroup "testing Trace" [
        property_tests
      ]

property_tests :: TestTree
property_tests = testGroup "Property tests" [
        testProperty "rotator: file naming" propNaming
#ifdef POSIX
      , testProperty "rotator: cleanup" $ propCleanup $ rot n
#endif
      ]
#ifdef POSIX
  where
    n = 5
    rot num = RotationParameters
              { rpLogLimitBytes = 10000000 -- 10 MB
              , rpMaxAgeHours   = 24
              , rpKeepFilesNum  = num
              }
#endif
\end{code}

\subsubsection{Check that the generated file name has only 15 digits added to the base name.}\label{code:propNaming}
\begin{code}
propNaming :: FilePath -> Property
propNaming name = ioProperty $ do
    filename <- nameLogFile name
    return $ length filename === length name + 15

\end{code}

\subsubsection{Test cleanup of rotator.}\label{code:propCleanup}
This test creates a random number of files with the same name but with different dates and
afterwards it calls the |cleanupRotator| function which removes old log files keeping only
|rpKeepFilesNum| files and deleting the others.
\begin{code}
#ifdef POSIX
data LocalFilePath = Dir FilePath
    deriving (Show)

instance Arbitrary LocalFilePath where
    arbitrary = do
        start <- QC.sized $ \n -> replicateM (n+1) (QC.elements $ ['a'..'z'])
        x     <- QC.sized $ \n -> replicateM n     (QC.elements $ ['a'..'d'] ++ "/")
        pure $ Dir $ start ++ removeAdjacentAndLastSlashes x

    shrink (Dir path) = map (Dir . removeAdjacentAndLastSlashes . (intercalate "/")) $
        product' $ map (filter (/= "")) $ map QC.shrink (splitOn "/" path)
      where
        product' :: [[a]] -> [[a]]
        product' = mapM (\x -> x >>= return)

removeAdjacentAndLastSlashes :: FilePath -> FilePath
removeAdjacentAndLastSlashes = concat . filter (/= "/") . groupBy (\_ b -> b /= '/')

data SmallAndLargeInt = SL Int
    deriving (Show)

instance Arbitrary SmallAndLargeInt where
    arbitrary = do
        QC.oneof [ smallGen
                 , largeGen
                 ]
      where
        smallGen :: QC.Gen SmallAndLargeInt
        smallGen = do
            QC.Small x <- (QC.arbitrary :: QC.Gen (QC.Small Int))
            pure $ SL $ abs x
        largeGen :: QC.Gen SmallAndLargeInt
        largeGen = do
            let maxBoundary = 00100000000000 -- 10 years for the format which is used
                minBoundary = 00000000010000 -- 1  hour   for the format which is used
            x <- QC.choose (minBoundary, maxBoundary)
            pure $ SL x

    shrink _ = []

data NumFiles = NF Int deriving (Show)
instance Arbitrary NumFiles where
    arbitrary = QC.oneof [ return (NF 0), return (NF 1), return (NF 5), return (NF 7)]

propCleanup :: RotationParameters -> LocalFilePath -> NumFiles -> SmallAndLargeInt -> Property
propCleanup rotationParams (Dir filename) (NF nFiles) (SL maxDev) = QC.withMaxSuccess 20 $ ioProperty $ do
    tmpDir0 <- getTemporaryDirectory
    let tmpDir = tmpDir0 </> "rotatorTest.base"
    let path = tmpDir </> filename
    -- generate nFiles different dates
    now <- getCurrentTime
    let tsnow = formatTime defaultTimeLocale tsformat now
    deviations <- replicateM nFiles $ QC.generate $ QC.choose (1, maxDev + 1)
    --TODO if generated within the same sec we have a problem
    let dates = map show $ scanl (+) (read tsnow) deviations
        files = map (\a -> path ++ ('-' : a)) dates
        sortedFiles = reverse $ sort files
        keepFilesNum = fromIntegral $ rpKeepFilesNum rotationParams
        toBeKept = reverse $ take keepFilesNum sortedFiles

    createDirectoryIfMissing True $ takeDirectory path
    forM_ (files) $ \f -> openFile f WriteMode

    cleanupRotator rotationParams path

    filesRemained <- listLogFiles path
    let kept = case filesRemained of
            Nothing -> []
            Just l  -> NE.toList l

    removeDirectoryRecursive tmpDir

    return $ kept === toBeKept
#endif
\end{code}
