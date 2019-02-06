
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
import           System.Directory (createDirectoryIfMissing, removeFile,
                     removePathForcibly)
import           System.IO (IOMode (WriteMode), openFile)
import           System.FilePath ((</>), takeDirectory)

import           Cardano.BM.Rotator (nameLogFile, cleanupRotator, listLogFiles)
import           Cardano.BM.Data.Rotation (RotationParameters (..))

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (Property, testProperty)
import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Arbitrary (Arbitrary)
import           Test.QuickCheck.Modifiers (Small (..))
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

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

\subsubsection{Check that name giver only adds 15 digits to the name of the file.}\label{code:prop_name_giving}
\begin{code}
prop_name_giving :: FilePath -> Property
prop_name_giving name = monadicIO $ do
    filename <- run $ nameLogFile name
    assert $ length filename == length name + 15

\end{code}

\subsubsection{Check cleanup of rotator.}\label{code:prop_cleanup}
\begin{code}

data LocalFilePath = Dir FilePath
    deriving (Show)

instance Arbitrary LocalFilePath where
    arbitrary = do
        start <- QC.sized $ \n -> replicateM n (QC.elements $ ['a'..'z'])
        x <- QC.sized $ \n -> replicateM n (QC.elements $ ['a'..'z'] ++ "/")
        pure $ Dir ("./test-rot" </> (start ++ x))

    shrink _ = []

prop_cleanup :: RotationParameters -> LocalFilePath -> Small Int -> Property
prop_cleanup rotationParams (Dir path) (Small n) = monadicIO $ do
    dates <- run $ replicateM (abs n) $ QC.generate $ replicateM 14 (QC.elements ['0'..'9'])
    let files = map (\a -> path ++ ('-' : a)) dates

    run $ createDirectoryIfMissing True $ takeDirectory path
    run $ forM_ (files) $ \f -> openFile f WriteMode

    run $ cleanupRotator rotationParams path

    filesRemained <- run $ listLogFiles path
    let keeped = case filesRemained of
            Nothing -> []
            Just l  -> NE.toList l
    -- delete the files
    run $ forM_ keeped removeFile
    run $ removePathForcibly "./test-rot"
    assert $ rpKeepFilesNum rotationParams >= fromIntegral (length keeped)

\end{code}
