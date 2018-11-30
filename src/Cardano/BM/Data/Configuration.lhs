
\subsection{Cardano.BM.Data.Configuration

%if False
\begin{code}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Cardano.BM.Data.Configuration
  (
    Representation (..)
  , Port 
  , LogBackend (..)
  , RotationParameters (..)
  -- * tests
  , test_log_backend_1
  , test_log_backend_2
  )
  where

import           Data.Set (fromList)
import qualified Data.ByteString.Char8 as BS
import           Data.Yaml as Y
import           GHC.Generics
import           GHC.Word (Word64)

import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Observable
import           Cardano.BM.Data.Output
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.SubTrace

\end{code}
%endif

\subsubsection{Representation}\label{code:Representation}
\begin{code}
type Port = Int
data Representation = Representation
    {
      rotation :: RotationParameters
    , backends :: [LogBackend]
    , hasEKG :: Maybe Port
    , hasGUI :: Maybe Port
    }

\end{code}

\subsubsection{RotationParameters}\label{code:RotationParameters}
\begin{code}
data RotationParameters = RotationParameters
    { rpLogLimitBytes :: !Word64  -- ^ max size of file in bytes
    , rpMaxAgeHours   :: !Word    -- ^ hours
    , rpKeepFilesNum  :: !Word    -- ^ number of files to keep
    } deriving (Generic, Show, Eq, ToJSON)

instance FromJSON RotationParameters where
    parseJSON = withObject "rotation params" $ \o -> do
        rpLogLimitBytes  <- o .: "logLimit"
        rpMaxAgeHours    <- o .:? "maxAge" .!= 24
        rpKeepFilesNum   <- o .: "keepFiles"
        return RotationParameters{..}

\end{code}

\subsubsection{LogBackend}\label{code:LogBackend}
\begin{code}
data LogBackend = LogBackend
    { lbKind     :: ScribeKind
    , lbName     :: LoggerName
    , lbTrace    :: SubTrace
    , lbSeverity :: Severity
    } deriving (Generic, Show, FromJSON, ToJSON)

\end{code}

\begin{code}
test_log_backend_1 =
    BS.putStrLn $ 
    encode $ LogBackend { lbKind = StdoutSK
                        , lbName = "test 1"
                        , lbTrace = Neutral
                        , lbSeverity = Info
                        }

test_log_backend_2 =
    BS.putStrLn $ 
    encode $ LogBackend { lbKind = StdoutSK
                        , lbName = "test 1"
                        , lbTrace = ObservableTrace (fromList [ MonotonicClock
                                                              , MemoryStats
                                                              , ProcessStats ])
                        , lbSeverity = Info
                        }
\end{code}
