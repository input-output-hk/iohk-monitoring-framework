
\subsection{Cardano.BM.Data.Configuration}

%if style == newcode
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
  , test_conf_representation_1
  , test_conf_representation_2
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
    deriving (Generic, Show, ToJSON, FromJSON)

\end{code}

\subsubsection{RotationParameters}\label{code:RotationParameters}
\begin{code}
data RotationParameters = RotationParameters
    { rpLogLimitBytes :: !Word64  -- max size of file in bytes
    , rpMaxAgeHours   :: !Word    -- hours
    , rpKeepFilesNum  :: !Word    -- number of files to keep
    } deriving (Generic, Show, Eq, FromJSON, ToJSON)

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
test_log_backend_1 :: IO ()
test_log_backend_1 =
    BS.putStrLn $
    encode $ LogBackend { lbKind = StdoutSK
                        , lbName = "test 1"
                        , lbTrace = Neutral
                        , lbSeverity = Info
                        }

test_log_backend_2 :: IO ()
test_log_backend_2 =
    BS.putStrLn $
    encode $ LogBackend { lbKind = StdoutSK
                        , lbName = "test 2"
                        , lbTrace = ObservableTrace (fromList [ MonotonicClock
                                                              , MemoryStats
                                                              , ProcessStats ])
                        , lbSeverity = Info
                        }

test_conf_representation_1 :: IO ()
test_conf_representation_1 =
    BS.putStrLn $
    encode $ Representation
        (RotationParameters 5000000 24 10)
        [ LogBackend { lbKind = StdoutSK, lbName = "stdout"
                     , lbTrace = UntimedTrace, lbSeverity = Info }
        ]
        (Just 12789)
        (Just 18321)

test_conf_representation_2 :: FilePath -> IO Representation
test_conf_representation_2 fp =
    decodeFileThrow fp

\end{code}
