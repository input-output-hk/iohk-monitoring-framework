
\subsection{Cardano.BM.Data.Rotation}

%if style == newcode
\begin{code}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}

module Cardano.BM.Data.Rotation
  ( RotationParameters (..)
  )
  where

import           Data.Aeson (FromJSON (..), ToJSON)

import           GHC.Generics (Generic)
import           GHC.Word (Word64)

\end{code}
%endif

\subsubsection{RotationParameters}\label{code:RotationParameters}
\begin{code}
data RotationParameters = RotationParameters
    { rpLogLimitBytes :: !Word64  -- max size of file in bytes
    , rpMaxAgeHours   :: !Word    -- hours
    , rpKeepFilesNum  :: !Word    -- number of files to keep
    } deriving (Generic, Show, Eq, Ord, FromJSON, ToJSON)

\end{code}
