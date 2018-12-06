
\subsection{Cardano.BM.Data.Severity}

%if style == newcode
\begin{code}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}

module Cardano.BM.Data.Severity
  ( Severity (..)
  )
  where

import           Data.Aeson (FromJSON (..), ToJSON)
import           Data.Yaml (withText)

import           GHC.Generics (Generic)

\end{code}
%endif

\subsubsection{Severity}\label{code:Severity}
\begin{code}
data Severity = Debug | Info | Warning | Notice | Error
                deriving (Show, Eq, Ord, Generic, ToJSON)

instance FromJSON Severity where
    parseJSON = withText "severity" $ \case
                    "Debug"    -> pure Debug
                    "Info"     -> pure Info
                    "Notice"   -> pure Notice
                    "Warning"  -> pure Warning
                    "Error"    -> pure Error
                    _          -> pure Info   -- catch all

\end{code}
