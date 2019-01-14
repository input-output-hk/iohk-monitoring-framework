
\subsection{Cardano.BM.Data.SubTrace}

%if style == newcode
\begin{code}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Cardano.BM.Data.SubTrace
  (
    SubTrace (..)
  , NameOperator (..)
  , NameSelector (..)
  )
  where

import           Data.Aeson (FromJSON (..), ToJSON)
import           Data.Text (Text)

import           Cardano.BM.Data.LogItem (LoggerName)
import           Cardano.BM.Data.Observable

import           GHC.Generics (Generic)

\end{code}
%endif

\subsubsection{SubTrace}\label{code:SubTrace}\index{SubTrace}
\label{code:Neutral}\index{SubTrace!Neutral}
\label{code:UntimedTrace}\index{SubTrace!UntimedTrace}
\label{code:NoTrace}\index{SubTrace!NoTrace}
\label{code:TeeTrace}\index{SubTrace!TeeTrace}
\label{code:FilterTrace}\index{SubTrace!FilterTrace}
\label{code:DropOpening}\index{SubTrace!DropOpening}
\label{code:ObservableTrace}\index{SubTrace!ObservableTrace}
\label{code:NameOperator}\index{SubTrace!FilterTrace!NameOperator}
\label{code:NameSelector}\index{SubTrace!FilterTrace!NameSelector}
\begin{code}
data NameSelector = Exact Text | StartsWith Text | EndsWith Text | Contains Text
                    deriving (Generic, Show, FromJSON, ToJSON, Read, Eq)
data NameOperator = Drop NameSelector | Unhide NameSelector
                    deriving (Generic, Show, FromJSON, ToJSON, Read, Eq)
data SubTrace = Neutral
              | UntimedTrace
              | NoTrace
              | TeeTrace LoggerName
              | FilterTrace [NameOperator]
              | DropOpening
              | ObservableTrace [ObservableInstance]
                deriving (Generic, Show, FromJSON, ToJSON, Read, Eq)

\end{code}
