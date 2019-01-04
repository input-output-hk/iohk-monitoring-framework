
\subsection{Cardano.BM.Data.SubTrace}

%if style == newcode
\begin{code}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Cardano.BM.Data.SubTrace
  (
    SubTrace (..)
  )
  where

import           Data.Aeson (FromJSON (..), ToJSON)

import           Cardano.BM.Data.Observable

import           GHC.Generics (Generic)

\end{code}
%endif

\subsubsection{SubTrace}\label{code:SubTrace}\index{SubTrace}
\label{code:Neutral}\index{SubTrace!Neutral}
\label{code:UntimedTrace}\index{SubTrace!UntimedTrace}
\label{code:NoTrace}\index{SubTrace!NoTrace}
\label{code:DropOpening}\index{SubTrace!DropOpening}
\label{code:ObservableTrace}\index{SubTrace!ObservableTrace}
\begin{code}
data SubTrace = Neutral
              | UntimedTrace
              | NoTrace
              | DropOpening
              | ObservableTrace [ObservableInstance]
                deriving (Generic, Show, FromJSON, ToJSON, Read)

\end{code}
