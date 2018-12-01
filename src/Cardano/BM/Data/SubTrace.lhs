
\subsection{Cardano.BM.Data.SubTrace}

%if False
\begin{code}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Cardano.BM.Data.SubTrace
  ( 
    SubTrace (..)
  )
  where

import           Data.Aeson (FromJSON (..), ToJSON)
import           Data.Set (Set)

import           Cardano.BM.Data.Observable

import           GHC.Generics (Generic)

\end{code}
%endif

\subsubsection{SubTrace}\label{code:SubTrace}
\begin{code}
data SubTrace = Neutral
              | UntimedTrace
              | NoTrace
              | DropOpening
              | ObservableTrace (Set ObservableInstance)
                deriving (Generic, Show, FromJSON, ToJSON)
\end{code}
