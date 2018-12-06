
\subsection{Cardano.BM.Data.Observable}

%if style == newcode
\begin{code}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Cardano.BM.Data.Observable
  (
    ObservableInstance (..)
  )
  where

import           Data.Aeson (FromJSON (..), ToJSON)

import           GHC.Generics (Generic)

\end{code}
%endif

\subsubsection{ObservableInstance}\label{code:ObservableInstance}
\begin{code}
data ObservableInstance = MonotonicClock
                        | MemoryStats
                        | ProcessStats
                        | IOStats
                          deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON)

\end{code}
