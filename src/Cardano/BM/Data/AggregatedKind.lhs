
\subsection{Cardano.BM.Data.AggregatedKind}

%if style == newcode
\begin{code}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}

module Cardano.BM.Data.AggregatedKind
  ( AggregatedKind (..)
  )
  where

import           Data.Aeson (FromJSON, ToJSON)
import           GHC.Generics (Generic)

\end{code}
%endif

\subsubsection{AggregatedKind}\label{code:AggregatedKind}\index{AggregatedKind}
\label{StatsAK}\index{AggregatedKind!StatsAK}
\label{EwmaAK}\index{AggregatedKind!EwmaAK}
This identifies the type of Aggregated.
\begin{code}
data AggregatedKind = StatsAK
                    | EwmaAK
                        deriving (Generic, Eq, Show, FromJSON, ToJSON, Read)

\end{code}
