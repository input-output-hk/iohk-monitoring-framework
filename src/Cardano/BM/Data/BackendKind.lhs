
\subsection{Cardano.BM.Data.BackendKind}

%if style == newcode
\begin{code}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}

module Cardano.BM.Data.BackendKind
  ( BackendKind (..)
  )
  where

import           Data.Aeson (FromJSON, ToJSON)
import           GHC.Generics (Generic)

\end{code}
%endif

\subsubsection{BackendKind}\label{code:BackendKind}
This identifies the backends that can be attached to the \nameref{code:Switchboard}.
\begin{code}
data BackendKind = AggregationBK
                 | EKGViewBK
                 | KatipBK
                 | SwitchboardBK
                 deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON, Read)

\end{code}
