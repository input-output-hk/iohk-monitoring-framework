
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

\subsubsection{BackendKind}\label{code:BackendKind}\index{BackendKind}
\label{code:AggregationBK}\label{code:EKGViewBK}\label{code:KatipBK}\label{code:SwitchboardBK}
\index{BackendKind!AggregationBK}\index{BackendKind!EKGViewBK}\index{BackendKind!KatipBK}\index{BackendKind!MonitoringBK}\index{BackendKind!SwitchboardBK}
This identifies the backends that can be attached to the |Switchboard|.
\begin{code}
data BackendKind = AggregationBK
                 | EKGViewBK
                 | KatipBK
                 | MonitoringBK
                 | SwitchboardBK
                 deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON, Read)

\end{code}
