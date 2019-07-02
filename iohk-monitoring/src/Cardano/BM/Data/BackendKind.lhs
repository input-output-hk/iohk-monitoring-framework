
\subsection{Cardano.BM.Data.BackendKind}
\label{code:Cardano.BM.Data.BackendKind}

%if style == newcode
\begin{code}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}

module Cardano.BM.Data.BackendKind
  ( BackendKind (..)
  )
  where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Text (Text)
import           GHC.Generics (Generic)

\end{code}
%endif

\subsubsection{BackendKind}\label{code:BackendKind}\index{BackendKind}
\label{code:AggregationBK}\label{code:EditorBK}\label{code:EKGViewBK}\label{code:KatipBK}
\label{code:LogBufferBK}\label{code:MonitoringBK}\label{code:TraceAcceptorBK}
\label{code:SwitchboardBK}\label{code:GraylogBK}\label{code:TraceForwarderBK}\label{code:UserDefinedBK}
\index{BackendKind!AggregationBK}\index{BackendKind!EKGViewBK}\index{BackendKind!KatipBK}
\index{BackendKind!LogBufferBK}\index{BackendKind!MonitoringBK}\index{BackendKind!EditorBK}
\index{BackendKind!TraceAcceptorBK}\index{BackendKind!TraceForwarderBK}
\index{BackendKind!SwitchboardBK}\index{BackendKind!GraylogBK}\index{BackendKind!UserDefinedBK}
This identifies the backends that can be attached to the |Switchboard|.
\begin{code}

data BackendKind =
      AggregationBK
    | EditorBK
    | EKGViewBK
    | GraylogBK
    | KatipBK
    | LogBufferBK
    | MonitoringBK
    | TraceAcceptorBK
    | TraceForwarderBK
    | UserDefinedBK Text
    | SwitchboardBK
    deriving (Generic, Eq, Ord, Show, ToJSON, FromJSON, Read)

\end{code}
