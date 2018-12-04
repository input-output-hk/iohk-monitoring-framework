
\subsection{Cardano.BM.Data.Output}

%if style == newcode
\begin{code}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Cardano.BM.Data.Output
  ( 
    OutputKind (..)
  , ScribeKind (..)
  )
  where

import qualified Control.Concurrent.STM.TVar as STM
import           Data.Aeson (FromJSON (..), ToJSON)

import           Cardano.BM.Data.LogItem

import           GHC.Generics (Generic)

\end{code}
%endif

\subsubsection{OutputKind}\label{code:OutputKind}
\begin{code}
data OutputKind = StdOut
                | TVarList (STM.TVar [LogObject])
                | TVarListNamed (STM.TVar [LogNamed LogObject])
                | Null
                deriving (Eq)

\end{code}

\subsubsection{ScribeKind}\label{code:ScribeKind}
This identifies katip's scribes by type.
\begin{code}
data ScribeKind = FileTextSK
                | FileJsonSK
                | StdoutSK
                | StderrSK
                | DevNullSK
                deriving (Generic, Eq, Show, FromJSON, ToJSON)

\end{code}
