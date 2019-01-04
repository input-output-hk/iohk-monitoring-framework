
\subsection{Cardano.BM.Data.Output}

%if style == newcode
\begin{code}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Cardano.BM.Data.Output
  (
    OutputKind (..)
  , ScribeKind (..)
  , ScribeId
  , ScribeDefinition (..)
  )
  where

import qualified Control.Concurrent.STM.TVar as STM
import           Data.Aeson (FromJSON (..), ToJSON)
import           Data.Text (Text)

import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Rotation

import           GHC.Generics (Generic)

\end{code}
%endif

\subsubsection{OutputKind}\label{code:OutputKind}\index{OutputKind}
\label{code:TVarList}\index{OutputKind!TVarList}
\label{code:TVarListNamed}\index{OutputKind!TVarListNamed}
\begin{code}
data OutputKind = TVarList (STM.TVar [LogObject])
                | TVarListNamed (STM.TVar [LogNamed LogObject])
                deriving (Eq)

\end{code}

\subsubsection{ScribeKind}\label{code:ScribeKind}
\label{FileTextSK}\index{ScribeKind!FileTextSK}
\label{FileJsonSK}\index{ScribeKind!FileJsonSK}
\label{StdoutSK}\index{ScribeKind!StdoutSK}
\label{StderrSK}\index{ScribeKind!StderrSK}
This identifies katip's scribes by type.
\begin{code}
data ScribeKind = FileTextSK
                | FileJsonSK
                | StdoutSK
                | StderrSK
                deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON)

\end{code}

\subsubsection{ScribeId}\label{code:ScribeId}\index{ScribeId}
A scribe is identified by |ScribeKind x Filename|
\begin{code}
type ScribeId = Text -- (ScribeKind :: Filename)

\end{code}

\subsubsection{ScribeDefinition}\label{code:ScribeDefinition}\index{ScribeDefinition}
\label{code:scKind}\index{ScribeDefinition!scKind}
\label{code:scName}\index{ScribeDefinition!scName}
\label{code:scRotation}\index{ScribeDefinition!scRotation}
This identifies katip's scribes by type.
\begin{code}
data ScribeDefinition = ScribeDefinition
  { scKind     :: ScribeKind
  , scName     :: Text
  , scRotation :: Maybe RotationParameters
  }
  deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON)

\end{code}
