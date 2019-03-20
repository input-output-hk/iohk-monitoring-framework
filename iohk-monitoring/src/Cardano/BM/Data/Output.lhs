
\subsection{Cardano.BM.Data.Output}
\label{code:Cardano.BM.Data.Output}

%if style == newcode
\begin{code}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Cardano.BM.Data.Output
  (
    OutputKind (..)
  , ScribeKind (..)
  , ScribeId
  , ScribePrivacy (..)
  , ScribeDefinition (..)
  )
  where

import qualified Control.Concurrent.STM.TVar as STM
import           Data.Aeson (FromJSON (..), ToJSON, Value (..), parseJSON, (.:),
                     (.:?))
import           Data.Aeson.Types (typeMismatch)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)

import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Rotation

import           GHC.Generics (Generic)

\end{code}
%endif

\subsubsection{OutputKind}\label{code:OutputKind}\index{OutputKind}
\label{code:TVarList}\index{OutputKind!TVarList}
\begin{code}
data OutputKind a = TVarList (STM.TVar [LogObject a])
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
                deriving (Generic, Eq, Ord, Show, Read, FromJSON, ToJSON)

\end{code}

\subsubsection{ScribeId}\label{code:ScribeId}\index{ScribeId}
A scribe is identified by |ScribeKind x Filename|
\begin{code}
type ScribeId = Text -- (ScribeKind :: Filename)

\end{code}

\subsubsection{ScribePrivacy}\label{code:ScribePrivacy}\index{ScribePrivacy}
\label{code:ScPublic}\index{ScribePrivacy!ScPublic}
\label{code:ScPrivate}\index{ScribePrivacy!ScPrivate}
This declares if a scribe will be public (and must not contain sensitive data) or
private.
\begin{code}

data ScribePrivacy = ScPublic | ScPrivate
    deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON)

\end{code}

\subsubsection{ScribeDefinition}\label{code:ScribeDefinition}\index{ScribeDefinition}
\label{code:scKind}\index{ScribeDefinition!scKind}
\label{code:scName}\index{ScribeDefinition!scName}
\label{code:scPrivacy}\index{ScribeDefinition!scPrivacy}
\label{code:scRotation}\index{ScribeDefinition!scRotation}
This identifies katip's scribes by type.
\begin{code}
data ScribeDefinition = ScribeDefinition
    { scKind     :: ScribeKind
    , scName     :: Text
    , scPrivacy  :: ScribePrivacy
    , scRotation :: Maybe RotationParameters
    }
    deriving (Generic, Eq, Ord, Show, ToJSON)

instance FromJSON ScribeDefinition where
    parseJSON (Object o) = do
        kind       <- o .:  "scKind"
        name       <- o .:  "scName"
        mayPrivacy <- o .:? "scPrivacy"
        rotation   <- o .:? "scRotation"
        return $ ScribeDefinition
                    { scKind     = kind
                    , scName     = name
                    , scPrivacy  = fromMaybe ScPublic mayPrivacy
                    , scRotation = rotation
                    }
    parseJSON invalid = typeMismatch "ScribeDefinition" invalid

\end{code}
