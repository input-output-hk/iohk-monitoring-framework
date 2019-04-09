
\subsection{Cardano.BM.Data.Output}
\label{code:Cardano.BM.Data.Output}

%if style == newcode
\begin{code}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Cardano.BM.Data.Output
  (
    ScribeKind (..)
  , ScribeId
  , ScribePrivacy (..)
  , ScribeDefinition (..)
  )
  where

import           Data.Aeson (FromJSON (..), ToJSON, Value (..), parseJSON, (.:),
                     (.:?))
import           Data.Aeson.Types (typeMismatch)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)

import           Cardano.BM.Data.Rotation (RotationParameters)

import           GHC.Generics (Generic)

\end{code}
%endif

\subsubsection{ScribeKind}\label{code:ScribeKind}
\label{code:FileTextSK}\index{ScribeKind!FileTextSK}
\label{code:FileJsonSK}\index{ScribeKind!FileJsonSK}
\label{code:StdoutSK}\index{ScribeKind!StdoutSK}
\label{code:StderrSK}\index{ScribeKind!StderrSK}
\label{code:DevNullSK}\index{ScribeKind!DevNullSK}
This identifies katip's scribes by type.
\begin{code}
data ScribeKind = FileTextSK
                | FileJsonSK
                | StdoutSK
                | StderrSK
                | DevNullSK
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
