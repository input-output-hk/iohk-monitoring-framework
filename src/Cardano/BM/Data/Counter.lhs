
\subsection{Cardano.BM.Data.Counter}

%if style == newcode
\begin{code}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.BM.Data.Counter
  ( Counter (..)
  , CounterState (..)
  )
  where

import           Data.Aeson (ToJSON, toEncoding, toJSON)
import           Data.Text (Text)
import           Data.Time.Units (Microsecond, toMicroseconds)
import           Data.Unique (Unique, hashUnique)

import           GHC.Generics (Generic)

\end{code}
%endif


\subsubsection{Counter}\label{code:Counter}
\begin{code}
data Counter = MonotonicClockTime Text Microsecond
             | MemoryCounter Text Integer
             | StatInfo Text Integer
             | IOCounter Text Integer
             | CpuCounter Text Integer
               deriving (Eq, Show, Generic, ToJSON)

instance ToJSON Microsecond where
    toJSON     = toJSON     . toMicroseconds
    toEncoding = toEncoding . toMicroseconds

\end{code}

\subsubsection{CounterState}\label{code:CounterState}
\begin{code}
data CounterState = CounterState {
      csIdentifier :: Unique
    , csCounters   :: [Counter]
    }
    deriving (Generic, ToJSON)

instance ToJSON Unique where
    toJSON     = toJSON     . hashUnique
    toEncoding = toEncoding . hashUnique

instance Show CounterState where
    show cs = (show . hashUnique) (csIdentifier cs)
           <> " => " <> (show $ csCounters cs)

\end{code}
