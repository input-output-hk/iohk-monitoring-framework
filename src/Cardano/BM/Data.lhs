
\subsection{Cardano.BM.Data}

%if False
\begin{code}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.BM.Data
  (
  )
  where

import qualified Control.Concurrent.STM.TVar as STM

import           Control.Concurrent.MVar (MVar)
--import           Control.Monad.IO.Class (MonadIO)

import           Data.Aeson (FromJSON (..), ToJSON, toEncoding, toJSON)
import           Data.Map (Map)
import           Data.Set (Set)
import           Data.Text (Text)
import           Data.Time.Units (Microsecond, toMicroseconds)
import           Data.Unique (Unique, hashUnique)
import           Data.Yaml (withText)
import           GHC.Generics (Generic)

import           Cardano.BM.BaseTrace

\end{code}
%endif

