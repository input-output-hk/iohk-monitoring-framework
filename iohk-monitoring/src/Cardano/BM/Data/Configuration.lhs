
\subsection{Cardano.BM.Data.Configuration}
\label{code:Cardano.BM.Data.Configuration}

Data structure to help parsing configuration files.

%if style == newcode
\begin{code}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}

module Cardano.BM.Data.Configuration
  (
    Representation (..)
  , Port
  , parseRepresentation
  )
  where

import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import qualified Data.Set as Set
import           Data.Yaml
import           GHC.Generics

import           Cardano.BM.Data.BackendKind
import           Cardano.BM.Data.Output
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.Rotation

\end{code}
%endif

\subsubsection{Representation}\label{code:Representation}\index{Representation}\label{code:Port}\index{Port}
\begin{code}
type Port = Int
data Representation = Representation
    { minSeverity     :: Severity
    , rotation        :: Maybe RotationParameters
    , setupScribes    :: [ScribeDefinition]
    , defaultScribes  :: [(ScribeKind,Text)]
    , setupBackends   :: [BackendKind]
    , defaultBackends :: [BackendKind]
    , hasEKG          :: Maybe Port
    , hasGUI          :: Maybe Port
    , options         :: HM.HashMap Text Object
    }
    deriving (Generic, Show, ToJSON, FromJSON)

\end{code}

\subsubsection{parseRepresentation}\label{code:parseRepresentation}\index{parseRepresentation}
\begin{code}
parseRepresentation :: FilePath -> IO Representation
parseRepresentation fp = do
    repr :: Representation <- decodeFileThrow fp
    return $ implicit_fill_representation repr

\end{code}

after parsing the configuration representation we implicitly correct it.
\begin{code}
implicit_fill_representation :: Representation -> Representation
implicit_fill_representation =
    remove_ekgview_if_not_defined .
    filter_duplicates_from_backends .
    filter_duplicates_from_scribes .
    union_setup_and_usage_backends .
    add_ekgview_if_port_defined .
    add_katip_if_any_scribes
  where
    filter_duplicates_from_backends r =
        r {setupBackends = mkUniq $ setupBackends r}
    filter_duplicates_from_scribes r =
        r {setupScribes = mkUniq $ setupScribes r}
    union_setup_and_usage_backends r =
        r {setupBackends = setupBackends r <> defaultBackends r}
#ifdef ENABLE_EKG
    remove_ekgview_if_not_defined r =
        case hasEKG r of
        Nothing -> r { defaultBackends = filter (\bk -> bk /= EKGViewBK) (defaultBackends r)
                     , setupBackends = filter (\bk -> bk /= EKGViewBK) (setupBackends r)
                     }
        Just _  -> r
    add_ekgview_if_port_defined r =
        case hasEKG r of
        Nothing -> r
        Just _  -> r {setupBackends = setupBackends r <> [EKGViewBK]}
#else
    remove_ekgview_if_not_defined = id
    add_ekgview_if_port_defined = id
#endif
    add_katip_if_any_scribes r =
        if (any not [null $ setupScribes r, null $ defaultScribes r])
        then r {setupBackends = setupBackends r <> [KatipBK]}
        else r
    mkUniq :: Ord a => [a] -> [a]
    mkUniq = Set.toList . Set.fromList

\end{code}
