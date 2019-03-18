
\subsection{Cardano.BM.Data.Configuration}

Data structure to help parsing configuration files.

%if style == newcode
\begin{code}
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
  -- * tests
  , test_log_backend_1
  , test_log_backend_2
  , test_conf_representation_1
  , test_conf_representation_2
  )
  where

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import qualified Data.Set as Set
import           Data.Yaml
import           GHC.Generics

import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.Output
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.Rotation

\end{code}
%endif

\subsubsection{Representation}\label{code:Representation}
\begin{code}
type Port = Int
data Representation = Representation
    { minSeverity     :: Severity
    , rotation        :: RotationParameters
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

\subsubsection{parseRepresentation}\label{code:parseRepresentation}
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
    add_ekgview_if_port_defined r =
        case hasEKG r of
        Nothing -> r
        Just _  -> r {setupBackends = setupBackends r <> [EKGViewBK]}
    add_katip_if_any_scribes r =
        if (any (not) [null $ setupScribes r, null $ defaultScribes r])
        then r {setupBackends = setupBackends r <> [KatipBK]}
        else r
    mkUniq :: Ord a => [a] -> [a]
    mkUniq = Set.toList . Set.fromList

\end{code}

% only visible in Haskell
%if style == newcode
\begin{code}
test_log_backend_1 :: IO ()
test_log_backend_1 =
    BS.putStrLn $
    encode $ ScribeDefinition { scKind = StdoutSK
                        , scName = "testlog"
                        , scRotation = Nothing
                        }

test_log_backend_2 :: IO ()
test_log_backend_2 =
    BS.putStrLn $
    encode $ ScribeDefinition { scKind = StdoutSK
                        , scName = "testlog"
                        , scRotation = Just $ RotationParameters 5000000 24 10
                        }

test_conf_representation_1 :: IO ()
test_conf_representation_1 =
    BS.putStrLn $
    encode $ Representation
        { minSeverity = Info
        , rotation = RotationParameters 5000000 24 10
        , setupScribes =
            [ ScribeDefinition { scName = "stdout"
                            , scKind = StdoutSK
                            , scRotation = Nothing }
            ]
        , defaultScribes = [(StdoutSK, "stdout")]
        , setupBackends = [ EKGViewBK, KatipBK ]
        , defaultBackends = [ KatipBK ]
        , hasGUI = Just 12789
        , hasEKG = Just 18321
        , options =
            HM.fromList [ ("test1", (HM.singleton "value" "object1"))
                        , ("test2", (HM.singleton "value" "object2")) ]
    }

test_conf_representation_2 :: FilePath -> IO ()
test_conf_representation_2 fp = do
    repr <- parseRepresentation fp
    BS.putStrLn $ encode repr

\end{code}
%endif
