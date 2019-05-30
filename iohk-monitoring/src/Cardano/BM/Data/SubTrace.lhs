
\subsection{Cardano.BM.Data.SubTrace}
\label{code:Cardano.BM.Data.SubTrace}

%if style == newcode
\begin{code}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.BM.Data.SubTrace
  (
    SubTrace (..)
  , DropName (..), UnhideNames (..)
  , NameSelector (..)
  )
  where

import           Data.Aeson (FromJSON (..), ToJSON (..), Value (..), (.:), (.=), object, withObject)
import           Data.Text (Text, unpack)

import           Cardano.BM.Data.LogItem (LoggerName)
import           Cardano.BM.Data.Observable
import           Cardano.BM.Data.Severity (Severity (..))

import           GHC.Generics (Generic)

\end{code}
%endif

\subsubsection{SubTrace}\label{code:SubTrace}\index{SubTrace}
\label{code:Neutral}\index{SubTrace!Neutral}
\label{code:UntimedTrace}\index{SubTrace!UntimedTrace}
\label{code:NoTrace}\index{SubTrace!NoTrace}
\label{code:TeeTrace}\index{SubTrace!TeeTrace}
\label{code:FilterTrace}\index{SubTrace!FilterTrace}
\label{code:DropOpening}\index{SubTrace!DropOpening}
\label{code:ObservableTrace}\index{SubTrace!ObservableTrace}
\label{code:SetSeverity}\index{SubTrace!SetSeverity}
\label{code:NameOperator}\index{SubTrace!FilterTrace!NameOperator}
\label{code:NameSelector}\index{SubTrace!FilterTrace!NameSelector}
\begin{code}
data NameSelector = Exact Text | StartsWith Text | EndsWith Text | Contains Text
                    deriving (Generic, Show, FromJSON, ToJSON, Read, Eq)
data DropName     = Drop NameSelector
                    deriving (Generic, Show, FromJSON, ToJSON, Read, Eq)
data UnhideNames  = Unhide [NameSelector]
                    deriving (Generic, Show, FromJSON, ToJSON, Read, Eq)

data SubTrace = Neutral
              | UntimedTrace
              | NoTrace
              | TeeTrace LoggerName
              | FilterTrace [(DropName, UnhideNames)]
              | DropOpening
              | ObservableTrace [ObservableInstance]
              | SetSeverity Severity
                deriving (Generic, Show, Read, Eq)

instance FromJSON SubTrace where
    parseJSON = withObject "" $ \o -> do
                    subtrace :: Text <- o .: "subtrace"
                    case subtrace of
                        "Neutral"         -> return $ Neutral
                        "UntimedTrace"    -> return $ UntimedTrace
                        "NoTrace"         -> return $ NoTrace
                        "TeeTrace"        -> TeeTrace        <$> o .: "contents"
                        "FilterTrace"     -> FilterTrace     <$> o .: "contents"
                        "DropOpening"     -> return $ DropOpening
                        "ObservableTrace" -> ObservableTrace <$> o .: "contents"
                        "SetSeverity"     -> SetSeverity     <$> o .: "contents"
                        other             -> fail $ "unexpected subtrace: " ++ (unpack other)

instance ToJSON SubTrace where
    toJSON Neutral              = object ["subtrace" .= String "Neutral"         ]
    toJSON UntimedTrace         = object ["subtrace" .= String "UntimedTrace"    ]
    toJSON NoTrace              = object ["subtrace" .= String "NoTrace"         ]
    toJSON (TeeTrace name)      = object ["subtrace" .= String "TeeTrace"        , "contents" .= toJSON name]
    toJSON (FilterTrace dus)    = object ["subtrace" .= String "FilterTrace"     , "contents" .= toJSON dus ]
    toJSON DropOpening          = object ["subtrace" .= String "DropOpening"     ]
    toJSON (ObservableTrace os) = object ["subtrace" .= String "ObservableTrace" , "contents" .= toJSON os  ]
    toJSON (SetSeverity sev)    = object ["subtrace" .= String "SetSeverity"     , "contents" .= toJSON sev ]

\end{code}
