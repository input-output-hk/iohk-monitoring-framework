
\subsection{Cardano.BM.Data.Severity}

%if style == newcode
\begin{code}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}

module Cardano.BM.Data.Severity
  ( Severity (..)
  )
  where

import           Data.Aeson (FromJSON (..), ToJSON)
import           Data.Yaml (withText)

import           GHC.Generics (Generic)

\end{code}
%endif

\subsubsection{Severity}\label{code:Severity}
The intended meaning of severity codes:

Debug     | detailled information about values and decision flow
Info      | general information of events; progressing properly
Notice    | needs attention; something not progressing properly
Warning   | may continue into an error condition if continued
Error     | unexpected set of event or condition occured
Critical  | error condition causing degrade of operation
Alert     | a subsystem is no longer operating correctly, likely requires manual intervention
Emergency | at this point, the system can never progress without additional intervention

We were informed by the |Syslog| taxonomy: \url{https://en.wikipedia.org/wiki/Syslog#Severity_level}

\begin{code}
data Severity = Debug
              | Info
              | Notice
              | Warning
              | Error
              | Critical
              | Alert
              | Emergency
                deriving (Show, Eq, Ord, Generic, ToJSON, Read)

instance FromJSON Severity where
    parseJSON = withText "severity" $ \case
                    "Debug"     -> pure Debug
                    "Info"      -> pure Info
                    "Notice"    -> pure Notice
                    "Warning"   -> pure Warning
                    "Error"     -> pure Error
                    "Critical"  -> pure Critical
                    "Alert"     -> pure Alert
                    "Emergency" -> pure Emergency
                    _           -> pure Info   -- catch all

\end{code}
