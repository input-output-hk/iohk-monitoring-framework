
\subsection{Cardano.BM.Configuration.Static}
\label{code:Cardano.BM.Configuration.Static}

%if style == newcode
\begin{code}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}

module Cardano.BM.Configuration.Static
    (
      defaultConfigStdout
    , defaultConfigTesting
    ) where

import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.BackendKind
import           Cardano.BM.Data.Output
import           Cardano.BM.Data.Severity

\end{code}
%endif

\subsubsection{Default configuration outputting on |stdout|}
\begin{code}
defaultConfigStdout :: IO CM.Configuration
defaultConfigStdout = do
    c <- CM.empty
    CM.setMinSeverity c Debug
    CM.setSetupBackends c [KatipBK]
    CM.setDefaultBackends c [KatipBK]
    CM.setSetupScribes c [ ScribeDefinition {
                              scName = "stdout"
                            , scKind = StdoutSK
                            , scPrivacy = ScPublic
                            , scRotation = Nothing
                            }
                      ]
    CM.setDefaultScribes c ["StdoutSK::stdout"]
    return c

\end{code}

\subsubsection{Default configuration for testing}
\begin{code}
defaultConfigTesting :: IO CM.Configuration
defaultConfigTesting = do
    c <- CM.empty
    CM.setMinSeverity c Debug
#ifdef ENABLE_AGGREGATION
    CM.setSetupBackends c [KatipBK, AggregationBK]
    CM.setDefaultBackends c [KatipBK, AggregationBK]
#else
    CM.setSetupBackends c [KatipBK]
    CM.setDefaultBackends c [KatipBK]
#endif
    CM.setSetupScribes c [ ScribeDefinition {
                              scName = "nooutput"
                            , scKind = DevNullSK
                            , scPrivacy = ScPublic
                            , scRotation = Nothing
                            }
                      ]
    CM.setDefaultScribes c ["NullSK::nooutput"]

    return c

\end{code}
