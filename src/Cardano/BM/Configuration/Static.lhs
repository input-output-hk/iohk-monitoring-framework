\subsection{Cardano.BM.Configuration.Static}
\label{module:Cardano.BM.Configuration.Static}

%if style == newcode
\begin{code}
{-# LANGUAGE OverloadedStrings   #-}

module Cardano.BM.Configuration.Static
    (
      defaultConfigStdout
    ) where

import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.Output
import           Cardano.BM.Data.Severity
--import           Cardano.BM.Output.Log (passN)

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
                            , scRotation = Nothing
                            }
                      ]
    CM.setDefaultScribes c ["StdoutSK::stdout"]
    return c


\end{code}
