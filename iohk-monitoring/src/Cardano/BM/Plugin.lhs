
\subsection{Cardano.BM.Plugin}
\label{code:Cardano.BM.Plugin}

%if style == newcode
\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
module Cardano.BM.Plugin
  ( Plugin (..)
  , loadPlugin
  )
  where

import           Cardano.BM.Backend.Log (Scribe)
import           Cardano.BM.Backend.Switchboard (Switchboard,
                     addExternalBackend, addExternalScribe)
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.Output

\end{code}
%endif

\subsubsection{Plugins extend functionality}\label{code:Plugin}\index{Plugin}
A |Plugin| has a name and is either a |Backend| or a |Scribe|.
\begin{code}

data Plugin a = BackendPlugin (Backend a) BackendId
              | ScribePlugin Scribe ScribeId
\end{code}

\subsubsection{Plugin behaviour}

\subsubsection{Integrating plugins}
\begin{code}
loadPlugin :: Plugin a -> Switchboard a -> IO ()
loadPlugin (BackendPlugin be nm) sb = do
    addExternalBackend sb be nm
loadPlugin (ScribePlugin sc nm) sb = do
    addExternalScribe sb sc nm

\end{code}
