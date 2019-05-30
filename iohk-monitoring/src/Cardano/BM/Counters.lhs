
\subsection{Cardano.BM.Counters}
\label{code:Cardano.BM.Counters}

Here the platform is chosen on which we compile this program.

Currently, we mainly support |Linux| with its 'proc' filesystem.

\begin{code}
{-# LANGUAGE CPP #-}

#if defined(linux_HOST_OS)
#define LINUX
#endif

module Cardano.BM.Counters
    (
      Platform.readCounters
    , getMonoClock
    ) where

#ifdef LINUX
import qualified Cardano.BM.Counters.Linux as Platform
#else
import qualified Cardano.BM.Counters.Dummy as Platform
#endif

import           Cardano.BM.Counters.Common (getMonoClock)

\end{code}
