
\subsection{Cardano.BM.Counters}
\label{code:Cardano.BM.Counters}

The platform is chosen on which we compile this library.

Currently, we mainly support |Linux| with its 'proc' filesystem,
but also partially support |Windows|.

\begin{code}
{-# LANGUAGE CPP #-}

#if defined(linux_HOST_OS)
#define LINUX
#endif
#if defined(mingw32_HOST_OS)
#define WINDOWS
#endif

module Cardano.BM.Counters
    (
      Platform.readCounters
    , getMonoClock
    ) where

#ifdef LINUX
import qualified Cardano.BM.Counters.Linux as Platform
#elif defined(WINDOWS)
import qualified Cardano.BM.Counters.Windows as Platform
#else
import qualified Cardano.BM.Counters.Dummy as Platform
#endif

import           Cardano.BM.Counters.Common (getMonoClock)

\end{code}
