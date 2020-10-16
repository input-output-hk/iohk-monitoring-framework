{-# LANGUAGE CPP #-}
module Cardano.BM.Stats
    ( ProcessStats(..)
    , Platform.readProcessStats
    )
where

import Cardano.BM.Stats.Types (ProcessStats(..))

#if defined(linux_HOST_OS)
import qualified Cardano.BM.Counters.Linux   as Platform
#elif defined(mingw32_HOST_OS)
import qualified Cardano.BM.Counters.Windows as Platform
#elif defined(darwin_HOST_OS)
import qualified Cardano.BM.Counters.Darwin  as Platform
#else
import qualified Cardano.BM.Counters.Dummy   as Platform
#endif
