module Cardano.BM.Stats.Types
  (ProcessStats(..))
where

data ProcessStats
  = ProcessStatsDarwin
    { psCentiSecsCpu    :: !Word
    , psRSS             :: !Word
    }
  | ProcessStatsDummy
    {
    }
  | ProcessStatsLinux
    { psCentiSecsCpu    :: !Word
    , psRSS             :: !Word
    , psCentiSecsIOWait :: !Word
    , psThreads         :: !Word
    }
  | ProcessStatsWindows
    { psCentiSecsCpu    :: !Word
    , psRSS             :: !Word
    }
  deriving Show
