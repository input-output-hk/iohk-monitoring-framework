module Cardano.BM.Stats.Types
  (ProcessStats(..))
where

data ProcessStats
  = ProcessStatsDarwin
    { psCentiSecsCpu    :: !Word
    , psCentiSecsGC     :: !Word
    , psRSS             :: !Word
    }
  | ProcessStatsDummy
    {
    }
  | ProcessStatsLinux
    { psCentiSecsCpu    :: !Word
    , psCentiSecsGC     :: !Word
    , psRSS             :: !Word
    , psCentiSecsIOWait :: !Word
    , psThreads         :: !Word
    }
  | ProcessStatsWindows
    { psCentiSecsCpu    :: !Word
    , psCentiSecsGC     :: !Word
    , psRSS             :: !Word
    }
  deriving Show
