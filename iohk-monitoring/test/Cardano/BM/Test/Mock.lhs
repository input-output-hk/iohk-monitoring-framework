
\subsection{Cardano.BM.Test.Mock}
\label{code:Cardano.BM.Test.Mock}

%if style == newcode
\begin{code}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

#if !defined(mingw32_HOST_OS)
#define POSIX
#endif

module Cardano.BM.Test.Mock
    (
      MockSwitchboard (..)
    , traceMock
    ) where

import           Control.Concurrent.STM (TVar, atomically, modifyTVar)
import           Data.Maybe (fromMaybe)

import           Cardano.BM.Backend.Switchboard (mainTraceConditionally)
import qualified Cardano.BM.Configuration as Config
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.SubTrace (SubTrace (..))
import           Cardano.BM.Data.Trace (Trace)
import           Cardano.BM.Data.Tracer (Tracer (..), traceWith)

\end{code}
%endif

\subsubsection{MockSwitchboard}\label{code:MockSwitchboard}\index{MockSwitchboard}
|MockSwitchboard| is useful for tests since it keeps the |LogObject|s
to be output in a list.

\begin{code}
newtype MockSwitchboard a = MockSB (TVar [LogObject a])

instance IsEffectuator MockSwitchboard a where
    effectuate (MockSB tvar) item = atomically $ modifyTVar tvar ((:) item)
    handleOverflow _ = pure ()

\end{code}

\subsubsection{traceMock}\label{code:traceMock}\index{traceMock}
A |Trace| which forwards |LogObject|s to |MockSwitchboard| simulating
functionality of |mainTraceConditionally|.

\begin{code}
traceMock :: MockSwitchboard a -> Config.Configuration -> Trace IO a
traceMock ms config =
    Tracer $ \(ctx, lo) -> do
        traceWith mainTrace (ctx, lo)
        subTrace <- fromMaybe Neutral <$> Config.findSubTrace config ctx
        case subTrace of
            TeeTrace secName ->
                traceWith mainTrace (ctx <> "." <> secName, lo)
            _ -> return ()
  where
    mainTrace = mainTraceConditionally config ms

\end{code}
