\subsection{Cardano.BM.Output.LogBuffer}
\label{module:Cardano.BM.Output.LogBuffer}

%if style == newcode
\begin{code}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.BM.Output.LogBuffer
    ( LogBuffer
    , readBuffer
    , effectuate
    , realize
    , unrealize
    ) where

import           Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, withMVar)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.IO as TIO
import           System.IO (stderr)

import           Cardano.BM.Data.Backend (BackendKind (LogBufferBK),
                     IsBackend (..), IsEffectuator (..))
import           Cardano.BM.Data.LogItem (LoggerName, LogObject (..))

\end{code}
%endif

\subsubsection{Structure of LogBuffer}\label{code:LogBuffer}\index{LogBuffer}
\begin{code}
newtype LogBuffer a = LogBuffer
    { getLogBuf :: LogBufferMVar a }

type LogBufferMVar a = MVar (LogBufferInternal a)

data LogBufferInternal a = LogBufferInternal
    { logBuffer :: LogBufferMap a
    }

\end{code}

\subsubsection{Relation from log context name to log item}
We keep the latest |LogObject| from a log context in a |HashMap|.
\begin{code}
type LogBufferMap a = HM.HashMap LoggerName (LogObject a)

\end{code}

\subsubsection{Read out the latest |LogObject|s}
\begin{code}
readBuffer :: LogBuffer a -> IO [(LoggerName, LogObject a)]
readBuffer buffer =
    withMVar (getLogBuf buffer) $ \currentBuffer ->
        return $ HM.toList $ logBuffer currentBuffer
\end{code}

\subsubsection{LogBuffer is an effectuator}\index{LogBuffer!instance of IsEffectuator}
Function |effectuate| is called to pass in a |LogObject| for log buffering.
\begin{code}
instance IsEffectuator LogBuffer a where
    effectuate buffer lo@(LogObject logname _lometa _logitem) = do
        modifyMVar_ (getLogBuf buffer) $ \currentBuffer ->
            return $ LogBufferInternal $ HM.insert logname lo $ logBuffer currentBuffer

    handleOverflow _ = TIO.hPutStrLn stderr "Notice: overflow in LogBuffer, dropping log items!"

\end{code}

\subsubsection{|LogBuffer| implements |Backend| functions}\index{LogBuffer!instance of IsBackend}

|LogBuffer| is an |IsBackend|
\begin{code}
instance IsBackend LogBuffer a where
    typeof _ = LogBufferBK

    realize _ = do
        let emptyBuffer = LogBufferInternal HM.empty
        LogBuffer <$> newMVar emptyBuffer

    unrealize _ = return ()

\end{code}
