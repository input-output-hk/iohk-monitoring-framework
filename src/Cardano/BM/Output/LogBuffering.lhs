\subsection{Cardano.BM.Output.LogBuffering}
\label{module:Cardano.BM.Output.LogBuffering}

%if style == newcode
\begin{code}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.Output.LogBuffering
    ( LogBuffer
    , effectuate
    , realizefrom
    , unrealize
    ) where

import           Control.Concurrent.MVar (MVar, newMVar,
                     modifyMVar_, readMVar)
import qualified Data.HashMap.Strict as HM

import           Cardano.BM.Data.Backend (BackendKind (LogBufferingBK),
                     IsBackend (..), IsEffectuator (..))
import           Cardano.BM.Data.LogItem (LoggerName, LogNamed (..), LogObject)

\end{code}
%endif

\subsubsection{Structure of LogBuffering}\label{code:LogBuffer}\index{LogBuffer}
\begin{code}
newtype LogBuffer = LogBuffer
    { getLogBuf :: LogBufferMVar }

type LogBufferMVar = MVar LogBufferInternal

data LogBufferInternal = LogBufferInternal
    { logBuffer :: HM.HashMap LoggerName LogObject
    }
\end{code}

\subsubsection{LogBuffer view is an effectuator}\index{LogBuffer!instance of IsEffectuator}
Function |effectuate| is called to pass in a |NamedLogItem| for log buffering.
\begin{code}
instance IsEffectuator LogBuffer where
    effectuate buffer (LogNamed loggerName newItem) = do
        let logBufferMVar = getLogBuf buffer
        currentBuffer <- readMVar logBufferMVar
        let updatedBuffer = HM.insert loggerName newItem $ logBuffer currentBuffer
        modifyMVar_ logBufferMVar $ \_ ->
            return $ LogBufferInternal updatedBuffer

    handleOverflow _ = putStrLn "Notice: Log buffers's queue full, dropping log items!"

\end{code}

\subsubsection{|LogBuffer| implements |Backend| functions}\index{LogBuffer!instance of IsBackend}

|LogBuffer| is an |IsBackend|
\begin{code}
instance IsBackend LogBuffer where
    typeof _ = LogBufferingBK

    realize _ = do
        let emptyBuffer = LogBufferInternal HM.empty
        LogBuffer <$> newMVar emptyBuffer

    unrealize _ = return ()

\end{code}
