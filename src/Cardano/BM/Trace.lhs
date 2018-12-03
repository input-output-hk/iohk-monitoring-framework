
\subsection{Cardano.BM.Trace}

%if False
\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Cardano.BM.Trace
    (
      Trace
    , stdoutTrace
    , noTrace
    , mainTrace
    , traceInTVar
    , traceInTVarIO
    , traceNamedInTVarIO
    -- * context naming
    , appendName
    -- * utils
    , natTrace
    , subTrace
    -- * log functions
    , traceNamedObject
    , traceNamedItem
    , logDebug,   logDebugS,   logDebugP,   logDebugUnsafeP
    , logError,   logErrorS,   logErrorP,   logErrorUnsafeP
    , logInfo,    logInfoS,    logInfoP,    logInfoUnsafeP
    , logNotice,  logNoticeS,  logNoticeP,  logNoticeUnsafeP
    , logWarning, logWarningS, logWarningP, logWarningUnsafeP

    ) where

import           Control.Concurrent.MVar (MVar, newMVar, withMVar)
import qualified Control.Concurrent.STM.TVar as STM
import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.STM as STM
import           Data.Aeson.Text (encodeToLazyText)
import           Data.Functor.Contravariant (Contravariant (..), Op (..))
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Text.Lazy (toStrict)
import           System.IO.Unsafe (unsafePerformIO)

import           Cardano.BM.BaseTrace
import qualified Cardano.BM.Configuration as Config
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.Trace
import qualified Cardano.BM.Output.Switchboard as Switchboard
import           Cardano.BM.Data.SubTrace

\end{code}
%endif

\subsubsection{Enter new named context}\label{code:appendName}
The context name is created and checked that its size is below a limit
(currently 50 chars).
The minimum severity that a log message must be labelled with is looked up in
the configuration and recalculated.
\begin{code}
appendName :: MonadIO m => LoggerName -> Trace m -> m (Trace m)
appendName name (ctx, trace) = do
    let prevLoggerName  = loggerName ctx
        prevMinSeverity = minSeverity ctx
        newLoggerName   = appendWithDot prevLoggerName name
    globMinSeverity <- liftIO $ Config.minSeverity (configuration ctx)
    namedSeverity <- liftIO $ Config.inspectSeverity (configuration ctx) newLoggerName
    case namedSeverity of
        Nothing  -> return ( ctx { loggerName = newLoggerName }, trace )
        Just sev -> return ( ctx { loggerName = newLoggerName
                                , minSeverity = max (max sev prevMinSeverity) globMinSeverity }
                           , trace )

appendWithDot :: LoggerName -> LoggerName -> LoggerName
appendWithDot "" newName = T.take 50 newName
appendWithDot xs ""      = xs
appendWithDot xs newName = T.take 50 $ xs <> "." <> newName

\end{code}

\begin{code}
-- return a BaseTrace from a TraceNamed
named :: BaseTrace m (LogNamed i) -> LoggerName -> BaseTrace m i
named trace name = contramap (LogNamed name) trace

\end{code}

\todo[inline]{TODO remove |locallock|}
%if False
\begin{code}
{-# NOINLINE locallock #-}
\end{code}
%endif
\begin{code}
locallock :: MVar ()
locallock = unsafePerformIO $ newMVar ()
\end{code}


\subsubsection{Trace that forwards to the \nameref{code:Switchboard}}\label{code:mainTrace}

Every |Trace| ends in the switchboard which then takes care of
dispatching the messages to outputs

\begin{code}
mainTrace :: Switchboard.Switchboard -> TraceNamed IO
mainTrace sb = BaseTrace $ Op $ \lognamed -> do
    Switchboard.pass sb lognamed

\end{code}

\subsubsection{Concrete Trace on stdout}\label{code:stdoutTrace}

This function returns a trace with an action of type "|(LogNamed LogObject) -> IO ()|"
which will output a text message as text and all others as JSON encoded representation
to the console.

\begin{code}
stdoutTrace :: TraceNamed IO
stdoutTrace = BaseTrace $ Op $ \lognamed ->
    case lnItem lognamed of
        LP (LogMessage logItem) ->
            withMVar locallock $ \_ ->
                output (lnName lognamed) $ liPayload logItem
        obj ->
            withMVar locallock $ \_ ->
                output (lnName lognamed) $ toStrict (encodeToLazyText obj)
  where
    output nm msg = TIO.putStrLn $ nm <> " :: " <> msg

\end{code}


\subsubsection{Concrete Trace into a |TVar|}\label{code:traceInTVar}\label{code:traceInTVarIO}

\begin{code}
traceInTVar :: STM.TVar [a] -> BaseTrace STM.STM a
traceInTVar tvar = BaseTrace $ Op $ \a -> STM.modifyTVar tvar ((:) a)

traceInTVarIO :: STM.TVar [LogObject] -> TraceNamed IO
traceInTVarIO tvar = BaseTrace $ Op $ \ln ->
                         STM.atomically $ STM.modifyTVar tvar ((:) (lnItem ln))

traceNamedInTVarIO :: STM.TVar [LogNamed LogObject] -> TraceNamed IO
traceNamedInTVarIO tvar = BaseTrace $ Op $ \ln ->
                         STM.atomically $ STM.modifyTVar tvar ((:) ln)

\end{code}

\subsubsection{Check a log item's severity against the |Trace|'s minimum severity}\label{code:traceConditionally}
\todo[inline]{do we need three different |minSeverity| defined?}

We do a lookup of the global |minSeverity| in the configuration. And, a lookup of the |minSeverity| for the current named context. These values might have changed in the meanwhile.
\newline
A third filter is the |minSeverity| defined in the current context.
\begin{code}
traceConditionally
    :: (MonadIO m)
    => TraceContext -> BaseTrace m LogObject -> LogObject
    -> m ()
traceConditionally ctx logTrace msg@(LP (LogMessage item)) = do
    globminsev <- liftIO $ Config.minSeverity (configuration ctx)
    globnamesev <- liftIO $ Config.inspectSeverity (configuration ctx) (loggerName ctx)
    let minsev = max (minSeverity ctx) $ max globminsev (fromMaybe Debug globnamesev)
        flag = (liSeverity item) >= minsev
    when flag $ traceWith logTrace msg
traceConditionally _ logTrace logObject = traceWith logTrace logObject

\end{code}

\subsubsection{Enter message into a trace}\label{code:traceNamedItem}
The function |traceNamedItem| creates a |LogObject| and threads this through
the action defined in the |Trace|.

\begin{code}
traceNamedItem
    :: (MonadIO m)
    => Trace m
    -> LogSelection
    -> Severity
    -> T.Text
    -> m ()
traceNamedItem (ctx, logTrace) p s m =
    let logmsg = LP $ LogMessage $ LogItem { liSelection = p
                                           , liSeverity  = s
                                           , liPayload   = m
                                           }
    in
    traceConditionally ctx (named logTrace (loggerName ctx)) $ logmsg

logDebug, logInfo, logNotice, logWarning, logError
    :: (MonadIO m) => Trace m -> T.Text -> m ()
logDebug logTrace   = traceNamedItem logTrace Both Debug
logInfo logTrace    = traceNamedItem logTrace Both Info
logNotice logTrace  = traceNamedItem logTrace Both Notice
logWarning logTrace = traceNamedItem logTrace Both Warning
logError logTrace   = traceNamedItem logTrace Both Error

logDebugS, logInfoS, logNoticeS, logWarningS, logErrorS
    :: (MonadIO m) => Trace m -> T.Text -> m ()
logDebugS logTrace   = traceNamedItem logTrace Private Debug
logInfoS logTrace    = traceNamedItem logTrace Private Info
logNoticeS logTrace  = traceNamedItem logTrace Private Notice
logWarningS logTrace = traceNamedItem logTrace Private Warning
logErrorS logTrace   = traceNamedItem logTrace Private Error

logDebugP, logInfoP, logNoticeP, logWarningP, logErrorP
    :: (MonadIO m) => Trace m -> T.Text -> m ()
logDebugP logTrace   = traceNamedItem logTrace Public Debug
logInfoP logTrace    = traceNamedItem logTrace Public Info
logNoticeP logTrace  = traceNamedItem logTrace Public Notice
logWarningP logTrace = traceNamedItem logTrace Public Warning
logErrorP logTrace   = traceNamedItem logTrace Public Error

logDebugUnsafeP, logInfoUnsafeP, logNoticeUnsafeP, logWarningUnsafeP, logErrorUnsafeP
    :: (MonadIO m) => Trace m -> T.Text -> m ()
logDebugUnsafeP logTrace   = traceNamedItem logTrace PublicUnsafe Debug
logInfoUnsafeP logTrace    = traceNamedItem logTrace PublicUnsafe Info
logNoticeUnsafeP logTrace  = traceNamedItem logTrace PublicUnsafe Notice
logWarningUnsafeP logTrace = traceNamedItem logTrace PublicUnsafe Warning
logErrorUnsafeP logTrace   = traceNamedItem logTrace PublicUnsafe Error

\end{code}

%if False
\begin{spec}

{-
logMessage, logMessageS, logMessageP :: Trace m -> Severity -> T.Text -> m ()
logMessage logTrace  = traceNamedItem logTrace Both
logMessageS logTrace = traceNamedItem logTrace Private
logMessageP logTrace = traceNamedItem logTrace Public
-}

example :: IO ()
example = do
    let logTrace0 = stdoutTrace
    ctx <- newMVar $ TraceController $ mempty
    logTrace <- appendName "my_example" (ctx, logTrace0)
    insertInOracle logTrace "expect_answer" Neutral
    result <- bracketObserveIO logTrace "expect_answer" setVar\_
    logInfo logTrace $ pack $ show result

example\_TVar :: IO ()
example\_TVar = do
    tvar <- STM.newTVarIO []
    let logTrace0 = traceInTVarIO tvar
    ctx <- newMVar $ TraceController $ mempty
    logTrace <- appendName "my_example" $ (ctx, logTrace0)
    result <- bracketObserveIO logTrace "expect_answer" setVar_
    logInfo logTrace $ pack $ show result
    items <- STM.readTVarIO tvar
    TIO.putStrLn $ pack $ show $ dropPrims $ items
  where
    dropPrims :: [LogObject] -> [LogObject]
    dropPrims = filter (\case {LP _ -> False; _ -> True})

setVar_ :: STM.STM Integer
setVar_ = do
    t <- STM.newTVar 0
    STM.writeTVar t 42
    res <- STM.readTVar t
    return res

exampleConfiguration :: IO Integer
exampleConfiguration = withTrace (TraceConfiguration StdOut "my_example" (ObservableTrace observablesSet) Debug) $
    \tr -> bracketObserveIO tr "my_example" setVar\_
  where
    observablesSet :: Set ObservableInstance
    observablesSet = fromList [MonotonicClock, MemoryStats]

\end{spec}
%endif

\begin{code}

traceNamedObject
    :: Trace m
    -> LogObject
    -> m ()
traceNamedObject (ctx, logTrace) = traceWith (named logTrace (loggerName ctx))

\end{code}

\subsubsection{subTrace}\label{code:subTrace}
Transforms the |Trace| according to the
|Configuration| using the logger name of the
current |Trace| appended with the given name. If the
empty |Text| is given as name then the logger name
remains untouched.
\begin{code}
subTrace :: MonadIO m => T.Text -> Trace m -> m (SubTrace, Trace m)
subTrace name tr@(ctx, _) = do
    let newName = appendWithDot (loggerName ctx) name
    subtrace0 <- liftIO $ Config.findSubTrace (configuration ctx) newName
    let subtrace = case subtrace0 of Nothing -> Neutral; Just tr -> tr
    case subtrace of
        Neutral      -> do
                            tr' <- appendName name tr
                            return $ (subtrace, tr')
        UntimedTrace -> do
                            tr' <- appendName name tr
                            return $ (subtrace, tr')
        NoTrace      -> return (subtrace, (ctx, BaseTrace $ Op $ \_ -> pure ()))
        DropOpening  -> return (subtrace, (ctx, BaseTrace $ Op $ \lognamed -> do
            case lnItem lognamed of
                ObserveOpen _ -> return ()
                obj           -> traceNamedObject tr obj))
        ObservableTrace _ -> do
                            tr' <- appendName name tr
                            return $ (subtrace, tr')

\end{code}
