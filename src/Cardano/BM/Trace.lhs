
\subsection{Cardano.BM.Trace}

%if style == newcode
\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Cardano.BM.Trace
    (
      Trace
    , stdoutTrace
    , BaseTrace.noTrace
    , traceInTVar
    , traceInTVarIO
    , traceNamedInTVarIO
    -- * context naming
    , appendName
    -- * utils
    , natTrace
    , subTrace
    , typeofTrace
    , updateTracetype
    , evalFilters
    -- * log functions
    , traceNamedObject
    , traceNamedItem
    , logAlert,     logAlertS,     logAlertP,     logAlertUnsafeP
    , logCritical,  logCriticalS,  logCriticalP,  logCriticalUnsafeP
    , logDebug,     logDebugS,     logDebugP,     logDebugUnsafeP
    , logEmergency, logEmergencyS, logEmergencyP, logEmergencyUnsafeP
    , logError,     logErrorS,     logErrorP,     logErrorUnsafeP
    , logInfo,      logInfoS,      logInfoP,      logInfoUnsafeP
    , logNotice,    logNoticeS,    logNoticeP,    logNoticeUnsafeP
    , logWarning,   logWarningS,   logWarningP,   logWarningUnsafeP
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

import qualified Cardano.BM.BaseTrace as BaseTrace
import qualified Cardano.BM.Configuration as Config
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.Trace
import           Cardano.BM.Data.SubTrace

\end{code}
%endif

\subsubsection{Utilities}
Natural transformation from monad |m| to monad |n|.
\begin{code}
natTrace :: (forall x . m x -> n x) -> Trace m -> Trace n
natTrace nat (ctx, trace) = (ctx, BaseTrace.natTrace nat trace)

\end{code}

Access type of |Trace|.\label{code:typeofTrace}\index{typeofTrace}
\begin{code}
typeofTrace :: Trace m -> SubTrace
typeofTrace (ctx, _) = tracetype ctx

\end{code}

Update type of |Trace|.\label{code:updateTracetype}\index{updateTracetype}
\begin{code}
updateTracetype :: SubTrace -> Trace m -> Trace m
updateTracetype subtr (ctx, tr) = (ctx {tracetype=subtr}, tr)

\end{code}


\subsubsection{Enter new named context}\label{code:appendName}\index{appendName}
The context name is created and checked that its size is below a limit
(currently 80 chars).
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
appendWithDot "" newName = T.take 80 newName
appendWithDot xs ""      = xs
appendWithDot xs newName = T.take 80 $ xs <> "." <> newName

\end{code}

\subsubsection{Contramap a trace and produce the naming context}
\begin{code}
named :: BaseTrace.BaseTrace m (LogNamed i) -> LoggerName -> BaseTrace.BaseTrace m i
named trace name = contramap (LogNamed name) trace

\end{code}

\subsubsection{Trace a |LogObject| through}
\label{code:traceNamedObject}\index{traceNamedObject}
\begin{code}
traceNamedObject
    :: MonadIO m
    => Trace m
    -> LogObject
    -> m ()
traceNamedObject trace@(ctx, logTrace) lo = do
    let lname = loggerName ctx
    doOutput <- case (typeofTrace trace) of
        FilterTrace filters ->
             case lo of
                LogValue loname _ ->
                    return $ evalFilters filters (lname <> "." <> loname)
                _ ->
                    return $ evalFilters filters lname
        TeeTrace secName -> do
             -- create a newly named copy of the |LogObject|
             BaseTrace.traceWith (named logTrace (lname <> "." <> secName)) lo
             return True
        _ -> return True
    if doOutput
    then BaseTrace.traceWith (named logTrace lname) lo
    else return ()

\end{code}

\subsubsection{Evaluation of |FilterTrace|}\label{code:evalFilters}\index{evalFilters}

A filter consists of a |DropName| and a list of |UnhideNames|. If the context name matches
the |DropName| filter, then at least one of the |UnhideNames| must match the name to have
the evaluation of the filters return |True|.

\begin{code}
evalFilters :: [(DropName, UnhideNames)] -> LoggerName -> Bool
evalFilters fs nm =
    all (\(no, yes) -> if (dropFilter nm no) then (unhideFilter nm yes) else True) fs
  where
    dropFilter :: LoggerName -> DropName -> Bool
    dropFilter name (Drop sel) = {-not-} (matchName name sel)
    unhideFilter :: LoggerName -> UnhideNames -> Bool
    unhideFilter _ (Unhide []) = False
    unhideFilter name (Unhide us) = any (\sel -> matchName name sel) us
    matchName :: LoggerName -> NameSelector -> Bool
    matchName name (Exact name') = name == name'
    matchName name (StartsWith prefix) = T.isPrefixOf prefix name
    matchName name (EndsWith postfix) = T.isSuffixOf postfix name
    matchName name (Contains name') = T.isInfixOf name' name
\end{code}

\subsubsection{Concrete Trace on stdout}\label{code:stdoutTrace}\index{stdoutTrace}

This function returns a trace with an action of type "|(LogNamed LogObject) -> IO ()|"
which will output a text message as text and all others as JSON encoded representation
to the console.

\todo[inline]{TODO remove |locallock|}
%if style == newcode
\begin{code}
{-# NOINLINE locallock #-}
\end{code}
%endif
\begin{code}
locallock :: MVar ()
locallock = unsafePerformIO $ newMVar ()
\end{code}

\begin{code}
stdoutTrace :: TraceNamed IO
stdoutTrace = BaseTrace.BaseTrace $ Op $ \lognamed ->
    withMVar locallock $ \_ ->
        case lnItem lognamed of
            (LogMessage logItem) ->
                    output (lnName lognamed) $ liPayload logItem
            obj ->
                    output (lnName lognamed) $ toStrict (encodeToLazyText obj)
  where
    output nm msg = TIO.putStrLn $ nm <> " :: " <> msg

\end{code}


\subsubsection{Concrete Trace into a |TVar|}\label{code:traceInTVar}\label{code:traceInTVarIO}\index{traceInTVar}\index{traceInTVarIO}\label{code:traceNamedInTVarIO}\index{traceNamedInTVarIO}

\begin{code}
traceInTVar :: STM.TVar [a] -> BaseTrace.BaseTrace STM.STM a
traceInTVar tvar = BaseTrace.BaseTrace $ Op $ \a -> STM.modifyTVar tvar ((:) a)

traceInTVarIO :: STM.TVar [LogObject] -> TraceNamed IO
traceInTVarIO tvar = BaseTrace.BaseTrace $ Op $ \ln ->
                         STM.atomically $ STM.modifyTVar tvar ((:) (lnItem ln))

traceNamedInTVarIO :: STM.TVar [LogNamed LogObject] -> TraceNamed IO
traceNamedInTVarIO tvar = BaseTrace.BaseTrace $ Op $ \ln ->
                         STM.atomically $ STM.modifyTVar tvar ((:) ln)

\end{code}

\subsubsection{Check a log item's severity against the |Trace|'s minimum severity}\label{code:traceConditionally}\index{traceConditionally}
\todo[inline]{do we need three different |minSeverity| defined?}

We do a lookup of the global |minSeverity| in the configuration. And, a lookup of the |minSeverity| for the current named context. These values might have changed in the meanwhile.
\newline
A third filter is the |minSeverity| defined in the current context.
\begin{code}
traceConditionally
    :: MonadIO m
    => Trace m -> LogObject
    -> m ()
traceConditionally logTrace@(ctx, _) msg@(LogMessage item) = do
    globminsev <- liftIO $ Config.minSeverity (configuration ctx)
    globnamesev <- liftIO $ Config.inspectSeverity (configuration ctx) (loggerName ctx)
    let minsev = max (minSeverity ctx) $ max globminsev (fromMaybe Debug globnamesev)
        flag = (liSeverity item) >= minsev
    when flag $ traceNamedObject logTrace msg
traceConditionally logTrace logObject =
    traceNamedObject logTrace logObject

\end{code}

\subsubsection{Enter message into a trace}\label{code:traceNamedItem}\index{traceNamedItem}
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
traceNamedItem trace p s m =
    let logmsg = LogMessage $ LogItem { liSelection = p
                                           , liSeverity  = s
                                           , liPayload   = m
                                           }
    in
    traceConditionally trace $ logmsg

\end{code}

\subsubsection{Logging functions}
\label{code:logDebug}\index{logDebug}
\label{code:logDebugS}\index{logDebugS}
\label{code:logDebugP}\index{logDebugP}
\label{code:logInfo}\index{logInfo}
\label{code:logInfoS}\index{logInfoS}
\label{code:logInfoP}\index{logInfoP}
\label{code:logNotice}\index{logNotice}
\label{code:logNoticeS}\index{logNoticeS}
\label{code:logNoticeP}\index{logNoticeP}
\label{code:logWarning}\index{logWarning}
\label{code:logWarningS}\index{logWarningS}
\label{code:logWarningP}\index{logWarningP}
\label{code:logError}\index{logError}
\label{code:logErrorS}\index{logErrorS}
\label{code:logErrorP}\index{logErrorP}
\label{code:logCritical}\index{logCritical}
\label{code:logCriticalS}\index{logCriticalS}
\label{code:logCriticalP}\index{logCriticalP}
\label{code:logAlert}\index{logAlert}
\label{code:logAlertS}\index{logAlertS}
\label{code:logAlertP}\index{logAlertP}
\label{code:logEmergency}\index{logEmergency}
\label{code:logEmergencyS}\index{logEmergencyS}
\label{code:logEmergencyP}\index{logEmergencyP}
\begin{code}
logDebug, logInfo, logNotice, logWarning, logError, logCritical, logAlert, logEmergency
    :: (MonadIO m) => Trace m -> T.Text -> m ()
logDebug     logTrace = traceNamedItem logTrace Both Debug
logInfo      logTrace = traceNamedItem logTrace Both Info
logNotice    logTrace = traceNamedItem logTrace Both Notice
logWarning   logTrace = traceNamedItem logTrace Both Warning
logError     logTrace = traceNamedItem logTrace Both Error
logCritical  logTrace = traceNamedItem logTrace Both Critical
logAlert     logTrace = traceNamedItem logTrace Both Alert
logEmergency logTrace = traceNamedItem logTrace Both Emergency

logDebugS, logInfoS, logNoticeS, logWarningS, logErrorS, logCriticalS, logAlertS, logEmergencyS
    :: (MonadIO m) => Trace m -> T.Text -> m ()
logDebugS     logTrace = traceNamedItem logTrace Private Debug
logInfoS      logTrace = traceNamedItem logTrace Private Info
logNoticeS    logTrace = traceNamedItem logTrace Private Notice
logWarningS   logTrace = traceNamedItem logTrace Private Warning
logErrorS     logTrace = traceNamedItem logTrace Private Error
logCriticalS  logTrace = traceNamedItem logTrace Private Critical
logAlertS     logTrace = traceNamedItem logTrace Private Alert
logEmergencyS logTrace = traceNamedItem logTrace Private Emergency

logDebugP, logInfoP, logNoticeP, logWarningP, logErrorP, logCriticalP, logAlertP, logEmergencyP
    :: (MonadIO m) => Trace m -> T.Text -> m ()
logDebugP     logTrace = traceNamedItem logTrace Public Debug
logInfoP      logTrace = traceNamedItem logTrace Public Info
logNoticeP    logTrace = traceNamedItem logTrace Public Notice
logWarningP   logTrace = traceNamedItem logTrace Public Warning
logErrorP     logTrace = traceNamedItem logTrace Public Error
logCriticalP  logTrace = traceNamedItem logTrace Public Critical
logAlertP     logTrace = traceNamedItem logTrace Public Alert
logEmergencyP logTrace = traceNamedItem logTrace Public Emergency

logDebugUnsafeP, logInfoUnsafeP, logNoticeUnsafeP, logWarningUnsafeP, logErrorUnsafeP,
    logCriticalUnsafeP, logAlertUnsafeP, logEmergencyUnsafeP
    :: (MonadIO m) => Trace m -> T.Text -> m ()
logDebugUnsafeP     logTrace = traceNamedItem logTrace PublicUnsafe Debug
logInfoUnsafeP      logTrace = traceNamedItem logTrace PublicUnsafe Info
logNoticeUnsafeP    logTrace = traceNamedItem logTrace PublicUnsafe Notice
logWarningUnsafeP   logTrace = traceNamedItem logTrace PublicUnsafe Warning
logErrorUnsafeP     logTrace = traceNamedItem logTrace PublicUnsafe Error
logCriticalUnsafeP  logTrace = traceNamedItem logTrace PublicUnsafe Critical
logAlertUnsafeP     logTrace = traceNamedItem logTrace PublicUnsafe Alert
logEmergencyUnsafeP logTrace = traceNamedItem logTrace PublicUnsafe Emergency

\end{code}

%if style == newcode
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
    dropPrims = filter (\case {LogMessage _ -> False; LogValue _ -> False; _ -> True})

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

\subsubsection{subTrace}\label{code:subTrace}\index{subTrace}
Transforms the input |Trace| according to the
|Configuration| using the logger name of the
current |Trace| appended with the new name. If the
empty |Text| is passed, then the logger name
remains untouched.
\begin{code}
subTrace :: MonadIO m => T.Text -> Trace m -> m (Trace m)
subTrace name tr@(ctx, _) = do
    let newName = appendWithDot (loggerName ctx) name
    subtrace0 <- liftIO $ Config.findSubTrace (configuration ctx) newName
    let subtrace = case subtrace0 of Nothing -> Neutral; Just str -> str
    case subtrace of
        Neutral       -> do
                            tr' <- appendName name tr
                            return $ updateTracetype subtrace tr'
        UntimedTrace  -> do
                            tr' <- appendName name tr
                            return $ updateTracetype subtrace tr'
        TeeTrace _    -> do
                            tr' <- appendName name tr
                            return $ updateTracetype subtrace tr'
        FilterTrace _ -> do
                            tr' <- appendName name tr
                            return $ updateTracetype subtrace tr'
        NoTrace       -> return $ updateTracetype subtrace (ctx, BaseTrace.BaseTrace $ Op $ \_ -> pure ())
        DropOpening   -> return $ updateTracetype subtrace (ctx, BaseTrace.BaseTrace $ Op $ \lognamed -> do
            case lnItem lognamed of
                ObserveOpen _ -> return ()
                obj           -> traceNamedObject tr obj )
        ObservableTrace _ -> do
                            tr' <- appendName name tr
                            return $ updateTracetype subtrace tr'

\end{code}
