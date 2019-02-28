
\subsection{Cardano.BM.Output.Log}
\label{code:Cardano.BM.Output.Log}

%if style == newcode
\begin{code}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.BM.Output.Log
    (
      Log
    , effectuate
    , realize
    , unrealize
    ) where

import           Control.AutoUpdate (UpdateSettings (..), defaultUpdateSettings,
                     mkAutoUpdate)
import           Control.Concurrent.MVar (MVar, modifyMVar_, readMVar,
                     newMVar, withMVar)
import           Control.Exception.Safe (catchIO)
import           Control.Monad (forM, forM_, void, when)
import           Control.Lens ((^.))
import           Data.Aeson.Text (encodeToLazyText)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import           Data.List (find)
import           Data.Maybe (isNothing)
import           Data.String (fromString)
import           Data.Text (Text, isPrefixOf, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Builder (Builder, fromText, toLazyText)
import qualified Data.Text.Lazy.IO as TIO
import           Data.Time (diffUTCTime)
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Data.Version (Version (..), showVersion)
import           GHC.Conc (atomically)
import           GHC.IO.Handle (hDuplicate)
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath (takeDirectory)
import           System.IO (BufferMode (LineBuffering), Handle, hClose,
                     hSetBuffering, stderr, stdout, openFile, IOMode (WriteMode))

import qualified Katip as K
import qualified Katip.Core as KC
import           Katip.Scribes.Handle (brackets)

import qualified Cardano.BM.Configuration as Config
import           Cardano.BM.Configuration.Model (getScribes, getSetupScribes)
import           Cardano.BM.Data.Aggregated
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.MessageCounter (MessageCounter (..), resetCounters,
                     updateMessageCounters)
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Output
import           Cardano.BM.Data.Rotation (RotationParameters (..))
import           Cardano.BM.Data.Severity
import           Cardano.BM.Rotator (cleanupRotator, evalRotator,
                     initializeRotator, prtoutException)

\end{code}
%endif

\subsubsection{Internal representation}\label{code:Log}\index{Log}
\begin{code}
type LogMVar = MVar LogInternal
newtype Log = Log
    { getK :: LogMVar }

data LogInternal = LogInternal
    { kLogEnv       :: K.LogEnv
    , msgCounters   :: MessageCounter
    , configuration :: Config.Configuration }

\end{code}

\subsubsection{Log implements |effectuate|}\index{Log!instance of IsEffectuator}
\begin{code}
instance IsEffectuator Log where
    effectuate katip item = do
        let logMVar = getK katip
        c <- configuration <$> readMVar logMVar
        setupScribes <- getSetupScribes c
        selscribes <- getScribes c (lnName item)
        let selscribesFiltered =
                case lnItem item of
                    LogObject _ (LogMessage (LogItem Private _))
                        -> removePublicScribes setupScribes selscribes
                    _   -> selscribes
        forM_ selscribesFiltered $ \sc -> passN sc katip item
        -- increase the counter for the specific severity and message type
        modifyMVar_ logMVar $ \li -> return $
            li{ msgCounters = updateMessageCounters (msgCounters li) (lnItem item) }
        -- reset message counters afer 60 sec = 1 min
        resetMessageCounters logMVar 60 Warning selscribesFiltered
      where
        removePublicScribes allScribes = filter $ \sc ->
            let (_, nameD) = T.breakOn "::" sc
                -- drop "::" from the start of name
                name = T.drop 2 nameD
            in
            case find (\x -> scName x == name) allScribes of
                Nothing     -> False
                Just scribe -> scPrivacy scribe == ScPrivate
        resetMessageCounters logMVar interval sev scribes = do
            counters <- msgCounters <$> readMVar logMVar
            let start = mcStart counters
                now = case lnItem item of
                        LogObject meta _ -> tstamp meta
                diffTime = round $ diffUTCTime now start
            when (diffTime > interval) $ do
                countersObjects <- forM (HM.toList $ mcCountersMap counters) $ \(key, count) ->
                        LogObject
                            <$> (mkLOMeta sev)
                            <*> pure (LogValue (pack key) (PureI $ toInteger count))
                intervalObject <-
                    LogObject
                        <$> (mkLOMeta sev)
                        <*> pure (LogValue "time_interval_(s)" (PureI diffTime))
                let namedCounters = map (\lo -> LogNamed "#messagecounters.katip" lo)
                                        (countersObjects ++ [intervalObject])
                forM_ scribes $ \sc ->
                    forM_ namedCounters $ \namedCounter ->
                        passN sc katip namedCounter
                modifyMVar_ logMVar $ \li -> return $
                    li{ msgCounters = resetCounters now }

    handleOverflow _ = putStrLn "Notice: Katip's queue full, dropping log items!"

\end{code}

\subsubsection{Log implements backend functions}\index{Log!instance of IsBackend}
\begin{code}
instance IsBackend Log where
    typeof _ = KatipBK

    realize config = do
        let updateEnv :: K.LogEnv -> IO UTCTime -> K.LogEnv
            updateEnv le timer =
                le { K._logEnvTimer = timer, K._logEnvHost = "hostname" }
            register :: [ScribeDefinition] -> K.LogEnv -> IO K.LogEnv
            register [] le = return le
            register (defsc : dscs) le = do
                let kind      = scKind     defsc
                    name      = scName     defsc
                    rotParams = scRotation defsc
                    name'     = pack (show kind) <> "::" <> name
                scr <- createScribe kind name rotParams
                register dscs =<< K.registerScribe name' scr scribeSettings le
            mockVersion :: Version
            mockVersion = Version [0,1,0,0] []
            scribeSettings :: KC.ScribeSettings
            scribeSettings =
                let bufferSize = 5000  -- size of the queue (in log items)
                in
                KC.ScribeSettings bufferSize
            createScribe FileTextSK name rotParams = mkTextFileScribe
                                                        rotParams
                                                        (FileDescription $ unpack name)
                                                        False
            createScribe FileJsonSK name rotParams = mkJsonFileScribe
                                                        rotParams
                                                        (FileDescription $ unpack name)
                                                        False
            createScribe StdoutSK _ _ = mkStdoutScribe
            createScribe StderrSK _ _ = mkStderrScribe

        cfoKey <- Config.getOptionOrDefault config (pack "cfokey") (pack "<unknown>")
        le0 <- K.initLogEnv
                    (K.Namespace ["iohk"])
                    (fromString $ (unpack cfoKey) <> ":" <> showVersion mockVersion)
        -- request a new time 'getCurrentTime' at most 100 times a second
        timer <- mkAutoUpdate defaultUpdateSettings { updateAction = getCurrentTime, updateFreq = 10000 }
        let le1 = updateEnv le0 timer
        scribes <- getSetupScribes config
        le <- register scribes le1

        messageCounters <- resetCounters <$> getCurrentTime

        kref <- newMVar $ LogInternal le messageCounters config

        return $ Log kref

    unrealize katip = do
        le <- withMVar (getK katip) $ \k -> return (kLogEnv k)
        void $ K.closeScribes le

\end{code}

\begin{spec}
example :: IO ()
example = do
    config <- Config.setup "from_some_path.yaml"
    k <- setup config
    passN (pack (show StdoutSK)) k $ LogNamed
                                            { lnName = "test"
                                            , lnItem = LogMessage $ LogItem
                                                { liSelection = Both
                                                , liSeverity  = Info
                                                , liPayload   = "Hello!"
                                                }
                                            }
    passN (pack (show StdoutSK)) k $ LogNamed
                                            { lnName = "test"
                                            , lnItem = LogValue "cpu-no" 1
                                            }

\end{spec}

Needed instances for |katip|:
\begin{code}
deriving instance K.ToObject LogObject
deriving instance K.ToObject LogItem
deriving instance K.ToObject (Maybe LOContent)

instance KC.LogItem LogObject where
    payloadKeys _ _ = KC.AllKeys
instance KC.LogItem LogItem where
    payloadKeys _ _ = KC.AllKeys
instance KC.LogItem (Maybe LOContent) where
    payloadKeys _ _ = KC.AllKeys

\end{code}

\subsubsection{Log.passN}\label{code:passN}
The following function copies the |NamedLogItem| to the queues of all scribes
that match on their name.
Compare start of name of scribe to |(show backend <> "::")|.
This function is non-blocking.
\begin{code}
passN :: ScribeId -> Log -> NamedLogItem -> IO ()
passN backend katip namedLogItem = do
    env <- kLogEnv <$> readMVar (getK katip)
    forM_ (Map.toList $ K._logEnvScribes env) $
          \(scName, (KC.ScribeHandle _ shChan)) ->
              -- check start of name to match |ScribeKind|
                if backend `isPrefixOf` scName
                then do
                    let (LogObject lometa loitem) = lnItem namedLogItem
                    let (sev, msg, payload) = case loitem of
                                (LogMessage logItem) ->
                                     (severity lometa, liPayload logItem, Nothing)
                                (ObserveDiff _) ->
                                     let text = TL.toStrict (encodeToLazyText loitem)
                                     in
                                     (severity lometa, text, Just loitem)
                                (ObserveOpen _) ->
                                     let text = TL.toStrict (encodeToLazyText loitem)
                                     in
                                     (severity lometa, text, Just loitem)
                                (ObserveClose _) ->
                                     let text = TL.toStrict (encodeToLazyText loitem)
                                     in
                                     (severity lometa, text, Just loitem)
                                (AggregatedMessage aggregated) ->
                                     let text = T.concat $ (flip map) aggregated $ \(name, agg) ->
                                                "\n" <> name <> ": " <> pack (show agg)
                                    in
                                    (severity lometa, text, Nothing)
                                (LogValue name value) ->
                                    (severity lometa, name <> " = " <> pack (showSI value), Nothing)
                                (MonitoringEffect logitem) ->
                                     let text = TL.toStrict (encodeToLazyText logitem)
                                     in
                                     (severity lometa, text, Just loitem)
                                KillPill ->
                                    (severity lometa, "Kill pill received!", Nothing)
                    if (msg == "") && (isNothing payload)
                    then return ()
                    else do
                        let threadIdText = KC.ThreadIdText $ tid lometa
                        let ns = lnName namedLogItem
                        let itemTime = tstamp lometa
                        let itemKatip = K.Item {
                                  _itemApp       = env ^. KC.logEnvApp
                                , _itemEnv       = env ^. KC.logEnvEnv
                                , _itemSeverity  = sev2klog sev
                                , _itemThread    = threadIdText
                                , _itemHost      = env ^. KC.logEnvHost
                                , _itemProcess   = env ^. KC.logEnvPid
                                , _itemPayload   = payload
                                , _itemMessage   = K.logStr msg
                                , _itemTime      = itemTime
                                , _itemNamespace = (env ^. KC.logEnvApp) <> (K.Namespace [ns])
                                , _itemLoc       = Nothing
                                }
                        void $ atomically $ KC.tryWriteTBQueue shChan (KC.NewItem itemKatip)
                else return ()
\end{code}

\subsubsection{Scribes}
\begin{code}
mkStdoutScribe :: IO K.Scribe
mkStdoutScribe = do
    -- duplicate stdout so that Katip's closing
    -- action will not close the real stdout
    stdout' <- hDuplicate stdout
    mkTextFileScribeH stdout' True

mkStderrScribe :: IO K.Scribe
mkStderrScribe = do
    -- duplicate stderr so that Katip's closing
    -- action will not close the real stderr
    stderr' <- hDuplicate stderr
    mkTextFileScribeH stderr' True

mkTextFileScribeH :: Handle -> Bool -> IO K.Scribe
mkTextFileScribeH handler color = do
    mkFileScribeH handler formatter color
  where
    formatter h colorize verbosity item =
        TIO.hPutStrLn h $! toLazyText $ formatItem colorize verbosity item

mkFileScribeH
    :: Handle
    -> (forall a . K.LogItem a => Handle -> Bool -> K.Verbosity -> K.Item a -> IO ())
    -> Bool
    -> IO K.Scribe
mkFileScribeH h formatter colorize = do
    hSetBuffering h LineBuffering
    locklocal <- newMVar ()
    let logger :: forall a. K.LogItem a =>  K.Item a -> IO ()
        logger item = withMVar locklocal $ \_ ->
                        formatter h colorize K.V0 item
    pure $ K.Scribe logger (hClose h)

mkTextFileScribe :: Maybe RotationParameters -> FileDescription -> Bool -> IO K.Scribe
mkTextFileScribe rotParams fdesc colorize = do
    mkFileScribe rotParams fdesc formatter colorize
  where
    formatter :: Handle -> Bool -> K.Verbosity -> K.Item a -> IO Int
    formatter hdl colorize' v' item =
        case KC._itemMessage item of
                K.LogStr ""  ->
                    -- if message is empty do not output it
                    return 0
                _ -> do
                    let tmsg = toLazyText $ formatItem colorize' v' item
                    TIO.hPutStrLn hdl tmsg
                    return $ fromIntegral $ TL.length tmsg

mkJsonFileScribe :: Maybe RotationParameters -> FileDescription -> Bool -> IO K.Scribe
mkJsonFileScribe rotParams fdesc colorize = do
    mkFileScribe rotParams fdesc formatter colorize
  where
    formatter :: (K.LogItem a) => Handle -> Bool -> K.Verbosity -> K.Item a -> IO Int
    formatter h _ verbosity item = do
        let jmsg = case KC._itemMessage item of
                -- if a message is contained in item then only the
                -- message is printed and not the data
                K.LogStr ""  -> K.itemJson verbosity item
                K.LogStr msg -> K.itemJson verbosity $
                                    item { KC._itemMessage = K.logStr (""::Text)
                                         , KC._itemPayload = LogItem Both $ TL.toStrict $ toLazyText msg
                                         -- do we need the severity from meta?
                                         }
            tmsg = encodeToLazyText jmsg
        TIO.hPutStrLn h tmsg
        return $ fromIntegral $ TL.length tmsg

mkFileScribe
    :: Maybe RotationParameters
    -> FileDescription
    -> (forall a . K.LogItem a => Handle -> Bool -> K.Verbosity -> K.Item a -> IO Int)
    -> Bool
    -> IO K.Scribe
mkFileScribe (Just rotParams) fdesc formatter colorize = do
    let prefixDir = prefixPath fdesc
    (createDirectoryIfMissing True prefixDir)
        `catchIO` (prtoutException ("cannot log prefix directory: " ++ prefixDir))
    let fpath = filePath fdesc
    trp <- initializeRotator rotParams fpath
    scribestate <- newMVar trp  -- triple of (handle), (bytes remaining), (rotate time)
    -- sporadically remove old log files - every 10 seconds
    cleanup <- mkAutoUpdate defaultUpdateSettings {
                                updateAction = cleanupRotator rotParams fpath
                              , updateFreq = 10000000
                              }
    let finalizer :: IO ()
        finalizer = withMVar scribestate $
                                \(h, _, _) -> hClose h
    let logger :: forall a. K.LogItem a => K.Item a -> IO ()
        logger item =
            modifyMVar_ scribestate $ \(h, bytes, rottime) -> do
                byteswritten <- formatter h colorize K.V0 item
                -- remove old files
                cleanup
                -- detect log file rotation
                let bytes' = bytes - (toInteger $ byteswritten)
                let tdiff' = round $ diffUTCTime rottime (K._itemTime item)
                if bytes' < 0 || tdiff' < (0 :: Integer)
                    then do   -- log file rotation
                        hClose h
                        (h2, bytes2, rottime2) <- evalRotator rotParams fpath
                        return (h2, bytes2, rottime2)
                    else
                        return (h, bytes', rottime)
    return $ K.Scribe logger finalizer
-- log rotation disabled.
mkFileScribe Nothing fdesc formatter colorize = do
    let prefixDir = prefixPath fdesc
    (createDirectoryIfMissing True prefixDir)
        `catchIO` (prtoutException ("cannot log prefix directory: " ++ prefixDir))
    let fpath = filePath fdesc
    h <- catchIO (openFile fpath WriteMode) $
                        \e -> do
                            prtoutException ("error while opening log: " ++ fpath) e
                            -- fallback to standard output in case of exception
                            return stdout
    hSetBuffering h LineBuffering
    scribestate <- newMVar h
    let finalizer :: IO ()
        finalizer = withMVar scribestate hClose
    let logger :: forall a. K.LogItem a => K.Item a -> IO ()
        logger item =
            withMVar scribestate $ \handler ->
                void $ formatter handler colorize K.V0 item
    return $ K.Scribe logger finalizer

\end{code}

\begin{code}
formatItem :: Bool -> K.Verbosity -> K.Item a -> Builder
formatItem withColor _verb K.Item{..} =
    fromText header <>
    fromText " " <>
    brackets (fromText timestamp) <>
    fromText " " <>
    KC.unLogStr _itemMessage
  where
    header = colorBySeverity _itemSeverity $
             "[" <> mconcat namedcontext <> ":" <> severity <> ":" <> threadid <> "]"
    namedcontext = KC.intercalateNs _itemNamespace
    severity = KC.renderSeverity _itemSeverity
    threadid = KC.getThreadIdText _itemThread
    timestamp = pack $ formatTime defaultTimeLocale tsformat _itemTime
    tsformat :: String
    tsformat = "%F %T%2Q %Z"
    colorBySeverity s m = case s of
      K.EmergencyS -> red m
      K.AlertS     -> red m
      K.CriticalS  -> red m
      K.ErrorS     -> red m
      K.NoticeS    -> magenta m
      K.WarningS   -> yellow m
      K.InfoS      -> blue m
      _          -> m
    red = colorize "31"
    yellow = colorize "33"
    magenta = colorize "35"
    blue = colorize "34"
    colorize c m
      | withColor = "\ESC["<> c <> "m" <> m <> "\ESC[0m"
      | otherwise = m

-- translate Severity to Log.Severity
sev2klog :: Severity -> K.Severity
sev2klog = \case
    Debug     -> K.DebugS
    Info      -> K.InfoS
    Notice    -> K.NoticeS
    Warning   -> K.WarningS
    Error     -> K.ErrorS
    Critical  -> K.CriticalS
    Alert     -> K.AlertS
    Emergency -> K.EmergencyS

\end{code}

\begin{code}
data FileDescription = FileDescription {
                         filePath   :: !FilePath }
                       deriving (Show)

prefixPath :: FileDescription -> FilePath
prefixPath = takeDirectory . filePath

\end{code}
