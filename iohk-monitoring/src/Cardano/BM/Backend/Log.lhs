
\subsection{Cardano.BM.Backend.Log}
\label{code:Cardano.BM.Backend.Log}

%if style == newcode
\begin{code}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.BM.Backend.Log
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
import           Data.Aeson (FromJSON, ToJSON, Result (Success), Value (..), fromJSON)
import           Data.Aeson.Text (encodeToLazyText)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import           Data.List (find)
import           Data.Maybe (catMaybes, isNothing)
import           Data.String (fromString)
import           Data.Text (Text, isPrefixOf, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Builder (Builder, fromText, toLazyText)
import qualified Data.Text.Lazy.IO as TIO
import           Data.Time (diffUTCTime)
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Data.Version (showVersion)
import           GHC.Conc (atomically)
import           GHC.IO.Handle (hDuplicate)
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath (takeDirectory)
import           System.IO (BufferMode (LineBuffering), Handle, hClose,
                     hSetBuffering, stderr, stdout, openFile, IOMode (WriteMode))
#ifdef ENABLE_SYSLOG
import           Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL (toStrict)
import qualified Data.Text.Encoding as T (encodeUtf8)
import           Language.Haskell.TH.Syntax (Loc(Loc, loc_filename, loc_start))
import           Systemd.Journal (JournalFields, codeFile, codeLine, message,
                     mkJournalField, priority, sendJournalFields,
                     syslogFacility, syslogIdentifier)
import qualified Systemd.Journal as J
import           System.Posix.Syslog (Facility)
#endif

import           Paths_iohk_monitoring (version)

import qualified Katip as K
import qualified Katip.Core as KC
import           Katip.Scribes.Handle (brackets)
#ifdef ENABLE_SYSLOG
import           Katip.Format.Time (formatAsIso8601)
#endif

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
import           Cardano.BM.Data.Tracer (ToObject (..))
import           Cardano.BM.Rotator (cleanupRotator, evalRotator,
                     initializeRotator, prtoutException)

\end{code}
%endif

\subsubsection{Internal representation}\label{code:Log}\index{Log}
\begin{code}
type LogMVar = MVar LogInternal
newtype Log a = Log
    { getK :: LogMVar }

data LogInternal = LogInternal
    { kLogEnv       :: K.LogEnv
    , msgCounters   :: MessageCounter
    , configuration :: Config.Configuration }

\end{code}

\subsubsection{Log implements |effectuate|}\index{Log!instance of IsEffectuator}
\begin{code}
instance ToObject a => IsEffectuator Log a where
    effectuate katip item = do
        let logMVar = getK katip
        c <- configuration <$> readMVar logMVar
        setupScribes <- getSetupScribes c
        selscribes <- getScribes c (loName item)
        let selscribesFiltered =
                case item of
                    LogObject _ (LOMeta _ _ _ Confidential) (LogMessage _)
                        -> removePublicScribes setupScribes selscribes
                    _   -> selscribes
        forM_ selscribesFiltered $ \sc -> passN sc katip item
        -- increase the counter for the specific severity and message type
        modifyMVar_ logMVar $ \li -> return $
            li{ msgCounters = updateMessageCounters (msgCounters li) item }
        -- reset message counters afer 60 sec = 1 min
        resetMessageCounters logMVar c 60 Warning selscribesFiltered
      where
        removePublicScribes allScribes = filter $ \sc ->
            let (_, nameD) = T.breakOn "::" sc
                -- drop "::" from the start of name
                name = T.drop 2 nameD
            in
            case find (\x -> scName x == name) allScribes of
                Nothing     -> False
                Just scribe -> scPrivacy scribe == ScPrivate
        resetMessageCounters logMVar cfg interval sev scribes = do
            counters <- msgCounters <$> readMVar logMVar
            let start = mcStart counters
                now = case item of
                        LogObject _ meta _ -> tstamp meta
                diffTime = round $ diffUTCTime now start
            when (diffTime > interval) $ do
                let counterName = "#messagecounters.katip"
                countersObjects <- forM (HM.toList $ mcCountersMap counters) $ \(key, count) ->
                        LogObject
                            <$> pure counterName
                            <*> (mkLOMeta sev Confidential)
                            <*> pure (LogValue key (PureI $ toInteger count))
                intervalObject <-
                    LogObject
                        <$> pure counterName
                        <*> (mkLOMeta sev Confidential)
                        <*> pure (LogValue "time_interval_(s)" (PureI diffTime))
                let namedCounters = countersObjects ++ [intervalObject]
                namedCountersFiltered <- catMaybes <$> (forM namedCounters $ \obj -> do
                    mayObj <- Config.testSubTrace cfg counterName obj
                    case mayObj of
                        Just o -> do
                            passSevFilter <- Config.testSeverity cfg counterName $ loMeta o
                            if passSevFilter
                            then return $ Just o
                            else return Nothing
                        Nothing -> return Nothing)
                forM_ scribes $ \sc ->
                    forM_ namedCountersFiltered $ \namedCounter ->
                        passN sc katip namedCounter
                modifyMVar_ logMVar $ \li -> return $
                    li{ msgCounters = resetCounters now }

    handleOverflow _ = TIO.hPutStrLn stderr "Notice: Katip's queue full, dropping log items!"

\end{code}

\subsubsection{Log implements backend functions}\index{Log!instance of IsBackend}
\begin{code}
instance (ToObject a, FromJSON a) => IsBackend Log a where
    typeof _ = KatipBK

    realize config = do
        let updateEnv :: K.LogEnv -> IO UTCTime -> K.LogEnv
            updateEnv le timer =
                le { K._logEnvTimer = timer, K._logEnvHost = "hostname" }
            register :: [ScribeDefinition] -> K.LogEnv -> IO K.LogEnv
            register [] le = return le
            register (defsc : dscs) le = do
                let kind      = scKind     defsc
                    sctype    = scFormat   defsc
                    name      = scName     defsc
                    rotParams = scRotation defsc
                    name'     = pack (show kind) <> "::" <> name
                scr <- createScribe kind sctype name rotParams
                register dscs =<< K.registerScribe name' scr scribeSettings le
            scribeSettings :: KC.ScribeSettings
            scribeSettings =
                let bufferSize = 5000  -- size of the queue (in log items)
                in
                KC.ScribeSettings bufferSize
            createScribe FileSK ScText name rotParams = mkTextFileScribe
                                                            rotParams
                                                            (FileDescription $ unpack name)
                                                            False
            createScribe FileSK ScJson name rotParams = mkJsonFileScribe
                                                            rotParams
                                                            (FileDescription $ unpack name)
                                                            False
#if defined(ENABLE_SYSLOG)
            createScribe JournalSK _ _ _ = mkJournalScribe
#endif
            createScribe StdoutSK sctype _ _ = mkStdoutScribe sctype
            createScribe StderrSK sctype _ _ = mkStderrScribe sctype
            createScribe DevNullSK _ _ _ = mkDevNullScribe

        cfoKey <- Config.getOptionOrDefault config (pack "cfokey") (pack "<unknown>")
        le0 <- K.initLogEnv
                    (K.Namespace ["iohk"])
                    (fromString $ (unpack cfoKey) <> ":" <> showVersion version)
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
    meta <- mkLOMeta Info Public
    passN (pack (show StdoutSK)) k $ LogObject
                                            { loName = "test"
                                            , loMeta = meta
                                            , loContent = LogMessage "Hello!"
                                            }
    meta' <- mkLOMeta Info Public
    passN (pack (show StdoutSK)) k $ LogObject
                                            { loName = "test"
                                            , loMeta = meta'
                                            , loContent = LogValue "cpu-no" 1
                                            }

\end{spec}

Needed instances for |katip|:
\begin{code}
deriving instance ToJSON a => K.ToObject (LogObject a)
deriving instance K.ToObject Text
deriving instance ToJSON a => K.ToObject (Maybe (LOContent a))

instance ToJSON a => KC.LogItem (LogObject a) where
    payloadKeys _ _ = KC.AllKeys
instance KC.LogItem Text where
    payloadKeys _ _ = KC.AllKeys
instance ToJSON a => KC.LogItem (Maybe (LOContent a)) where
    payloadKeys _ _ = KC.AllKeys

\end{code}

\subsubsection{Log.passN}\label{code:passN}
The following function copies the |LogObject| to the queues of all scribes
that match on their name.
Compare start of name of scribe to |(show backend <> "::")|.
This function is non-blocking.
\begin{code}
passN :: ToObject a => ScribeId -> Log a -> LogObject a -> IO ()
passN backend katip (LogObject loname lometa loitem) = do
    env <- kLogEnv <$> readMVar (getK katip)
    forM_ (Map.toList $ K._logEnvScribes env) $
          \(scName, (KC.ScribeHandle _ shChan)) ->
              -- check start of name to match |ScribeKind|
                if backend `isPrefixOf` scName
                then do
                    let (sev, msg, payload) = case loitem of
                                (LogMessage logItem) ->
                                     let loobj = toObject logItem
                                         (text,maylo) = case (HM.lookup "string" loobj) of
                                            Just (String m)  -> (m, Nothing)
                                            Just m           -> (TL.toStrict $ encodeToLazyText m, Nothing)
                                            Nothing          -> ("", Just loitem)
                                     in
                                     (severity lometa, text, maylo)
                                (LogError text) ->
                                     (severity lometa, text, Nothing)
                                (ObserveDiff _) ->
                                     let text = TL.toStrict (encodeToLazyText (toObject loitem))
                                     in
                                     (severity lometa, text, Just loitem)
                                (ObserveOpen _) ->
                                     let text = TL.toStrict (encodeToLazyText (toObject loitem))
                                     in
                                     (severity lometa, text, Just loitem)
                                (ObserveClose _) ->
                                     let text = TL.toStrict (encodeToLazyText (toObject loitem))
                                     in
                                     (severity lometa, text, Just loitem)
                                (AggregatedMessage aggregated) ->
                                     let text = T.concat $ (flip map) aggregated $ \(name, agg) ->
                                                "\n" <> name <> ": " <> pack (show agg)
                                    in
                                    (severity lometa, text, Nothing)
                                (LogValue name value) ->
                                    (severity lometa, name <> " = " <> pack (showSI value), Nothing)
                                (MonitoringEffect _) ->
                                     let text = TL.toStrict (encodeToLazyText (toObject loitem))
                                     in
                                     (severity lometa, text, Just loitem)
                                KillPill ->
                                    (severity lometa, "Kill pill received!", Nothing)
                                Command _ ->
                                    (severity lometa, "Command received!", Nothing)
                    if (msg == "") && (isNothing payload)
                    then return ()
                    else do
                        let threadIdText = KC.ThreadIdText $ tid lometa
                        let itemTime = tstamp lometa
                        let localname = T.split (== '.') loname
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
                                , _itemNamespace = (env ^. KC.logEnvApp) <> (K.Namespace localname)
                                , _itemLoc       = Nothing
                                }
                        void $ atomically $ KC.tryWriteTBQueue shChan (KC.NewItem itemKatip)
                else return ()
\end{code}

\subsubsection{Scribes}
The handles to \emph{stdout} and \emph{stderr} will be duplicated
because on exit \emph{katip} will close them otherwise.

\begin{code}
mkStdoutScribe :: ScribeFormat -> IO K.Scribe
mkStdoutScribe ScText = do
    stdout' <- hDuplicate stdout
    mkTextFileScribeH stdout' True
mkStdoutScribe ScJson = do
    stdout' <- hDuplicate stdout
    mkJsonFileScribeH stdout' True

mkStderrScribe :: ScribeFormat -> IO K.Scribe
mkStderrScribe ScText = do
    stderr' <- hDuplicate stderr
    mkTextFileScribeH stderr' True
mkStderrScribe ScJson = do
    stderr' <- hDuplicate stderr
    mkJsonFileScribeH stderr' True

mkDevNullScribe :: IO K.Scribe
mkDevNullScribe = do
    let logger _ = pure ()
    pure $ K.Scribe logger (pure ())

mkTextFileScribeH :: Handle -> Bool -> IO K.Scribe
mkTextFileScribeH handler color = do
    mkFileScribeH handler formatter color
  where
    formatter h r =
        let (_, msg) = renderTextMsg r
        in TIO.hPutStrLn h $! msg
mkJsonFileScribeH :: Handle -> Bool -> IO K.Scribe
mkJsonFileScribeH handler color = do
    mkFileScribeH handler formatter color
  where
    formatter h r =
        let (_, msg) = renderJsonMsg r
        in TIO.hPutStrLn h $! msg

mkFileScribeH
    :: Handle
    -> (forall a . K.LogItem a => Handle -> Rendering a -> IO ())
    -> Bool
    -> IO K.Scribe
mkFileScribeH h formatter colorize = do
    hSetBuffering h LineBuffering
    locklocal <- newMVar ()
    let logger :: forall a. K.LogItem a =>  K.Item a -> IO ()
        logger item = withMVar locklocal $ \_ ->
                        formatter h (Rendering colorize K.V0 item)
    pure $ K.Scribe logger (hClose h)

data Rendering a = Rendering { colorize  :: Bool
                             , verbosity :: K.Verbosity
                             , logitem   :: K.Item a
                             }

renderTextMsg :: (K.LogItem a) => Rendering a -> (Int, TL.Text)
renderTextMsg r =
    let m = toLazyText $ formatItem (colorize r) (verbosity r) (logitem r)
    in (fromIntegral $ TL.length m, m)

renderJsonMsg :: (K.LogItem a) => Rendering a -> (Int, TL.Text)
renderJsonMsg r =
    let m' = encodeToLazyText $ trimTime $ K.itemJson (verbosity r) (logitem r)
    in (fromIntegral $ TL.length m', m')

-- keep only two digits for the fraction of seconds
trimTime :: Value -> Value
trimTime (Object o) = Object $ HM.adjust
                                keep2Decimals
                                "at"
                                o
  where
    keep2Decimals :: Value -> Value
    keep2Decimals v = case fromJSON v of
                        Success (utct :: UTCTime) ->
                            String $ pack $ formatTime defaultTimeLocale jformat utct
                        _ -> v
    jformat :: String
    jformat = "%FT%T%2QZ"
trimTime v = v

mkTextFileScribe :: Maybe RotationParameters -> FileDescription -> Bool -> IO K.Scribe
mkTextFileScribe rotParams fdesc colorize = do
    mkFileScribe rotParams fdesc formatter colorize
  where
    formatter :: (K.LogItem a) => Handle -> Rendering a -> IO Int
    formatter hdl r =
        case KC._itemMessage (logitem r) of
                K.LogStr ""  ->
                    -- if message is empty do not output it
                    return 0
                _ -> do
                    let (mlen, tmsg) = renderTextMsg r
                    TIO.hPutStrLn hdl tmsg
                    return mlen

mkJsonFileScribe :: Maybe RotationParameters -> FileDescription -> Bool -> IO K.Scribe
mkJsonFileScribe rotParams fdesc colorize = do
    mkFileScribe rotParams fdesc formatter colorize
  where
    formatter :: (K.LogItem a) => Handle -> Rendering a -> IO Int
    formatter h r = do
        let (mlen, tmsg) = renderJsonMsg r
        TIO.hPutStrLn h tmsg
        return mlen

mkFileScribe
    :: Maybe RotationParameters
    -> FileDescription
    -> (forall a . K.LogItem a => Handle -> Rendering a -> IO Int)
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
                byteswritten <- formatter h (Rendering colorize K.V0 item)
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
                void $ formatter handler (Rendering colorize K.V0 item)
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

\begin{code}
#ifdef ENABLE_SYSLOG
mkJournalScribe :: IO K.Scribe
mkJournalScribe = return $ journalScribe Nothing (sev2klog Debug) K.V3

-- taken from https://github.com/haskell-service/katip-libsystemd-journal
journalScribe :: Maybe Facility
              -> K.Severity
              -> K.Verbosity
              -> K.Scribe
journalScribe facility severity verbosity = K.Scribe liPush scribeFinalizer
  where
    liPush :: K.LogItem a => K.Item a -> IO ()
    liPush i = when (K.permitItem severity i) $
        sendJournalFields $ itemToJournalFields facility verbosity i

    scribeFinalizer :: IO ()
    scribeFinalizer = pure ()

\end{code}

Converts a |Katip Item| into a libsystemd-journal |JournalFields| map.
\begin{code}
itemToJournalFields :: K.LogItem a
                    => Maybe Facility
                    -> K.Verbosity
                    -> K.Item a
                    -> JournalFields
itemToJournalFields facility verbosity item = mconcat [ defaultFields item
                                                      , maybe HM.empty facilityFields facility
                                                      , maybe HM.empty locFields (K._itemLoc item)
                                                      ]
  where
    defaultFields kItem =
        mconcat [ message (TL.toStrict $ toLazyText $ KC.unLogStr (KC._itemMessage kItem))
                , priority (mapSeverity (KC._itemSeverity kItem))
                , syslogIdentifier (unNS (KC._itemApp kItem))
                , HM.fromList [ (environment, T.encodeUtf8 $ KC.getEnvironment (KC._itemEnv kItem))
                              , (namespace, T.encodeUtf8 $ unNS (KC._itemNamespace kItem))
                              , (payload, BL.toStrict $ encode $ KC.payloadObject verbosity (KC._itemPayload kItem))
                              , (thread, T.encodeUtf8 $ KC.getThreadIdText (KC._itemThread kItem))
                              , (time, T.encodeUtf8 $ formatAsIso8601 (KC._itemTime kItem))
                              ]
                ]
    facilityFields = syslogFacility
    locFields Loc{..} = mconcat [ codeFile loc_filename
                                , codeLine (fst loc_start)
                                ]

    environment = mkJournalField "environment"
    namespace = mkJournalField "namespace"
    payload = mkJournalField "payload"
    thread = mkJournalField "thread"
    time = mkJournalField "time"

    unNS ns = case K.unNamespace ns of
        []  -> T.empty
        [p] -> p
        parts -> T.intercalate "." parts

    mapSeverity s = case s of
        K.DebugS     -> J.Debug
        K.InfoS      -> J.Info
        K.NoticeS    -> J.Notice
        K.WarningS   -> J.Warning
        K.ErrorS     -> J.Error
        K.CriticalS  -> J.Critical
        K.AlertS     -> J.Alert
        K.EmergencyS -> J.Emergency

#endif

\end{code}
