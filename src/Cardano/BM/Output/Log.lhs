
\subsection{Cardano.BM.Output.Log}

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
import           Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar,
                     withMVar)
import           Control.Exception (Exception (..))
import           Control.Exception.Safe (catchIO)
import           Control.Monad (forM_, void)
import           Control.Lens ((^.))
import           Data.Aeson.Text (encodeToLazyText)
import qualified Data.Map as Map
import           Data.Maybe (isNothing)
import           Data.String (fromString)
import           Data.Text (Text, isPrefixOf, pack, unpack)
import qualified Data.Text as T
import           Data.Text.Lazy.Builder (Builder, fromText, toLazyText)
import           Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy.IO as TIO
import           Data.Time (UTCTime, getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Data.Version (Version (..), showVersion)
import           GHC.Conc (atomically, myThreadId)
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath (takeDirectory)
import           System.IO (BufferMode (LineBuffering), Handle, hClose,
                     hSetBuffering, stderr, stdout, openFile, IOMode (WriteMode))

import qualified Katip as K
import qualified Katip.Core as KC
import           Katip.Scribes.Handle (brackets)

import qualified Cardano.BM.Configuration as Config
import           Cardano.BM.Configuration.Model (getScribes, getSetupScribes)
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Output
import           Cardano.BM.Data.Severity

\end{code}
%endif

\subsubsection{Internal representation}\label{code:Log}
\begin{code}
type LogMVar = MVar LogInternal
newtype Log = Log
    { getK :: LogMVar }

data LogInternal = LogInternal
    { kLogEnv       :: K.LogEnv
    , configuration :: Config.Configuration }

\end{code}

\subsubsection{Log implements |effectuate|}
\begin{code}
instance IsEffectuator Log where
    effectuate katip item = do
        c <- withMVar (getK katip) $ \k -> return (configuration k)
        selscribes <- getScribes c (lnName item)
        forM_ selscribes $ \sc -> passN sc katip item

\end{code}

\subsubsection{Log implements backend functions}
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
                let kind = scKind defsc
                    name = scName defsc
                    name' = pack (show kind) <> "::" <> name
                scr <- createScribe kind name
                register dscs =<< K.registerScribe name' scr scribeSettings le
            mockVersion :: Version
            mockVersion = Version [0,1,0,0] []
            scribeSettings :: KC.ScribeSettings
            scribeSettings =
                let bufferSize = 5000  -- size of the queue (in log items)
                in
                KC.ScribeSettings bufferSize
            createScribe FileTextSK name = mkTextFileScribe  (FileDescription $ unpack name) False
            createScribe FileJsonSK name = mkJsonFileScribe  (FileDescription $ unpack name) False
            createScribe StdoutSK _ = mkStdoutScribe
            createScribe StderrSK _ = mkStderrScribe

        cfoKey <- Config.getOptionOrDefault config (pack "cfokey") (pack "<unknown>")
        le0 <- K.initLogEnv
                    (K.Namespace ["iohk"])
                    (fromString $ (unpack cfoKey) <> ":" <> showVersion mockVersion)
        -- request a new time 'getCurrentTime' at most 100 times a second
        timer <- mkAutoUpdate defaultUpdateSettings { updateAction = getCurrentTime, updateFreq = 10000 }
        let le1 = updateEnv le0 timer
        scribes <- getSetupScribes config
        le <- register scribes le1

        kref <- newEmptyMVar
        putMVar kref $ LogInternal le config

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
                                            , lnItem = LP $ LogMessage $ LogItem
                                                { liSelection = Both
                                                , liSeverity  = Info
                                                , liPayload   = "Hello!"
                                                }
                                            }
    passN (pack (show StdoutSK)) k $ LogNamed
                                            { lnName = "test"
                                            , lnItem = LP $ LogValue "cpu-no" 1
                                            }
\end{spec}

\begin{code}
-- useful instances for katip
deriving instance K.ToObject LogObject
deriving instance K.ToObject LogItem
deriving instance K.ToObject (Maybe LogObject)

instance KC.LogItem LogObject where
    payloadKeys _ _ = KC.AllKeys
instance KC.LogItem LogItem where
    payloadKeys _ _ = KC.AllKeys
instance KC.LogItem (Maybe LogObject) where
    payloadKeys _ _ = KC.AllKeys

\end{code}

\subsubsection{Log.passN}\label{code:passN}
The following function copies the |NamedLogItem| to the queues of all scribes
that match on their name.
Compare start of name of scribe to |(show backend <> "::")|.
This function is non-blocking.
\begin{code}
passN :: Text -> Log -> NamedLogItem -> IO ()
passN backend katip namedLogItem = do
    env <- withMVar (getK katip) $ \k -> return (kLogEnv k)
    forM_ (Map.toList $ K._logEnvScribes env) $
          \(scName, (KC.ScribeHandle _ shChan)) ->
              -- check start of name to match |ScribeKind|
                if backend `isPrefixOf` scName
                then do
                    let item = lnItem namedLogItem
                    let (sev, msg, payload) = case item of
                                (LP (LogMessage logItem)) ->
                                     (liSeverity logItem, liPayload logItem, Nothing)
                                (AggregatedMessage aggregated) ->
                                    let
                                        text = T.concat $ (flip map) aggregated $ \(name, agg) ->
                                                "\n" <> name <> ": " <> pack (show agg)
                                    in
                                    (Info, text, Nothing)
                                (LP (LogValue name value)) ->
                                    (Debug, name <> " = " <> pack (show value), Nothing)
                                KillPill ->
                                    (Info, "Kill pill received!", Nothing)
                                _ -> (Info, "", (Nothing :: Maybe LogObject))
                    if (msg == "") && (isNothing payload)
                    then return ()
                    else do
                        threadIdText <- KC.mkThreadIdText <$> myThreadId
                        let ns = lnName namedLogItem
                        itemTime <- env ^. KC.logEnvTimer
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
mkStdoutScribe = mkTextFileScribeH stdout True

mkStderrScribe :: IO K.Scribe
mkStderrScribe = mkTextFileScribeH stderr True

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

mkTextFileScribe :: FileDescription -> Bool -> IO K.Scribe
mkTextFileScribe fdesc colorize = do
    mkFileScribe fdesc formatter colorize
  where
    formatter :: Handle -> Bool -> K.Verbosity -> K.Item a -> IO ()
    formatter hdl colorize' v' item =
        case KC._itemMessage item of
                K.LogStr ""  ->
                    -- if message is empty do not output it
                    return ()
                _ -> do
                    let tmsg = toLazyText $ formatItem colorize' v' item
                    TIO.hPutStrLn hdl tmsg

mkJsonFileScribe :: FileDescription -> Bool -> IO K.Scribe
mkJsonFileScribe fdesc colorize = do
    mkFileScribe fdesc formatter colorize
  where
    formatter :: (K.LogItem a) => Handle -> Bool -> K.Verbosity -> K.Item a -> IO ()
    formatter h _ verbosity item = do
        let tmsg = case KC._itemMessage item of
                -- if a message is contained in item then only the
                -- message is printed and not the data
                K.LogStr ""  -> K.itemJson verbosity item
                K.LogStr msg -> K.itemJson verbosity $
                                    item { KC._itemMessage = K.logStr (""::Text)
                                         , KC._itemPayload = LogItem Both Info $ toStrict $ toLazyText msg
                                         }
        TIO.hPutStrLn h (encodeToLazyText tmsg)

mkFileScribe
    :: FileDescription
    -> (forall a . K.LogItem a => Handle -> Bool -> K.Verbosity -> K.Item a -> IO ())
    -> Bool
    -> IO K.Scribe
mkFileScribe fdesc formatter colorize = do
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
                  formatter handler colorize K.V0 item
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

-- display message and stack trace of exception on stdout
prtoutException :: Exception e => String -> e -> IO ()
prtoutException msg e = do
    putStrLn msg
    putStrLn ("exception: " ++ displayException e)

\end{code}
