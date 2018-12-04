
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
    , setup
    --, pass
    , passN
    --, takedown
    , example
    ) where

import           Control.AutoUpdate (UpdateSettings (..), defaultUpdateSettings,
                     mkAutoUpdate)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar,
                     withMVar)
import           Control.Exception (Exception (..))
import           Control.Exception.Safe (catchIO)
import           Control.Monad (forM_)
import           Control.Lens ((^.))
import           Data.Aeson.Text (encodeToLazyText)
import qualified Data.Map as Map
import           Data.String (fromString)
import           Data.Text (Text, isPrefixOf, pack, unpack)
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
import           Cardano.BM.Configuration.Model (setDefaultBackends)
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Output
import           Cardano.BM.Data.Severity

\end{code}
%endif

Log is a singleton.
\begin{code}
type KatipMVar = MVar KatipInternal
newtype Log = Log
    { getK :: KatipMVar }

-- Our internal state
data KatipInternal = KatipInternal
    { kLogEnv    :: K.LogEnv }

\end{code}

\begin{code}
instance HasPass Log where
    pass _ _ = pure () -- error "use passN"

\end{code}

Setup |katip| and its scribes according to the configuration
\begin{code}
setup :: Config.Configuration -> IO Log
setup config = do
    cfoKey <- Config.getOptionOrDefault config (pack "cfokey") (pack "<unknown>")
    -- TODO setup katip
    le0 <- K.initLogEnv
                (K.Namespace ["ouroboros-bm"])
                (fromString $ (unpack cfoKey) <> ":" <> showVersion mockVersion)
    -- request a new time 'getCurrentTime' at most 100 times a second
    timer <- mkAutoUpdate defaultUpdateSettings { updateAction = getCurrentTime, updateFreq = 10000 }
    let le1 = updateEnv le0 timer
    stdoutScribe <- mkStdoutScribeJson K.V0
    le <- register [(StdoutSK, "stdout", stdoutScribe)] le1

    kref <- newEmptyMVar
    putMVar kref $ KatipInternal le
    let katipref = Log kref
    setDefaultBackends config
        [ MkBackend {pass' = Cardano.BM.Output.Log.passN (pack (show StdoutSK  )) katipref}
        , MkBackend {pass' = Cardano.BM.Output.Log.passN (pack (show FileTextSK)) katipref}
        , MkBackend {pass' = Cardano.BM.Output.Log.passN (pack (show FileJsonSK)) katipref}
        ]

    return katipref
  where
    updateEnv :: K.LogEnv -> IO UTCTime -> K.LogEnv
    updateEnv le timer =
        le { K._logEnvTimer = timer, K._logEnvHost = "hostname" }
    register :: [(ScribeKind, Text, K.Scribe)] -> K.LogEnv -> IO K.LogEnv
    register [] le = return le
    register ((kind, name, scribe) : scs) le =
        let name' = pack (show kind) <> "::" <> name in
        register scs =<< K.registerScribe name' scribe scribeSettings le
    mockVersion :: Version
    mockVersion = Version [0,1,0,0] []
    scribeSettings :: KC.ScribeSettings
    scribeSettings = KC.ScribeSettings bufferSize
      where
        bufferSize = 5000  -- size of the queue (in log items)

\end{code}

\begin{code}
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
\end{code}

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

\begin{code}
passN :: Text -> Log -> NamedLogItem -> IO ()
passN backend katip namedLogItem = withMVar (getK katip) $ \k -> do
    -- TODO go through list of registered scribes
    --      and put into queue of scribe if backend kind matches
    --      compare start of name of scribe to (show backend <> "::")
    let env = kLogEnv k
    forM_ (Map.toList $ K._logEnvScribes env) $
          \(scName, (KC.ScribeHandle _ shChan)) ->
              -- check start of name to match |ScribeKind|
                if backend `isPrefixOf` scName
                then do
                    let item = lnItem namedLogItem
                    let (sev, msg, payload) = case item of
                                (LP (LogMessage logItem)) ->
                                     (liSeverity logItem, liPayload logItem, Nothing)
                                _ -> (Info, "", Just item)
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
                    atomically $ KC.tryWriteTBQueue shChan (KC.NewItem itemKatip)
                else return False
\end{code}

\subsubsection{Scribes}
\begin{code}
mkStdoutScribe :: K.Verbosity -> IO K.Scribe
mkStdoutScribe = mkTextFileScribeH stdout True

mkStdoutScribeJson :: K.Verbosity -> IO K.Scribe
mkStdoutScribeJson = mkJsonFileScribeH stdout True

mkStderrScribe :: K.Verbosity -> IO K.Scribe
mkStderrScribe = mkTextFileScribeH stderr True

mkJsonFileScribeH :: Handle -> Bool -> K.Verbosity -> IO K.Scribe
mkJsonFileScribeH handler color verb = do
    mkFileScribeH handler formatter color verb
  where
    formatter :: (K.LogItem a) => Handle -> Bool -> K.Verbosity -> K.Item a -> IO ()
    formatter h _ verbosity item = do
        let tmsg = case KC._itemMessage item of
                K.LogStr ""  -> K.itemJson verbosity item
                K.LogStr msg -> K.itemJson verbosity $
                                    item { KC._itemMessage = K.logStr (""::Text)
                                         , KC._itemPayload = LogItem Both Info $ toStrict $ toLazyText msg
                                         }
        TIO.hPutStrLn h (encodeToLazyText tmsg)

mkTextFileScribeH :: Handle -> Bool -> K.Verbosity -> IO K.Scribe
mkTextFileScribeH handler color verb = do
    mkFileScribeH handler formatter color verb
  where
    formatter h colorize verbosity item =
        TIO.hPutStrLn h $! toLazyText $ formatItem colorize verbosity item

mkFileScribeH
    :: Handle
    -> (forall a . K.LogItem a => Handle -> Bool -> K.Verbosity -> K.Item a -> IO ())  -- format and output function
    -> Bool  -- whether the output is colourized
    -> K.Verbosity
    -> IO K.Scribe
mkFileScribeH h formatter colorize verbosity = do
    hSetBuffering h LineBuffering
    locklocal <- newMVar ()
    let logger :: forall a. K.LogItem a =>  K.Item a -> IO ()
        logger item = withMVar locklocal $ \_ ->
                        formatter h colorize verbosity item
    pure $ K.Scribe logger (hClose h)

mkTextFileScribe :: FileDescription -> Bool -> Severity -> K.Verbosity -> IO K.Scribe
mkTextFileScribe fdesc colorize s v = do
    mkFileScribe fdesc formatter colorize s v
  where
    formatter :: Handle -> Bool -> K.Verbosity -> K.Item a -> IO ()
    formatter hdl colorize' v' item = do
        let tmsg = toLazyText $ formatItem colorize' v' item
        TIO.hPutStrLn hdl tmsg

mkFileScribe
    :: FileDescription
    -> (forall a . K.LogItem a => Handle -> Bool -> K.Verbosity -> K.Item a -> IO ())  -- format and output function, returns written bytes
    -> Bool  -- whether the output is colourized
    -> Severity
    -> K.Verbosity
    -> IO K.Scribe
mkFileScribe fdesc formatter colorize _ v = do
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
                  formatter handler colorize v item
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
    Debug   -> K.DebugS
    Info    -> K.InfoS
    Notice  -> K.NoticeS
    Warning -> K.WarningS
    Error   -> K.ErrorS

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
