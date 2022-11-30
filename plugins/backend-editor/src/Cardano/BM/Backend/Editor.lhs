\subsection{Cardano.BM.Backend.Editor}
\label{code:Cardano.BM.Backend.Editor}

%if style == newcode
\begin{code}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Cardano.BM.Backend.Editor
    (
      Editor
    -- * Plugin
    , plugin
    ) where

import           Prelude
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, swapMVar, readMVar, withMVar)
import           Control.Exception.Safe (IOException, SomeException, catch, handle)
import           Control.Monad  (void, when, forM_)
import           Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Time (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Safe (readMay)
import           System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import           System.FilePath ((</>))
import           System.IO (stderr)

import qualified Graphics.UI.Threepenny as Threepenny
import           Graphics.UI.Threepenny.Core (Element, UI, Window, (#), (#+), (#.))
import qualified Graphics.UI.Threepenny.Core as Threepenny

import           Cardano.BM.Configuration
import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.AggregatedKind
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.BackendKind (BackendKind (EditorBK))
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Output (ScribeId)
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.SubTrace
import           Cardano.BM.Data.Trace
import           Cardano.BM.Data.Tracer (nullTracer, traceWith)
import           Cardano.BM.Backend.LogBuffer
import           Cardano.BM.Plugin (Plugin (..))
import           Cardano.BM.Rotator (tsformat)

\end{code}
%endif

This simple configuration editor is accessible through a browser on
\url{http://127.0.0.1:13789}, or whatever port has been set in the
configuration.

A number of maps that relate logging context name to behaviour can be
changed.
And, most importantly, the global minimum severity that defines the filtering
of log messages.

\subsubsection{links}
The GUI is built on top of \emph{Threepenny-GUI} (\url{http://hackage.haskell.org/package/threepenny-gui}).
The appearance is due to \emph{w3-css} (\url{https://www.w3schools.com/w3css}).


\subsubsection{Plugin definition}
\begin{code}
plugin :: (IsEffectuator s a, ToJSON a, FromJSON a)
       => Configuration -> Trace IO a -> s a -> IO (Plugin a)
plugin config trace sb = do
    be :: Cardano.BM.Backend.Editor.Editor a <- realizefrom config trace sb
    pure $ BackendPlugin
               (MkBackend { bEffectuate = effectuate be, bUnrealize = unrealize be })
               (bekind be)
\end{code}

\subsubsection{Structure of Editor}\label{code:Editor}\index{Editor}
\begin{code}
type EditorMVar a = MVar (EditorInternal a)
newtype Editor a = Editor
    { getEd :: EditorMVar a }

data EditorInternal a = EditorInternal
    { edSBtrace :: Trace IO a
    , edThread  :: Async.Async ()
    , edBuffer  :: !(LogBuffer a)
    }

\end{code}

\subsubsection{|Editor| implements |Backend| functions}\index{Editor!instance of IsBackend}

|Editor| is an |IsBackend|
\begin{code}
instance (ToJSON a, FromJSON a) => IsBackend Editor a where
    bekind _ = EditorBK

    realize _ = fail "Editor cannot be instantiated by 'realize'"

    realizefrom config sbtrace _ = mdo
        gref <- newEmptyMVar
        let gui = Editor gref
        port <- getGUIport config
        when (port <= 0) $ fail "cannot create GUI"

        -- local |LogBuffer|
        logbuf :: Cardano.BM.Backend.LogBuffer.LogBuffer a <- Cardano.BM.Backend.LogBuffer.realize config

        thd <- Async.async $ do
                Threepenny.startGUI Threepenny.defaultConfig
                                   { Threepenny.jsPort       = Just port
                                   , Threepenny.jsAddr       = Just "127.0.0.1"
                                   , Threepenny.jsStatic     = Just "iohk-monitoring/static"
                                   , Threepenny.jsCustomHTML = Just "configuration-editor.html"
                                   } $ prepare gui config
                `catch` nullSetup sbtrace gref
                            EditorInternal
                              { edSBtrace = nullTracer
                              , edThread = thd
                              , edBuffer = logbuf
                              }
        Async.link thd
        putMVar gref $ EditorInternal
                        { edSBtrace = sbtrace
                        , edThread = thd
                        , edBuffer = logbuf
                        }
        pure gui
     where
       nullSetup
         :: Trace IO a
         -> EditorMVar a
         -> EditorInternal a
         -> SomeException
         -> IO ()
       nullSetup trace mvar nullEditor e = do
         meta <- mkLOMeta Error Public
         traceWith trace $ ("#editor.realizeFrom", LogObject "#editor.realizeFrom" meta $
           LogError $ "Editor backend disabled due to initialisation error: " <> (Text.pack $ show e))
         _ <- swapMVar mvar nullEditor
         pure ()

    unrealize editor =
        withMVar (getEd editor) $ \ed ->
            Async.cancel $ edThread ed

\end{code}

\subsubsection{Editor is an effectuator}\index{Editor!instance of IsEffectuator}
Function |effectuate| is called to pass in a |LogObject| for display in the GUI.
\begin{code}
instance IsEffectuator Editor a where
    effectuate editor item =
        withMVar (getEd editor) $ \ed ->
            effectuate (edBuffer ed) item

    handleOverflow _ = Text.hPutStrLn stderr "Notice: overflow in Editor!"

\end{code}

\subsubsection{Prepare the view}
\begin{code}

data Cmd = Backends | Scribes | Severities | SubTrace | Aggregation | Buffer | ExportConfiguration
           deriving (Enum, Eq, Show, Read)

prepare :: forall a. ToJSON a => Editor a -> Configuration -> Window -> UI ()
prepare editor config window =
  void $ do
    inputKey   <- Threepenny.input #. "w3-input w3-border" # Threepenny.set Threepenny.size "34"
    inputValue <- Threepenny.input #. "w3-input w3-border" # Threepenny.set Threepenny.size "60"
    outputMsg  <- Threepenny.input #. "w3-input w3-border"

    currentCmd <- Threepenny.p #. "current-cmd"

    disable inputKey
    disable inputValue
    disable outputMsg

    preprareCurrent editor config window inputKey inputValue outputMsg currentCmd

preprareCurrent :: forall a. ToJSON a => Editor a -> Configuration -> Window -> Element ->  Element ->  Element -> Element ->  UI ()
preprareCurrent editor config window inputKey inputValue outputMsg currentCmd = do
    Threepenny.getElementById window "main-section" >>= \case
        Nothing -> pure ()
        Just mainSection -> void $ Threepenny.element mainSection #+
            [ Threepenny.div #. "w3-panel" #+
                [ Threepenny.div #. "w3-border w3-border-dark-grey" #+
                    [ Threepenny.div #. "w3-panel" #+ [ minimumSeveritySelection ]
                    ]
                , Threepenny.div #. "w3-panel" #+ []
                , Threepenny.div #. "w3-border w3-border-dark-grey" #+
                    [ Threepenny.div #. "w3-bar w3-grey" #+ [ commandTabs ]
                    , Threepenny.div #. "w3-panel"       #+ [ mkEditInputs ]
                    , Threepenny.div #. "w3-panel"       #+ [ Threepenny.element outputMsg ]
                    ]
                ]
            ]
  where
    accessBufferMap :: UI [(LoggerName, LogObject a)]
    accessBufferMap = Threepenny.liftIO $ readBuffer =<< edBuffer <$> readMVar (getEd editor)

    addItemButton :: (Element -> UI ()) -> UI ()
    addItemButton = performActionOnId addItemButtonId

    cancelSaveItemButton :: (Element -> UI ()) -> UI ()
    cancelSaveItemButton = performActionOnId cancelSaveItemButtonId

    cleanOutputTable :: UI ()
    cleanOutputTable =
      performActionOnId outputTableId $ \t ->
        void $ Threepenny.element t # Threepenny.set Threepenny.children []

    commands :: [Cmd]
    commands = [Backends .. ]

    commandTabs :: UI Element
    commandTabs =
      Threepenny.row $ flip map commands $ \cmd -> do
        b <- Threepenny.button #. "w3-bar-item w3-button w3-grey"
                            #  Threepenny.set (Threepenny.attr "id") (show cmd)
                            #+ [ Threepenny.bold #+ [ Threepenny.string (show cmd) ] ]
        Threepenny.on Threepenny.click b $ const $ do
            cleanAndDisable inputKey
            cleanAndDisable inputValue
            clean outputMsg
            switchToTab cmd
        pure b

    displayBuffer :: ToJSON a => Cmd -> [(LoggerName, LogObject a)] -> UI ()
    displayBuffer cmd sel = do
      showCurrentTab cmd
      rememberCurrent cmd
      saveItemButton disable
      cancelSaveItemButton disable
      addItemButton disable
      cleanOutputTable
      performActionOnId outputTableId $
        \t -> void $ Threepenny.element t #+
                [ Threepenny.tr #+
                    [ Threepenny.th #+ [ Threepenny.string "LoggerName" ]
                    , Threepenny.th #+ [ Threepenny.string $ show cmd <> " value" ]
                    , Threepenny.th #+ [ Threepenny.string "" ]
                    ]
                ]
      forM_ sel $
        \(n,v) -> performActionOnId outputTableId $
          \t -> void $ Threepenny.element t #+ [ mkSimpleRow n v ]

    displayExport :: Cmd -> UI ()
    displayExport cmd = do
      showCurrentTab cmd
      rememberCurrent cmd
      saveItemButton disable
      cancelSaveItemButton disable
      addItemButton disable
      cleanOutputTable
      exportConfiguration

    displayItems :: forall b. Show b => Cmd -> (CM.ConfigurationInternal -> HashMap.HashMap LoggerName b) -> UI ()
    displayItems cmd sel = do
      showCurrentTab cmd
      rememberCurrent cmd
      saveItemButton disable
      cancelSaveItemButton disable
      addItemButton enable
      cleanOutputTable
      performActionOnId outputTableId $
        \t -> void $ Threepenny.element t #+
                [ Threepenny.tr #+
                    [ Threepenny.th #+ [ Threepenny.string "LoggerName" ]
                    , Threepenny.th #+ [ Threepenny.string $ show cmd <> " value" ]
                    , Threepenny.th #+ [ Threepenny.string "" ]
                    ]
                ]
      cg <- Threepenny.liftIO $ readMVar (CM.getCG config)
      forM_ (HashMap.toList $ sel cg) $
            \(n,v) -> performActionOnId outputTableId $
                \t -> void $ Threepenny.element t #+ [ mkTableRow cmd n v ]

    exportConfiguration :: UI ()
    exportConfiguration = do
      currentDir <- Threepenny.liftIO getCurrentDirectory
      let dir = currentDir </> "iohk-monitoring/static/conf"
      Threepenny.liftIO $ createDirectoryIfMissing True dir
      tsnow <- formatTime defaultTimeLocale tsformat <$> Threepenny.liftIO getCurrentTime
      let filename = "config.yaml" ++ "-" ++ tsnow
          filepath = dir </> filename
      res <- Threepenny.liftIO $
                handle (\(e :: IOException) -> pure $ show e) $ do
                  CM.exportConfiguration config filepath
                  pure ("Configuration was exported to the file: " ++ filepath)
      setMessage res
      performActionOnId outputTableId $
          \t -> void $ Threepenny.element t #+
                        [ mkLinkToFile "Link to configuration file" ("/static/conf" </> filename) ]

    minimumSeveritySelection :: UI Element
    minimumSeveritySelection = do
      minsev <- Threepenny.select #. "minsevfield" #+ map mkSevOption (enumFrom Debug)

      Threepenny.on Threepenny.selectionChange minsev $ setMinSev minsev

      Threepenny.row
            [ Threepenny.string "Set minimum severity to:"
            , Threepenny.span # Threepenny.set Threepenny.html "&nbsp;"
            , Threepenny.span #. "severity-dropdown big" #+ [ Threepenny.element minsev ]
            ]

    mkEditInputs :: UI Element
    mkEditInputs =
      Threepenny.row
          [ Threepenny.element inputKey
          , Threepenny.span #. "key-value-separator" #+ [Threepenny.string ":"]
          , Threepenny.element inputValue
          , Threepenny.span #. "key-value-separator" #+ [Threepenny.string ""]
          , saveItemHandler
          , Threepenny.span #. "key-value-separator" #+ [Threepenny.string ""]
          , do
              b <- Threepenny.button #. "w3-btn w3-ripple w3-lime save-item-button"
                             #  Threepenny.set (Threepenny.attr "id") saveItemButtonId
                             #  Threepenny.set Threepenny.enabled False
                             #+ [Threepenny.bold #+ [Threepenny.string "Save"]]
              Threepenny.on Threepenny.click b $ const $ do
                  k <- inputKey   # Threepenny.get Threepenny.value
                  v <- inputValue # Threepenny.get Threepenny.value
                  m <- currentCmd # Threepenny.get Threepenny.value
                  case (readMay m :: Maybe Cmd) of
                      Nothing -> setError "parse error on cmd"
                      Just c  -> do
                          cleanAndDisable inputKey
                          cleanAndDisable inputValue
                          saveItemButton disable
                          cancelSaveItemButton disable
                          setMessage $ "Setting '" ++ k ++ "' to '" ++ v ++ "' in " ++ m
                          updateItem c (Text.pack k) v
                          switchToTab c
              pure b
          , Threepenny.span #. "key-value-separator" #+ [Threepenny.string ""]
          , do
              b <- Threepenny.button #. "w3-btn w3-ripple w3-white"
                             #  Threepenny.set (Threepenny.attr "id") cancelSaveItemButtonId
                             #  Threepenny.set Threepenny.enabled False
                             #+ [Threepenny.bold #+ [Threepenny.string "Cancel"]]
              Threepenny.on Threepenny.click b $ const $ do
                  cleanAndDisable inputKey
                  cleanAndDisable inputValue
                  saveItemButton disable
                  cancelSaveItemButton disable
              pure b
          ]

    mkSevOption :: Severity -> UI Element
    mkSevOption sev = do
      confMinSev <- Threepenny.liftIO $ minSeverity config
      Threepenny.option # Threepenny.set Threepenny.text (show sev)
                        # Threepenny.set Threepenny.value (show sev)
                        # if confMinSev == sev then Threepenny.set Threepenny.selected True else id

    mkTableRow :: Show t => Cmd -> LoggerName -> t -> UI Element
    mkTableRow cmd n v =
      Threepenny.tr #. "itemrow" #+
        [ Threepenny.td #+ [ Threepenny.string (Text.unpack n) ]
        , Threepenny.td #+ [ Threepenny.string (show v) ]
        , Threepenny.td #+
            [ do
                b <- Threepenny.button #. "w3-small w3-btn w3-ripple w3-orange edit-item-button"
                                     #+ [ Threepenny.bold #+ [ Threepenny.string "Edit" ] ]
                Threepenny.on Threepenny.click b $ const $ do
                  saveItemButton enable
                  cancelSaveItemButton enable
                  clean outputMsg
                  enable inputKey
                  enable inputValue
                  setValueOf inputKey (Text.unpack n)
                  setValueOf inputValue (show v)
                  rememberCurrent cmd
                pure b
            , Threepenny.span # Threepenny.set Threepenny.html "&nbsp;&nbsp;&nbsp;"
            , do
                b <- Threepenny.button #. "w3-small w3-btn w3-ripple w3-red"
                                     #+ [ Threepenny.bold #+ [ Threepenny.string "Delete" ] ]
                Threepenny.on Threepenny.click b $ const $ do
                  Threepenny.liftIO $ removeItem cmd n
                  cleanAndDisable inputKey
                  cleanAndDisable inputValue
                  -- Initiate a click to current menu to update the items list after deleting.
                  performActionOnId (show cmd) $ Threepenny.runFunction . Threepenny.ffi "$(%1).click()"
                pure b
            ]
        ]

    performActionOnId :: String -> (Element -> UI ()) -> UI ()
    performActionOnId anId action =
      Threepenny.getElementById window anId >>= maybe (pure ()) action

    rememberCurrent :: Cmd -> UI ()
    rememberCurrent cmd = setValueOf currentCmd $ show cmd

    saveItemButton :: (Element -> UI ()) -> UI ()
    saveItemButton = performActionOnId saveItemButtonId

    setMinSev :: b -> Maybe Int -> UI ()
    setMinSev _el mi =
      maybe (pure ()) (\sev -> Threepenny.liftIO $ setMinSeverity config (toEnum sev :: Severity)) mi

    switchToTab :: Cmd -> UI ()
    switchToTab c =
      case c of
        Backends            -> displayItems c CM.cgMapBackend
        Severities          -> displayItems c CM.cgMapSeverity
        Scribes             -> displayItems c CM.cgMapScribe
        SubTrace            -> displayItems c CM.cgMapSubtrace
        Aggregation         -> displayItems c CM.cgMapAggregatedKind
        Buffer              -> accessBufferMap >>= displayBuffer c
        ExportConfiguration -> displayExport c

    removeItem :: Cmd -> LoggerName -> IO ()
    removeItem cmd name =
      case cmd of
        Backends    -> CM.setBackends       config name Nothing
        Severities  -> CM.setSeverity       config name Nothing
        Scribes     -> CM.setScribes        config name Nothing
        SubTrace    -> CM.setSubTrace       config name Nothing
        Aggregation -> CM.setAggregatedKind config name Nothing
        _otherwise  -> pure ()

    saveItemHandler :: UI Element
    saveItemHandler = do
      b <- Threepenny.button #. "w3-btn w3-ripple w3-green save-item-button"
                            #  Threepenny.set (Threepenny.attr "id") addItemButtonId
                            #  Threepenny.set Threepenny.enabled False
                            #+ [Threepenny.bold #+ [Threepenny.string "New"]]
      Threepenny.on Threepenny.click b $ const $ do
        enable inputKey
        enable inputValue
        saveItemButton enable
        cancelSaveItemButton enable
      pure b

    setError :: String -> UI ()
    setError m = setValueOf outputMsg ("ERROR: " ++ m)

    setMessage :: String -> UI ()
    setMessage m = setValueOf outputMsg m

    showCurrentTab :: Cmd -> UI ()
    showCurrentTab cmd = do
      let baseClasses = "w3-bar-item w3-button"
          classesForCurrentTab = baseClasses <> " " <> "w3-light-grey"
      performActionOnId (show cmd) $ setClasses classesForCurrentTab
      let otherTabs = List.delete cmd commands
      forM_ otherTabs $ \tabName ->
        performActionOnId (show tabName) $ setClasses baseClasses


    updateItem :: Cmd -> LoggerName -> String -> UI ()
    updateItem cmd k v =
      case cmd of
        Backends ->
          case (readMay v :: Maybe [BackendKind]) of
            Nothing -> setError "parse error on backend list"
            Just v' -> Threepenny.liftIO $ CM.setBackends config k $ Just v'
        Severities ->
          case (readMay v :: Maybe Severity) of
            Nothing -> setError "parse error on severity"
            Just v' -> Threepenny.liftIO $ CM.setSeverity config k $ Just v'
        Scribes ->
          case (readMay v :: Maybe [ScribeId]) of
            Nothing -> setError "parse error on scribe list"
            Just v' -> Threepenny.liftIO $ CM.setScribes config k $ Just v'
        SubTrace ->
          case (readMay v :: Maybe SubTrace) of
            Nothing -> setError "parse error on subtrace"
            Just v' -> Threepenny.liftIO $ CM.setSubTrace config k $ Just v'
        Aggregation ->
          case (readMay v :: Maybe AggregatedKind) of
            Nothing -> setError "parse error on aggregated kind"
            Just v' -> Threepenny.liftIO $ CM.setAggregatedKind config k $ Just v'
        _otherwise -> pure ()


-- ---------------------------------------------- ---------------------------------------------------

addItemButtonId, cancelSaveItemButtonId, saveItemButtonId, outputTableId :: String
addItemButtonId = "add-item-button"
cancelSaveItemButtonId = "cancel-save-item-button"
saveItemButtonId = "save-item-button"
outputTableId = "output-table"

clean   anElement = setValueOf anElement ""

cleanAndDisable anElement = clean anElement >> disable anElement

enable :: Element -> UI ()
enable  anElement = turn anElement True

disable :: Element -> UI ()
disable anElement = turn anElement False

mkLinkToFile :: String -> FilePath -> UI Element
mkLinkToFile str file =
  Threepenny.anchor # Threepenny.set (Threepenny.attr "href") file
                    # Threepenny.set (Threepenny.attr "target") "_blank"
                    #+ [ Threepenny.string str ]

mkSimpleRow :: ToJSON a => LoggerName -> LogObject a -> UI Element
mkSimpleRow n lo@(LogObject _lonm _lometa _lov) =
  Threepenny.tr #. "itemrow" #+
    [ Threepenny.td #+ [ Threepenny.string (Text.unpack n) ]
    , Threepenny.td #+ [ Threepenny.string $ BS8.unpack $ encode lo ]
    ]

setClasses :: String -> Element -> UI ()
setClasses classes anElement =
  void $ Threepenny.element anElement # Threepenny.set Threepenny.class_  classes

setValueOf :: Element -> String -> UI ()
setValueOf anElement aValue =
  void $ Threepenny.element anElement # Threepenny.set Threepenny.value aValue

turn :: Element -> Bool -> UI ()
turn anElement toState =
  void $ Threepenny.element anElement # Threepenny.set Threepenny.enabled toState

\end{code}
