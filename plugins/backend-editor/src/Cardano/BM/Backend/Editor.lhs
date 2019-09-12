\subsection{Cardano.BM.Backend.Editor}
\label{code:Cardano.BM.Backend.Editor}

%if style == newcode
\begin{code}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Cardano.BM.Backend.Editor
    (
      Editor
    , effectuate
    , realizefrom
    , unrealize
    -- * Plugin
    , plugin
    ) where

import           Prelude hiding (lookup)
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar, withMVar)
import           Control.Exception.Safe (SomeException, catch)
import           Control.Monad  (void, when, forM_)
import           Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.HashMap.Strict as HM
import           Data.List (delete)
import           Data.Text (pack, unpack)
import qualified Data.Text.IO as TIO
import           Data.Time (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Safe (readMay)
import           System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import           System.FilePath ((</>))
import           System.IO (stderr)

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core hiding (delete)

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
plugin :: (IsEffectuator s a, ToJSON a, FromJSON a) => Configuration -> Trace IO a -> s a -> IO (Plugin a)
plugin config trace sb = do
    be :: Cardano.BM.Backend.Editor.Editor a <- realizefrom config trace sb
    return $ BackendPlugin
               (MkBackend { bEffectuate = effectuate be, bUnrealize = unrealize be })
               "editor"
\end{code}

\subsubsection{Structure of Editor}\label{code:Editor}\index{Editor}
\begin{code}
type EditorMVar a = MVar (EditorInternal a)
newtype Editor a = Editor
    { getEd :: EditorMVar a }

data EditorInternal a = EditorInternal
    { edSBtrace :: Trace IO a
    , edThread  :: Async.Async ()
    , edBuffer  :: LogBuffer a
    }

\end{code}

\subsubsection{|Editor| implements |Backend| functions}\index{Editor!instance of IsBackend}

|Editor| is an |IsBackend|
\begin{code}
instance (ToJSON a, FromJSON a) => IsBackend Editor a where
    typeof _ = EditorBK

    realize _ = fail "Editor cannot be instantiated by 'realize'"

    realizefrom config sbtrace _ = do
        gref <- newEmptyMVar
        let gui = Editor gref
        port <- getGUIport config
        when (port <= 0) $ fail "cannot create GUI"

        -- local |LogBuffer|
        logbuf :: Cardano.BM.Backend.LogBuffer.LogBuffer a <- Cardano.BM.Backend.LogBuffer.realize config

        thd <- Async.async $
            startGUI defaultConfig { jsPort       = Just port
                                   , jsAddr       = Just "127.0.0.1"
                                   , jsStatic     = Just "iohk-monitoring/static"
                                   , jsCustomHTML = Just "configuration-editor.html"
                                   } $ prepare gui config
        Async.link thd
        putMVar gref $ EditorInternal
                        { edSBtrace = sbtrace
                        , edThread = thd
                        , edBuffer = logbuf
                        }
        return gui

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

    handleOverflow _ = TIO.hPutStrLn stderr "Notice: overflow in Editor!"

\end{code}

\subsubsection{Prepare the view}
\begin{code}

data Cmd = Backends | Scribes | Severities | SubTrace | Aggregation | Buffer | ExportConfiguration
           deriving (Enum, Eq, Show, Read)

prepare :: ToJSON a => Editor a -> Configuration -> Window -> UI ()
prepare editor config window = void $ do
    let commands = [Backends .. ]

    inputKey   <- UI.input #. "w3-input w3-border" # set UI.size "34"
    inputValue <- UI.input #. "w3-input w3-border" # set UI.size "60"
    outputMsg  <- UI.input #. "w3-input w3-border"

    currentCmd <- UI.p #. "current-cmd"

    let performActionOnId anId action =
            getElementById window anId >>= \case
                Nothing        -> return ()
                Just anElement -> action anElement

    let turn       anElement toState   = void $ element anElement # set UI.enabled toState
    let setValueOf anElement aValue    = void $ element anElement # set UI.value   aValue
    let setClasses classes   anElement = void $ element anElement # set UI.class_  classes

    let setError   m = setValueOf outputMsg ("ERROR: " ++ m)
    let setMessage m = setValueOf outputMsg m

    let enable  anElement = turn anElement True
    let disable anElement = turn anElement False
    let clean   anElement = setValueOf anElement ""
    let cleanAndDisable anElement = clean anElement >> disable anElement

    let rememberCurrent cmd = setValueOf currentCmd $ show cmd

    let removeItem Backends    k = CM.setBackends       config k Nothing
        removeItem Severities  k = CM.setSeverity       config k Nothing
        removeItem Scribes     k = CM.setScribes        config k Nothing
        removeItem SubTrace    k = CM.setSubTrace       config k Nothing
        removeItem Aggregation k = CM.setAggregatedKind config k Nothing
        removeItem _           _ = pure ()

    let updateItem Backends    k v = case (readMay v :: Maybe [BackendKind]) of
                                         Nothing -> setError "parse error on backend list"
                                         Just v' -> liftIO $ CM.setBackends config k $ Just v'
        updateItem Severities  k v = case (readMay v :: Maybe Severity) of
                                         Nothing -> setError "parse error on severity"
                                         Just v' -> liftIO $ CM.setSeverity config k $ Just v'
        updateItem Scribes     k v = case (readMay v :: Maybe [ScribeId]) of
                                         Nothing -> setError "parse error on scribe list"
                                         Just v' -> liftIO $ CM.setScribes config k $ Just v'
        updateItem SubTrace    k v = case (readMay v :: Maybe SubTrace) of
                                         Nothing -> setError "parse error on subtrace"
                                         Just v' -> liftIO $ CM.setSubTrace config k $ Just v'
        updateItem Aggregation k v = case (readMay v :: Maybe AggregatedKind) of
                                         Nothing -> setError "parse error on aggregated kind"
                                         Just v' -> liftIO $ CM.setAggregatedKind config k $ Just v'
        updateItem _           _ _ = pure ()

    disable inputKey
    disable inputValue
    disable outputMsg

    let saveItemButtonId       = "save-item-button"
    let cancelSaveItemButtonId = "cancel-save-item-button"
    let addItemButtonId        = "add-item-button"
    let outputTableId          = "output-table"

    let addItemButton          = performActionOnId addItemButtonId
    let saveItemButton         = performActionOnId saveItemButtonId
    let cancelSaveItemButton   = performActionOnId cancelSaveItemButtonId
    let cleanOutputTable       = performActionOnId outputTableId $ \t -> void $ element t # set children []

    let mkLinkToFile :: String -> FilePath -> UI Element
        mkLinkToFile str file = UI.anchor # set (attr "href") file
                                          # set (attr "target") "_blank"
                                          #+ [ string str ]
    let mkSimpleRow :: ToJSON a => LoggerName -> LogObject a -> UI Element
        mkSimpleRow n lo@(LogObject _lonm _lometa _lov) = UI.tr #. "itemrow" #+
            [ UI.td #+ [ string (unpack n) ]
            , UI.td #+ [ string $ BS8.unpack $ encode lo ]
            ]
    let mkTableRow :: Show t => Cmd -> LoggerName -> t -> UI Element
        mkTableRow cmd n v = UI.tr #. "itemrow" #+
            [ UI.td #+ [ string (unpack n) ]
            , UI.td #+ [ string (show v) ]
            , UI.td #+
                  [ do
                      b <- UI.button #. "w3-small w3-btn w3-ripple w3-orange edit-item-button"
                                     #+ [ UI.bold #+ [ string "Edit" ] ]
                      on UI.click b $ const $ do
                          saveItemButton enable
                          cancelSaveItemButton enable
                          clean outputMsg
                          enable inputKey
                          enable inputValue
                          setValueOf inputKey (unpack n)
                          setValueOf inputValue (show v)
                          rememberCurrent cmd
                      return b
                  , UI.span # set html "&nbsp;&nbsp;&nbsp;"
                  , do
                      b <- UI.button #. "w3-small w3-btn w3-ripple w3-red"
                                     #+ [ UI.bold #+ [ string "Delete" ] ]
                      on UI.click b $ const $ do
                          liftIO $ removeItem cmd n
                          cleanAndDisable inputKey
                          cleanAndDisable inputValue
                          -- Initiate a click to current menu to update the items list after deleting.
                          performActionOnId (show cmd) $ runFunction . ffi "$(%1).click()"
                      return b
                  ]
            ]

    let showCurrentTab cmd = do
            let baseClasses = "w3-bar-item w3-button"
                classesForCurrentTab = baseClasses <> " " <> "w3-light-grey"
            performActionOnId (show cmd) $ setClasses classesForCurrentTab
            let otherTabs = delete cmd commands
            forM_ otherTabs $ \tabName ->
                performActionOnId (show tabName) $ setClasses baseClasses

    let displayItems cmd sel = do
            showCurrentTab cmd
            rememberCurrent cmd
            saveItemButton disable
            cancelSaveItemButton disable
            addItemButton enable
            cleanOutputTable
            performActionOnId outputTableId $
                \t -> void $ element t #+
                    [ UI.tr #+
                        [ UI.th #+ [ string "LoggerName" ]
                        , UI.th #+ [ string $ show cmd <> " value" ]
                        , UI.th #+ [ string "" ]
                        ]
                    ]
            cg <- liftIO $ readMVar (CM.getCG config)
            forM_ (HM.toList $ sel cg) $
                \(n,v) -> performActionOnId outputTableId $
                    \t -> void $ element t #+ [ mkTableRow cmd n v ]

    let displayBuffer :: ToJSON a => Cmd -> [(LoggerName, LogObject a)] -> UI ()
        displayBuffer cmd sel = do
            showCurrentTab cmd
            rememberCurrent cmd
            saveItemButton disable
            cancelSaveItemButton disable
            addItemButton disable
            cleanOutputTable
            performActionOnId outputTableId $
                \t -> void $ element t #+
                    [ UI.tr #+
                        [ UI.th #+ [ string "LoggerName" ]
                        , UI.th #+ [ string $ show cmd <> " value" ]
                        , UI.th #+ [ string "" ]
                        ]
                    ]
            forM_ (sel) $
                \(n,v) -> performActionOnId outputTableId $
                    \t -> void $ element t #+ [ mkSimpleRow n v ]

    let accessBufferMap = do
            ed <- liftIO $ readMVar (getEd editor)
            liftIO $ readBuffer $ edBuffer ed

    let exportConfiguration = do
            currentDir <- liftIO getCurrentDirectory
            let dir = currentDir </> "iohk-monitoring/static/conf"
            liftIO $ createDirectoryIfMissing True dir
            tsnow <- formatTime defaultTimeLocale tsformat <$> liftIO getCurrentTime
            let filename = "config.yaml" ++ "-" ++ tsnow
                filepath = dir </> filename
            res <- liftIO $ catch
                (CM.exportConfiguration config filepath >>
                    return ("Configuration was exported to the file: " ++ filepath))
                (\(e :: SomeException) -> return $ show e)
            setMessage res
            performActionOnId outputTableId $
                \t -> void $ element t #+ [ mkLinkToFile
                                                "Link to configuration file"
                                                ("/static/conf" </> filename)
                                            ]

    let displayExport cmd = do
            showCurrentTab cmd
            rememberCurrent cmd
            saveItemButton disable
            cancelSaveItemButton disable
            addItemButton disable
            cleanOutputTable
            exportConfiguration

    let switchToTab c@Backends            = displayItems c $ CM.cgMapBackend
        switchToTab c@Severities          = displayItems c $ CM.cgMapSeverity
        switchToTab c@Scribes             = displayItems c $ CM.cgMapScribe
        switchToTab c@SubTrace            = displayItems c $ CM.cgMapSubtrace
        switchToTab c@Aggregation         = displayItems c $ CM.cgMapAggregatedKind
        switchToTab c@Buffer              = accessBufferMap >>= displayBuffer c
        switchToTab c@ExportConfiguration = displayExport c

    let mkEditInputs =
            row [ element inputKey
                , UI.span #. "key-value-separator" #+ [string ":"]
                , element inputValue
                , UI.span #. "key-value-separator" #+ [string ""]
                , do
                    b <- UI.button #. "w3-btn w3-ripple w3-green save-item-button"
                                   #  set (UI.attr "id") addItemButtonId
                                   #  set UI.enabled False
                                   #+ [UI.bold #+ [string "New"]]
                    on UI.click b $ const $ do
                        enable inputKey
                        enable inputValue
                        saveItemButton enable
                        cancelSaveItemButton enable
                    return b
                , UI.span #. "key-value-separator" #+ [string ""]
                , do
                    b <- UI.button #. "w3-btn w3-ripple w3-lime save-item-button"
                                   #  set (UI.attr "id") saveItemButtonId
                                   #  set UI.enabled False
                                   #+ [UI.bold #+ [string "Save"]]
                    on UI.click b $ const $ do
                        k <- inputKey   # get UI.value
                        v <- inputValue # get UI.value
                        m <- currentCmd # get UI.value
                        case (readMay m :: Maybe Cmd) of
                            Nothing -> setError "parse error on cmd"
                            Just c  -> do
                                cleanAndDisable inputKey
                                cleanAndDisable inputValue
                                saveItemButton disable
                                cancelSaveItemButton disable
                                setMessage $ "Setting '" ++ k ++ "' to '" ++ v ++ "' in " ++ m
                                updateItem c (pack k) v
                                switchToTab c
                    return b
                , UI.span #. "key-value-separator" #+ [string ""]
                , do
                    b <- UI.button #. "w3-btn w3-ripple w3-white"
                                   #  set (UI.attr "id") cancelSaveItemButtonId
                                   #  set UI.enabled False
                                   #+ [UI.bold #+ [string "Cancel"]]
                    on UI.click b $ const $ do
                        cleanAndDisable inputKey
                        cleanAndDisable inputValue
                        saveItemButton disable
                        cancelSaveItemButton disable
                    return b
                ]

    let minimumSeveritySelection = do
            confMinSev <- liftIO $ minSeverity config
            let setMinSev _el Nothing    = pure ()
                setMinSev _el (Just sev) = liftIO $
                    setMinSeverity config (toEnum sev :: Severity)

                mkSevOption sev = UI.option # set UI.text (show sev)
                                            # set UI.value (show sev)
                                            # if (confMinSev == sev) then set UI.selected True else id

            minsev <- UI.select #. "minsevfield" #+
                         map mkSevOption (enumFrom Debug)

            on UI.selectionChange minsev $ setMinSev minsev

            row [ string "Set minimum severity to:"
                , UI.span # set html "&nbsp;"
                , UI.span #. "severity-dropdown big" #+ [ element minsev ]
                ]

    let commandTabs =
            row $ flip map commands $ \cmd -> do
                   b <- UI.button #. "w3-bar-item w3-button w3-grey"
                                  #  set (UI.attr "id") (show cmd)
                                  #+ [ UI.bold #+ [ string (show cmd) ] ]
                   on UI.click b $ const $ do
                       cleanAndDisable inputKey
                       cleanAndDisable inputValue
                       clean outputMsg
                       switchToTab cmd
                   return b

    getElementById window "main-section" >>= \case
        Nothing -> pure ()
        Just mainSection -> void $ element mainSection #+
            [ UI.div #. "w3-panel" #+
                [ UI.div #. "w3-border w3-border-dark-grey" #+
                    [ UI.div #. "w3-panel" #+ [ minimumSeveritySelection ]
                    ]
                , UI.div #. "w3-panel" #+ []
                , UI.div #. "w3-border w3-border-dark-grey" #+
                    [ UI.div #. "w3-bar w3-grey" #+ [ commandTabs ]
                    , UI.div #. "w3-panel"       #+ [ mkEditInputs ]
                    , UI.div #. "w3-panel"       #+ [ element outputMsg ]
                    ]
                ]
            ]

\end{code}
