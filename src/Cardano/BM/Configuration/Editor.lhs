\subsection{Cardano.BM.Configuration.Editor}
\label{code:Cardano.BM.Configuration.Editor}

%if style == newcode
\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.Configuration.Editor
    (
      startup
    ) where

import           Prelude hiding (lookup)
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (readMVar)
import           Control.Monad  (void)
import qualified Data.HashMap.Strict as HM
import           Data.Text (pack, unpack)
import           Safe (readMay)

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core hiding (delete)

import           Cardano.BM.Configuration
import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.AggregatedKind
import           Cardano.BM.Data.BackendKind
import           Cardano.BM.Data.LogItem (LoggerName)
import           Cardano.BM.Data.Output (ScribeId)
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.SubTrace

\end{code}
%endif

\begin{code}
startup :: Configuration -> IO ()
startup config = do
    port <- getGUIport config
    if port > 0
    then do
        thd <- Async.async $
                 startGUI defaultConfig { jsPort       = Just port
                                        , jsAddr       = Just "127.0.0.1"
                                        , jsStatic     = Just "static"
                                        , jsCustomHTML = Just "configuration-editor.html"
                                        } $ prepare config
        Async.link thd
        pure ()
    else pure ()

\end{code}

\begin{code}

data Cmd = Backends | Scribes | Severities | SubTrace | Aggregation
           deriving (Show, Read)

prepare :: Configuration -> Window -> UI ()
prepare config window = void $ do
    void $ return window # set title "IOHK logging and monitoring"

    -- editing or adding map entry
    inputKey <- UI.input #. "inputkey"
    inputValue <- UI.input #. "inputvalue"
    inputMap <- UI.p #. "inputmap"
    void $ element inputKey # set UI.size "30"
    void $ element inputValue # set UI.size "60"
    outputMsg <- UI.input #. "outputmsg"
    void $ element outputMsg # set UI.size "60"

    let mkPairItem :: Show t => Cmd -> LoggerName -> t -> UI Element
        mkPairItem cmd n v =
            let entries = [ UI.td #+ [string (unpack n)]
                          , UI.td #+ [string (show v)]
                          , UI.td #+ [do
                              b <- UI.button #. "itmbutton" #+ [string "edit"]
                              on UI.click b $ const $ do
                                  void $ element inputKey # set UI.value (unpack n)
                                  void $ element inputValue # set UI.value (show v)
                                  void $ element inputMap # set UI.value (show cmd)
                              return b]
                          ]
            in UI.tr #. "itemrow" #+ entries
    let apply2output f = do
            tgt <- getElementById window "output"
            case tgt of
                Nothing -> pure ()
                Just t  -> f t
    let listPairs cmd sel = do
            apply2output $ \t -> void $ element t # set children []
            cg <- liftIO $ readMVar (CM.getCG config)
            mapM_ (\(n,v) -> apply2output $ \t -> void $ element t #+ [mkPairItem cmd n v]
                ) $ HM.toList (sel cg)

    -- commands
    let switchTo c@Backends    = listPairs c CM.cgMapBackend
        switchTo c@Severities  = listPairs c CM.cgMapSeverity
        switchTo c@Scribes     = listPairs c CM.cgMapScribe
        switchTo c@SubTrace    = listPairs c CM.cgMapSubtrace
        switchTo c@Aggregation = listPairs c CM.cgMapAggregatedKind

    let mkCommandButtons =
            let btns = map (\n -> do
                            b <- UI.button #. "cmdbutton" #+ [string (show n)]
                            on UI.click b $ const $ (switchTo n)
                            return b)
                            [Backends, Scribes, Severities, SubTrace, Aggregation]
            in row btns

    -- control global minimum severity
    confMinSev <- liftIO $ minSeverity config
    let setMinSev _el Nothing    = pure ()
        setMinSev _el (Just sev) = liftIO $ do
            setMinSeverity config (toEnum sev :: Severity)
        mkSevOption sev = UI.option # set UI.text (show sev)
                                    # set UI.value (show sev)
                                    # if (confMinSev == sev) then set UI.selected True else id
    minsev <- UI.select #. "minsevfield" #+
                 map mkSevOption (enumFrom Debug)   -- for all severities

    on UI.selectionChange minsev $ setMinSev minsev
    let mkMinSevEntry = row [string "set min. severity: ", element minsev]

    let setError m = void $ element outputMsg # set UI.value ("ERROR: " ++ m)
    let setMessage m = void $ element outputMsg # set UI.value m

    -- construct row with input fields
    let removeItem Backends    k = CM.setBackends config k Nothing
        removeItem Severities  k = CM.setSeverity config k Nothing
        removeItem Scribes     k = CM.setScribes config k Nothing
        removeItem SubTrace    k = CM.setSubTrace config k Nothing
        removeItem Aggregation k = CM.setAggregatedKind config k Nothing
    let delItem = do
            k <- inputKey # get UI.value
            m <- inputMap # get UI.value
            case (readMay m :: Maybe Cmd) of
                Nothing -> setError "parse error on cmd"
                Just c  -> do
                    setMessage $ "deleting " ++ k ++ " from " ++ m
                    liftIO $ removeItem c (pack k)
                    switchTo c
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
    let setItem = do
            k <- inputKey # get UI.value
            v <- inputValue # get UI.value
            m <- inputMap # get UI.value
            case (readMay m :: Maybe Cmd) of
                Nothing -> setError "parse error on cmd"
                Just c  -> do
                    setMessage $ "setting " ++ k ++ " => " ++ v ++ " in " ++ m
                    updateItem c (pack k) v
                    switchTo c
    let mkRowEdit = row [string "edit/add entry: ", element inputKey, string " => " , element inputValue]
        mkRowBtns = row [do { b <- UI.button #. "itmbutton" #+ [string "delete"]
                            ; on UI.click b $ const $ (delItem)
                            ; return b}
                        ,do { b <- UI.button #. "itmbutton" #+ [string "store"]
                            ; on UI.click b $ const $ (setItem)
                            ; return b}
                        ]

    -- GUI layout
    let glue = string " "
    let topGrid = [grid
                    [ [mkCommandButtons]
                    , [row [string " "], glue]
                    , [mkMinSevEntry]
                    , [row [string " "], glue]
                    , [mkRowEdit]
                    , [mkRowBtns]
                    , [element outputMsg]
                    ]
                  ]

    tgt <- getElementById window "gridtarget"
    case tgt of
        Nothing -> pure ()
        Just t  -> void $ element t #+ topGrid

\end{code}
