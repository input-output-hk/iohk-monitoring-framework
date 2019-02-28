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
    inputKey <- UI.input #. "w3-input w3-border w3-round-large"
    inputValue <- UI.input #. "w3-input w3-border w3-round-large"
    inputMap <- UI.p #. "inputmap"
    void $ element inputKey # set UI.size "30"
    void $ element inputValue # set UI.size "60"
    outputMsg <- UI.input #. "w3-input w3-border w3-round-large"
    void $ element outputMsg # set UI.size "60"
                             # set UI.enabled False

    let mkPairItem :: Show t => Cmd -> LoggerName -> t -> UI Element
        mkPairItem cmd n v =
            let entries = [ UI.td #+ [string (unpack n)]
                          , UI.td #+ [string (show v)]
                          , UI.td #+ [do
                              b <- UI.button #. "w3-small w3-btn w3-ripple w3-teal" #+ [UI.bold #+ [string "edit"]]
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
                            b <- UI.button #. "w3-small w3-btn w3-ripple w3-grey" #+ [UI.bold #+ [string (show n)]]
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
    let mkMinSevEntry = row [string "set minimum severity to:", UI.span # set html "&nbsp;&nbsp;", element minsev]

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
    let mkRowEdit = row [element inputKey, UI.span #. "w3-tag w3-round w3-blue midalign" # set UI.text " => " , element inputValue]
        mkRowBtns = row [do { b <- UI.button #. "w3-small w3-btn w3-ripple w3-teal" #+ [string "delete"]
                            ; on UI.click b $ const $ (delItem)
                            ; return b}
                        ,do { b <- UI.button #. "w3-small w3-btn w3-ripple w3-teal" #+ [string "store"]
                            ; on UI.click b $ const $ (setItem)
                            ; return b}
                        ]

    -- layout
    let topGrid = UI.div #. "w3-panel" #+ [
                      UI.div #. "w3-panel w3-border w3-border-blue" #+ [
                          UI.div #. "w3-panel" #+ [mkMinSevEntry]
                        ]
                    , UI.div #. "w3-panel w3-border w3-border-blue" #+ [
                          UI.div #. "w3-panel" #+ [UI.p # set UI.text "set or update a behaviour for a named logging context:"]
                        , UI.div #. "w3-panel" #+ [mkCommandButtons]
                        , UI.div #. "w3-panel" #+ [mkRowEdit]
                        , UI.div #. "w3-panel" #+ [mkRowBtns]
                        , UI.div #. "w3-panel" #+ [element outputMsg]
                        ]
                    ]

    tgt <- getElementById window "gridtarget"
    case tgt of
        Nothing -> pure ()
        Just t  -> void $ element t #+ [topGrid]

\end{code}
