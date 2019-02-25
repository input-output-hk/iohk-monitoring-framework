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
import           Control.Monad  (forM, void)
import qualified Data.HashMap.Strict as HM
import           Data.Text (unpack)

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core hiding (delete)

import           Cardano.BM.Configuration
import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.LogItem (LoggerName)
import           Cardano.BM.Data.Severity

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

data Cmds = Backends | Scribes | Severities | SubTrace | Aggregation
            deriving (Show)

prepare :: Configuration -> Window -> UI ()
prepare config window = void $ do
    void $ return window # set title "IOHK logging and monitoring"

    let delItem sel n = undefined
    let mkPairItem :: Show t => (CM.ConfigurationInternal -> HM.HashMap LoggerName t) -> LoggerName -> t -> UI Element
        mkPairItem sel n v =
            let entries = [ UI.td #+ [string (unpack n)]
                          , UI.td #+ [string (show v)]
                          , UI.td #+ [do
                              b <- UI.button #. "itmbutton" #+ [string "x"]
                              on UI.click b $ const $ (delItem sel n)
                              return b]
                          ]
            in UI.tr #. "itemrow" #+ entries
    let apply2output f = do
            tgt <- getElementById window "output"
            case tgt of
                Nothing -> pure ()
                Just t  -> f t
    let listPairs sel = do
            apply2output $ \t -> void $ element t # set children []
            cg <- liftIO $ readMVar (CM.getCG config)
            mapM_ (\(n,v) -> apply2output $ \t -> void $ element t #+ [mkPairItem sel n v]
                ) $ HM.toList (sel cg)

    -- commands
    let switchTo Backends    = listPairs CM.cgMapBackend
        switchTo Severities  = listPairs CM.cgMapSeverity
        switchTo Scribes     = listPairs CM.cgMapScribe
        switchTo SubTrace    = listPairs CM.cgMapSubtrace
        switchTo Aggregation = listPairs CM.cgMapAggregatedKind

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

    -- GUI layout
    let glue = string " "
    let topGrid = [grid
                    [[mkCommandButtons]
                    ,[row [string " "], glue]
                    ,[mkMinSevEntry]
                    ]
                  ]

    tgt <- getElementById window "gridtarget"
    case tgt of
        Nothing -> pure ()
        Just t  -> void $ element t #+ topGrid

\end{code}
