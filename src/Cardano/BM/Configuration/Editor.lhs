\subsection{Cardano.BM.Configuration.Editor}
\label{code:Cardano.BM.Configuration.Editor}

%if style == newcode
\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{- # LANGUAGE RecursiveDo       # -}

module Cardano.BM.Configuration.Editor
    (
      startup
    ) where

import           Prelude hiding (lookup)
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (readMVar)
import           Control.Monad  (void)
import qualified Data.HashMap.Strict as HM
import           Data.Text (unpack)

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core hiding (delete)

import           Cardano.BM.Configuration
import qualified Cardano.BM.Configuration.Model as CM
import           Cardano.BM.Data.LogItem (LoggerName)

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

data Cmds = Backends | Scribes | Severities | SubTrace
            deriving (Show)

prepare :: Configuration -> Window -> UI ()
prepare config window = do
    return window # set title "IOHK logging and monitoring"

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
    let switchTo Backends   = listPairs CM.cgMapBackend
        switchTo Severities = listPairs CM.cgMapSeverity
        switchTo Scribes    = listPairs CM.cgMapScribe
        switchTo SubTrace   = listPairs CM.cgMapSubtrace

    let mkCommandButtons =
            let btns = map (\n -> do
                            b <- UI.button #. "cmdbutton" #+ [string (show n)]
                            on UI.click b $ const $ (switchTo n)
                            return b)
                            [Backends, Scribes, Severities, SubTrace]
            in row btns

    -- GUI layout
    let glue = string " "
    let topGrid = [grid
                    [[mkCommandButtons]
                    ,[row [string " "], glue]
                    ]
                  ]
    
    tgt <- getElementById window "target"
    case tgt of
        Nothing -> pure ()
        Just t  -> void $ element t #+ topGrid

\end{code}
