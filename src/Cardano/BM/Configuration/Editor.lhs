\subsection{Cardano.BM.Configuration.Editor}
\label{code:Cardano.BM.Configuration.Editor}

%if style == newcode
\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}

module Cardano.BM.Configuration.Editor
    (
      startup
    ) where

import           Prelude hiding (lookup)
import qualified Control.Concurrent.Async as Async
import           Control.Monad  (forever, void)
import           Data.List      (isPrefixOf)
-- import           Data.Maybe
-- import           Data.Monoid
import qualified Data.Map as Map
-- import qualified Data.Set as Set

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core hiding (delete)

import           Cardano.BM.Configuration
import qualified Cardano.BM.Configuration.Model as CM

\end{code}
%endif

\begin{code}
startup :: Configuration -> IO ()
startup config = do
    port <- getGUIport config
    if port > 0
    then
        void $ Async.async $ forever $
            startGUI defaultConfig { jsPort       = Just port
                                , jsAddr       = Just "127.0.0.1"
                                , jsStatic     = Just "wwwroot"
                                } prepare
    else pure ()
    
\end{code}

\begin{code}
prepare :: Window -> UI ()
prepare window = void $ mdo
    return window # set title "CRUD Example (Simple)"

    -- GUI elements
    createBtn   <- UI.button #+ [string "Create"]
    deleteBtn   <- UI.button #+ [string "Delete"]
    listBox     <- UI.listBox  bListBoxItems bSelection bDisplayDataItem
    filterEntry <- UI.entry    bFilterString
    ((firstname, lastname), tDataItem)
                <- dataItem    bSelectionDataItem

    -- GUI layout
    element listBox # set (attr "size") "10" # set style [("width","200px")]
    
    let uiDataItem = grid [[string "First Name:", element firstname]
                          ,[string "Last Name:" , element lastname]]
    let glue = string " "
    getBody window #+ [grid
        [[row [string "Filter prefix:", element filterEntry], glue]
        ,[element listBox, uiDataItem]
        ,[row [element createBtn, element deleteBtn], glue]
        ]]

    -- events and behaviors
    bFilterString <- stepper "" . rumors $ UI.userText filterEntry
    let tFilter = isPrefixOf <$> UI.userText filterEntry
        bFilter = facts  tFilter
        eFilter = rumors tFilter

    let eSelection  = rumors $ UI.userSelection listBox
        eDataItemIn = rumors $ tDataItem
        eCreate     = UI.click createBtn
        eDelete     = UI.click deleteBtn

    -- database
    -- bDatabase :: Behavior (Database DataItem)
    let update' mkey x = flip update x <$> mkey
    bDatabase <- accumB emptydb $ concatenate <$> unions
        [ create ("Emil","Example") <$ eCreate
        , filterJust $ update' <$> bSelection <@> eDataItemIn
        , delete <$> filterJust (bSelection <@ eDelete)
        ]

    -- selection
    -- bSelection :: Behavior (Maybe DatabaseKey)
    bSelection <- stepper Nothing $ head <$> unions
        [ eSelection
        , Nothing <$ eDelete
        , Just . nextKey <$> bDatabase <@ eCreate
        , (\b s p -> b >>= \a -> if p (s a) then Just a else Nothing)
            <$> bSelection <*> bShowDataItem <@> eFilter
        ]
                
    let bLookup :: Behavior (DatabaseKey -> Maybe DataItem)
        bLookup = flip lookup <$> bDatabase
                
        bShowDataItem :: Behavior (DatabaseKey -> String)
        bShowDataItem = (maybe "" showDataItem .) <$> bLookup

        bDisplayDataItem = (UI.string .) <$> bShowDataItem
                
        bListBoxItems :: Behavior [DatabaseKey]
        bListBoxItems = (\p show -> filter (p. show) . keys)
                    <$> bFilter <*> bShowDataItem <*> bDatabase

        bSelectionDataItem :: Behavior (Maybe DataItem)
        bSelectionDataItem = (=<<) <$> bLookup <*> bSelection

    -- automatically enable / disable editing
    let
        bDisplayItem :: Behavior Bool
        bDisplayItem = maybe False (const True) <$> bSelection
    
    element deleteBtn # sink UI.enabled bDisplayItem
    element firstname # sink UI.enabled bDisplayItem
    element lastname  # sink UI.enabled bDisplayItem

\end{code}

Database Model

\begin{code}
type DatabaseKey = Int
data Database a  = Database { nextKey :: !Int, db :: Map.Map DatabaseKey a }

emptydb = Database 0 Map.empty
keys    = Map.keys . db

create x     (Database newkey db) = Database (newkey+1) $ Map.insert newkey x db
update key x (Database newkey db) = Database newkey     $ Map.insert key    x db
delete key   (Database newkey db) = Database newkey     $ Map.delete key db
lookup key   (Database _      db) = Map.lookup key db

\end{code}

Data items that are stored in the data base

\begin{code}
type DataItem = (String, String)
showDataItem (firstname, lastname) = lastname ++ ", " ++ firstname

-- | Data item widget, consisting of two text entries
dataItem
    :: Behavior (Maybe DataItem)
    -> UI ((Element, Element), Tidings DataItem)
dataItem bItem = do
    entry1 <- UI.entry $ fst . maybe ("","") id <$> bItem
    entry2 <- UI.entry $ snd . maybe ("","") id <$> bItem
    
    return ( (getElement entry1, getElement entry2)
           , (,) <$> UI.userText entry1 <*> UI.userText entry2
           )

\end{code}
