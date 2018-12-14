{-# LANGUAGE OverloadedStrings #-}

module ProcHPC
  (
    main
  )
  where

import qualified Data.Text as T
import           System.Environment (getArgs)
import qualified Text.HTML.DOM as DOM
import           Text.XML.Cursor (Cursor, content, element, fromDocument,
                     child, ($//), (&|), (>=>))


defaultfp = "../../dist-newstyle/build/x86_64-osx/ghc-8.6.2/iohk-monitoring-0.1.0.0/hpc/vanilla/html/iohk-monitoring-0.1.0.0/hpc_index_fun.html"

findRows :: Cursor -> [Cursor]
findRows = element "body" >=> child >=> element "table" >=> child >=> element "tr" >=> child

extractModules :: Cursor -> [T.Text]
extractModules = -- T.append "@ " $ T.concat $
    element "td" >=> child >=> element "tt" >=> child >=> element "a" >=> child >=> content
extractScores :: Cursor -> [T.Text]
extractScores = -- T.append "@ " $ T.concat $
    element "td" >=> child >=> content

cursorFile :: String -> IO Cursor
cursorFile fp = do
     page <- DOM.readFile fp
     return $ fromDocument $ page

main :: IO ()
main = do
    args <- getArgs
    let filepath = case args of
                       (fp : _)  -> fp
                       otherwise -> defaultfp
    cursor <- cursorFile filepath
    let scores  = cursor $// findRows &| (T.concat . extractScores)
    let modules = cursor $// findRows &| (T.concat . extractModules)

    putStrLn "\\begin{tabular}{l r}"
    showTable $ reverse $
      zip ( showby12 (drop 11 modules) prepModule [] )
          ( showby12 (drop 11 scores) prepScore [] )
    putStrLn "\\end{tabular}"
  where
    showTable :: [(T.Text, T.Text)] -> IO ()
    showTable [] = return ()
    showTable (a : as) = showRow a >> showTable as
    showRow :: (T.Text, T.Text) -> IO ()
    showRow (m, s) = putStrLn $ T.unpack $ "   " <> m <> " & " <> s <> " \\\\"
    showby12 :: [T.Text] -> ([T.Text] -> T.Text) -> [T.Text] -> [T.Text]
    showby12 [] _ acc = acc
    showby12 as f acc = --do
        --print $ f $ take 12 as
        let a' = f $ take 12 as
        in
        showby12 (drop 12 as) f (a' : acc)
    prepModule (a : _) = T.replace "_" "\\_" $ T.drop 1 . snd $ T.breakOn "/" a
    prepScore (_ : _ : a : _) = T.replace "%" "\\%" a
