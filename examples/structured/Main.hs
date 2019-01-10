{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Main where

import           Control.Lens
import qualified Data.Aeson as A
import           Data.Text.Lazy (Text, pack)
import           Data.Tuple.Select
import           Formatting

import           Cardano.BM.Configuration.Static (defaultConfigStdout)
import           Cardano.BM.Setup (setupTrace)
import           Cardano.BM.Trace


main :: IO ()
main = print $ test5 ("neil", 42::Int)

test :: Text
test = format ("first=" % text % ", second=" % int) "neil" (42::Int)

test1 :: Text -> Int -> Text
test1 = format ("first=" % text % ", second=" % int)

test2 :: (Text, Int) -> Text
test2 x =
    let
        a = (x ^. _1)
        b = (x ^. _2)
    in
        format ("first=" % text % ", second=" % int) a b

test3 :: (Text, Int) -> Text
test3 x =
    let
        a = mapf (^. _1) text
        b = mapf (^. _2) int
    in
        format ("first=" % a % ", second=" % b) x x

test4 :: (Text, Int) -> Text
test4 x =
    let
        a = mapf (^. _1) text
        b = mapf (^. _2) int
    in
        format ("first=" % a <> ", second=" % b) x

test_naive :: (Text, Int) -> Text
test_naive x =
    let
        a = (x ^. _1)
        b = pack $ show (x ^. _2)
    in
        "first=" <> a <> ", second=" <> b

test5 :: (Text, Int) -> Text
test5 x =
    let
        a = mapf (sel1) text
        b = mapf (sel2) int
    in
        format ("first=" % a <> ", second=" % b) x
        
test_x1 = do
    cfg <- defaultConfigStdout
    trace0 <- setupTrace (Right cfg) "test"
    trace <- appendName "structured" trace0
    logStructured trace
            [("food",M"xq732"), ("time",D(10.45)), ("op",M"du"), ("some",I(-42))] -- named arguments
            [M"We ate ", First, M" for breakfast at ", Second] -- message creation

test_x2 = do
    cfg <- defaultConfigStdout
    trace0 <- setupTrace (Right cfg) "test"
    trace <- appendName "structured" trace0
    logStructured trace
            [("food","xq732"), ("time",10.45), ("json",J(A.String "bla")), ("some-int",42)]  -- named arguments
            ["We ate ", Fourth, " ", Third, " for breakfast at ", Second, " with some ", First] -- message creation

            