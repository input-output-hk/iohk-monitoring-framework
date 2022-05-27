{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.BM.Arbitrary
  ()
where

import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import           Cardano.BM.Data.Aggregated
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Severity

import           Test.QuickCheck (arbitrary, elements)
import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Arbitrary (Arbitrary)

instance Arbitrary Severity where
  arbitrary = elements $ enumFromTo minBound maxBound

instance Arbitrary PrivacyAnnotation where
  arbitrary = elements [Confidential, Public]

instance Arbitrary LOMeta where
  arbitrary = pure (LOMeta (posixSecondsToUTCTime $ fromIntegral (1 :: Int)))
     -- Not a very good choice for an arbitary timestamp.
    <*> elements ["thread", "of", "conscience"]
    <*> pure "localhost"
    <*> arbitrary
    <*> arbitrary

instance Arbitrary Measurable where
  arbitrary = QC.oneof
    [ Microseconds <$> arbitrary
    , Nanoseconds  <$> arbitrary
    , Seconds      <$> arbitrary
    , Bytes        <$> arbitrary
    , PureD        <$> arbitrary
    , PureI        <$> arbitrary
    , Severity     <$> arbitrary
    ]

instance Arbitrary a => Arbitrary (LOContent a) where
  arbitrary = QC.oneof
    [ LogMessage <$> arbitrary
    , LogError <$> message
    , LogValue <$> message <*> arbitrary
    -- TODO:  fill this in later.
    -- , LogStructured <$> arbitrary
    -- , ObserveOpen CounterState
    -- , ObserveDiff CounterState
    -- , ObserveClose CounterState
    -- , AggregatedMessage [(Text, Aggregated)]
    -- , MonitoringEffect MonitorAction
    -- , Command CommandValue
    , pure KillPill
    ] where
    message = elements
      [ "There are three flowers in the vase. The third flower is yellow."
      , "The sky above the port was the color of television, tuned to a dead channel."
      , "But what first motivated me wasn't anything I read. I just got mad seeing the machines ripping up the woods."
      ]

instance Arbitrary a => Arbitrary (LogObject a) where
  arbitrary = do
    loName <- elements
      [ "logger.for.nothing"
      , "tracer.of"
      , "things"
      ]
    loMeta <- arbitrary
    loContent :: LOContent a <- arbitrary
    pure LogObject { loName, loMeta, loContent }
