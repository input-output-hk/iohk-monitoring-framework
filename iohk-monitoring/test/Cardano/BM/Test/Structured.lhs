
\subsection{Cardano.BM.Test.Structured}
\label{code:Cardano.BM.Test.Structured}

%if style == newcode
\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.BM.Test.Structured (
    tests
  ) where

import qualified Control.Concurrent.STM as STM

import           Data.Aeson (ToJSON (..), Value (..), object, (.=))
import           Data.Text (Text)

import           Cardano.BM.Configuration.Static
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Tracer (Tracer (..), Transformable (..),
                     annotateConfidential, severityNotice,
                     toLogObject, traceWith, trStructured)
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.SubTrace
import           Cardano.BM.Backend.Switchboard (MockSwitchboard (..))
import qualified Cardano.BM.Setup as Setup

import           Cardano.BM.Test.Trace (TraceConfiguration (..), setupTrace)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (Assertion , assertBool, testCase)

\end{code}
%endif

\begin{code}
tests :: TestTree
tests = testGroup "Testing Structured Logging" [
              testCase "logging simple text" logSimpleText
            , testCase "logging data structures" logStructured
            ]

\end{code}

\subsubsection{Simple logging of text}\label{code:logSimpleText}
\begin{code}
logSimpleText :: Assertion
logSimpleText = do
    cfg <- defaultConfigTesting
    baseTrace :: Tracer IO (LogObject Text) <- Setup.setupTrace (Right cfg) "logSimpleText"

    traceWith (toLogObject baseTrace) ("This is a simple message." :: Text)
    traceWith (toLogObject baseTrace) (".. and another!" :: String)

    assertBool "OK" True

\end{code}

\subsubsection{Structured logging}\label{code:logStructured}
\begin{code}

data Pet = Pet { name :: Text, age :: Int}
           deriving (Show)

instance ToJSON Pet where
    toJSON (Pet n a) = 
        object [ "kind" .= String "Pet"
               , "name" .= toJSON n
               , "age" .= toJSON a ]

instance Transformable Text IO Pet where
    trTransformer = trStructured  -- transform to JSON structure

logStructured :: Assertion
logStructured = do
    cfg <- defaultConfigStdout
    -- baseTrace :: Tracer IO (LogObject Text) <- Setup.setupTrace (Right cfg) "logStructured"
    msgs <- STM.newTVarIO []
    baseTrace <- setupTrace $ TraceConfiguration cfg (MockSB msgs) "logStructured" Neutral

    let noticeTracer = severityNotice baseTrace
    let confidentialTracer = annotateConfidential baseTrace

    traceWith (toLogObject noticeTracer) (42 :: Integer)
    traceWith (toLogObject confidentialTracer) (Pet "bella" 8)

    ms <- STM.readTVarIO msgs

    assertBool
        ("assert number of messages traced == 2: " ++ (show $ length ms))
        (2 == length ms)
    assertBool
        ("verify traced integer with severity Notice: " ++ (show ms))
        (Notice == severity (loMeta (ms !! 1)))
    assertBool
        ("verify traced structure with privacy annotation Confidential: " ++ (show ms))
        (Confidential == privacy (loMeta (ms !! 0)))

\end{code}
