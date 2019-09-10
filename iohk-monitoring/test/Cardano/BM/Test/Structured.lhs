
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

import           Data.Aeson (ToJSON (..), Value (..), (.=))
import           Data.Text (Text, pack)

import           Cardano.BM.Configuration.Static
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Tracing hiding (setupTrace)
import           Cardano.BM.Data.Tracer
import           Cardano.BM.Data.SubTrace
-- import           Cardano.BM.Backend.Switchboard (MockSwitchboard (..))
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
            , testCase "logging with filtering" logFiltered
            , testCase "logging data structures (stdout)" logStructuredStdout
            ]

\end{code}

\subsubsection{Simple logging of text}\label{code:logSimpleText}
Trace textual messages. This is not structured logging and only here for reference.
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
This test shows how a user-defined structure \emph{Pet} can be traced.
The |trTransformer| by default is the |nullTracer|. Therefore, an instance
of \emph{Transformable Text IO Pet} uses the transformer |trStructured| to
create a structured log item using the |ToObject| instance. The function
|toObject| depends on the verbosity level and in case of |MinimalVerbosity|
will return an |emptyObject| and not output the structure at all. The output
in |NormalVerbosity| level will be a shortened structure with just its type.
Only in |MaximalVerbosity| level will the complete structure be output.
\begin{code}

data Pet = Pet { name :: Text, age :: Int}
           deriving (Show)

instance ToObject Pet where
    toObject MinimalVerbosity _ = emptyObject -- do not log
    toObject NormalVerbosity (Pet _ _) =
        mkObject [ "kind" .= String "Pet"]
    toObject MaximalVerbosity (Pet n a) =
        mkObject [ "kind" .= String "Pet"
                 , "name" .= toJSON n
                 , "age" .= toJSON a ]

instance Transformable Text IO Pet where
    -- transform to JSON Object
    trTransformer StructuredLogging verb tr = trStructured verb tr
    -- transform to textual representation using |show|
    trTransformer TextualRepresentation _v tr = Tracer $ \pet -> do
        meta <- mkLOMeta Info Public
        traceWith tr $ LogObject "pet" meta $ (LogMessage . pack . show) pet
    trTransformer _ _verb _tr = nullTracer

-- default privacy annotation: Public
instance DefinePrivacyAnnotation Pet
-- default severity: Debug
instance DefineSeverity Pet


logStructured :: Assertion
logStructured = do
    cfg <- defaultConfigStdout
    msgs <- STM.newTVarIO []
    baseTrace <- setupTrace $ TraceConfiguration cfg (MockSB msgs) "logStructured" Neutral

    let noticeTracer = severityNotice baseTrace
    let confidentialTracer = annotateConfidential baseTrace
    let pet = Pet "bella" 8

    traceWith (toLogObject noticeTracer) (42 :: Integer)
    traceWith (toLogObject confidentialTracer) pet
    traceWith (toLogObjectMinimal confidentialTracer) pet

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

logStructuredStdout :: Assertion
logStructuredStdout = do
    cfg <- defaultConfigStdout
    baseTrace :: Tracer IO (LogObject Text) <- Setup.setupTrace (Right cfg) "logStructured"

    let noticeTracer = severityNotice baseTrace
    let confidentialTracer = annotateConfidential baseTrace

    let pet = (Pet "bella" 8)
    traceWith (toLogObject noticeTracer) (42 :: Integer)
    traceWith (toLogObject confidentialTracer) pet
    traceWith (toLogObjectVerbose confidentialTracer) pet
    traceWith (toLogObjectMinimal confidentialTracer) pet
    traceWith (toLogObject' StructuredLogging MinimalVerbosity noticeTracer) (42 :: Integer)
    traceWith (toLogObject' StructuredLogging MinimalVerbosity confidentialTracer) pet
    traceWith (toLogObject' TextualRepresentation MaximalVerbosity noticeTracer) pet

    assertBool "OK" True

\end{code}

\subsubsection{Structured logging with filtering}\label{code:logFiltered}

\begin{code}

data Material = Material { description :: Text, weight :: Int}
           deriving (Show)

instance ToObject Material where
    toObject MinimalVerbosity _ = emptyObject -- do not log
    toObject NormalVerbosity (Material d _) =
        mkObject [ "kind" .= String "Material"
                 , "description" .= toJSON d ]
    toObject MaximalVerbosity (Material d w) =
        mkObject [ "kind" .= String "Material"
                 , "description" .= toJSON d
                 , "weight" .= toJSON w ]

instance Transformable Text IO Material where
    -- transform to JSON Object
    trTransformer StructuredLogging verb tr = trStructured verb tr
    -- transform to textual representation using |show|
    trTransformer TextualRepresentation _v tr = Tracer $ \mat -> do
        meta <- mkLOMeta Info Public
        traceWith tr $ LogObject "material" meta $ (LogMessage . pack . show) mat
    trTransformer _ _verb _tr = nullTracer

instance DefinePrivacyAnnotation Material where
    definePrivacyAnnotation _ = Confidential
instance DefineSeverity Material where
    defineSeverity (Material _d w) =
        if w < 100
        then Debug
        else Info

logFiltered :: Assertion
logFiltered = do
    cfg <- defaultConfigStdout
    msgs <- STM.newTVarIO []
    baseTrace <- setupTrace $ TraceConfiguration cfg (MockSB msgs) "logStructured" Neutral

    let stone = Material "stone" 1400
        water = Material "H2O" 1000
        dust  = Material "dust" 13
        confidentialTracer = annotatePrivacyAnnotation
                             $ filterPrivacyAnnotation (pure . const Confidential)
                             $ toLogObject $ baseTrace
        infoTracer = annotateSeverity
                     $ filterSeverity (pure . const Info)
                     $ toLogObject $ baseTrace
    traceWith confidentialTracer stone
    traceWith infoTracer water
    traceWith infoTracer dust   -- does not pass severity filter

    ms <- STM.readTVarIO msgs

    assertBool
        ("assert number of messages traced == 2: " ++ (show $ length ms))
        (2 == length ms)

\end{code}
