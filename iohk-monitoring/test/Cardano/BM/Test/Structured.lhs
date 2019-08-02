
\subsection{Cardano.BM.Test.Structured}
\label{code:Cardano.BM.Test.Structured}

%if style == newcode
\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.BM.Test.Structured (
    tests
  ) where

import           Data.Aeson (ToJSON (..), Value (..), encode, object, (.=))
import           Data.Text (Text)

import           Cardano.BM.Configuration.Static --(defaultConfigTesting)
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Tracer (Tracer (..), Transformable (..),
                     toLogObject, traceWith, trStructured)
import           Cardano.BM.Data.Severity
import qualified Cardano.BM.Setup as Setup

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (Assertion , assertBool, testCase)

\end{code}
%endif

\begin{code}
tests :: TestTree
tests = testGroup "Testing Structured Logging" [
            testCase "logging simple text" logText
    --   , testCase "logging data structures" logStructures
        ]

\end{code}

\subsubsection{Simple logging of text.}\label{code:logText}
\begin{code}

data Pet = Pet { name :: Text, age :: Int}
           deriving (Show)

instance ToJSON Pet where
    toJSON (Pet n a) = 
        object [ "kind" .= String "Pet"
               , "name" .= toJSON n
               , "age" .= toJSON a ]

instance Transformable Text IO Pet where
    trTransformer = trStructured
    -- trTransformer tr = Tracer $ \arg ->
    --     traceWith tr =<<
    --         LogObject <$> pure ""
    --                   <*> (mkLOMeta Debug Public)
    --                   <*> pure (LogStructured $ encode arg)

logText :: Assertion
logText = do
    cfg <- defaultConfigStdout
    baseTrace :: Tracer IO (LogObject Text) <- Setup.setupTrace (Right cfg) "logText"

    traceWith (toLogObject baseTrace) ("This is a simple message." :: Text)
    traceWith (toLogObject baseTrace) (".. and another!" :: String)
    traceWith (toLogObject baseTrace) (42 :: Integer)
    traceWith (toLogObject baseTrace) (Pet "bella" 8)

    assertBool "OK" True
\end{code}
