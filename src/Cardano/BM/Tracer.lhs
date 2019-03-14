
\subsection{Cardano.BM.Tracer}
\label{code:Cardano.BM.Tracer}

\begin{code}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}

module Cardano.BM.Tracer
    ( Tracer (..)
    , Contravariant(..)
    -- * tracers
    , nullTracer
    , stdoutTracer
    , debugTracer
    , showTracing
    -- * conditional tracing
    , condTracing
    , condTracingM
    -- * named tracing
    , namedTrace
    , appendNamedTracing
    , filterAppendNameTracing
    , filterNamedTracing
    , renderNamedTracing
    -- * testing
    , test1
    , test2
    , test3
    , test4
    ) where

import           Control.Monad (void)
import           Data.Functor.Contravariant (Op (..))
import           Data.Text (Text, pack, unpack)

import           Cardano.BM.Data.LogItem (LogNamed (..), LoggerName,
                     NamedLogItem, LogObject (..), LOContent (..),
                     PrivacyAnnotation (..),
                     PrivacyAndSeverityAnnotated (..), mkLOMeta)
import           Cardano.BM.Data.Severity (Severity (..))
import           Cardano.BM.Tracer.CallGraph
import           Cardano.BM.Tracer.Class
import           Cardano.BM.Tracer.Simple
import           Cardano.BM.Tracer.Transformers

\end{code}

\subsubsection{Testing conditional tracing in \emph{ghci}}
\begin{spec}

import Cardano.BM.Tracer

:set -Wno-type-defaults

let f = \tr -> do tracingWith tr "hello"
f (showTracing stdoutTracer)
f (condTracing False $ showTracing stdoutTracer)
f (condTracing True $ showTracing stdoutTracer)
f (condTracingM (pure True) $ showTracing stdoutTracer)

\end{spec}

\begin{code}

test1 :: IO ()
test1 = do
    let logTrace a = tracingWith (showTracing (contramap ("Debug: " ++) stdoutTracer)) a
    void $ callFun1 logTrace

callFun1 :: (String -> IO ()) -> IO Int
callFun1 logTrace = do
    logTrace "in function 1"
    return 42

\end{code}

\subsubsection{Testing named tracing in \emph{ghci}}
\begin{spec}

import Cardano.BM.Tracer

:set +m
:set -Wno-type-defaults

let f = \tr -> do tracingWith tr 42
let tr = namedTrace (appendNamedTracing "last"
                      (appendNamedTracing "mid"
                        (appendNamedTracing "first"
                          (renderNamedTracing stdoutTracer ))))

f tr

\end{spec}

\begin{code}

renderNamedItemTracing :: Show a => Tracer m String -> Tracer m (NamedLogItem a)
renderNamedItemTracing = contramap $ \item ->
    unpack (lnName item) ++ ": " ++ show (lnItem item)

named :: Tracer m (LogNamed a) -> Tracer m a
named = contramap (LogNamed mempty)

appendNamed :: LoggerName -> Tracer m (LogNamed a) -> Tracer m (LogNamed a)
appendNamed name = contramap $ (\(LogNamed oldName item) ->
    LogNamed (name <> "." <> oldName) item)

\end{code}

The function |toLogObject| can be specialized for various environments
\begin{code}
class Monad m => ToLogObject m where
  toLogObject :: Tracer m (LogObject a) -> Tracer m a

instance ToLogObject IO where
    toLogObject :: Tracer IO (LogObject a) -> Tracer IO a
    toLogObject (Tracer (Op tr)) = Tracer $ Op $ \a -> do
        lo <- LogObject <$> (mkLOMeta Debug Both)
                        <*> pure (LogMessage a)
        tr lo

-- instance ToLogObject SimF a where
--   toLogObject (Tracer tr) = Tracer $ \a -> do { ... }

tracingNamed :: Show a => Tracer IO (NamedLogItem a) -> Tracer IO a
tracingNamed = toLogObject . named

test2 :: IO ()
test2 = do
    let logTrace = appendNamed "test2" (renderNamedItemTracing stdoutTracer)

    void $ callFun2 logTrace

callFun2 :: Tracer IO (LogNamed (LogObject Text)) -> IO Int
callFun2 logTrace = do
    let logTrace' = appendNamed "fun2" logTrace
    tracingWith (tracingNamed logTrace') "in function 2"
    callFun3 logTrace'

callFun3 :: Tracer IO (LogNamed (LogObject Text)) -> IO Int
callFun3 logTrace = do
    tracingWith (tracingNamed (appendNamed "fun3" logTrace)) "in function 3"
    return 42

severityAnnotatedM :: Show a
    => Tracer IO (LogObject Text)
    -> Tracer IO (PrivacyAndSeverityAnnotated a)
severityAnnotatedM (Tracer (Op tr)) = Tracer $ Op $ \(PSA sev priv a) -> do
    lo <- LogObject <$> (mkLOMeta sev priv)
                    <*> pure (LogMessage $ pack $ show a)
    tr lo

test3 :: IO ()
test3 = do
    let logTrace =
            severityAnnotatedM $ named $ appendNamed "test3" $ renderNamedItemTracing stdoutTracer

    tracingWith logTrace $ PSA Info Private ("Hello" :: String)
    tracingWith logTrace $ PSA Warning Both "World"

\end{code}

\begin{code}
filterAppendNameTracing' :: Monad m
    => m (LogNamed a -> Bool)
    -> LoggerName
    -> Tracer m (LogNamed a)
    -> Tracer m (LogNamed a)
filterAppendNameTracing' test name = (appendNamed name) . (condTracingM test)

test4 :: IO ()
test4 = do
    let appendF = filterAppendNameTracing' oracle
        logTrace = appendF "test4" (renderNamedItemTracing stdoutTracer)

    tracingWith (tracingNamed logTrace) ("Hello" :: String)

    let logTrace' = appendF "inner" logTrace
    tracingWith (tracingNamed logTrace') "World"

    let logTrace'' = appendF "innest" logTrace'
    tracingWith (tracingNamed logTrace'') "!!"
  where
    oracle :: Monad m => m (LogNamed a -> Bool)
    oracle = return $ ((/=) "test4.inner.") . lnName

\end{code}
