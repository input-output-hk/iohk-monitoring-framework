
\subsection{Cardano.BM.Data.Tracer}
\label{code:Cardano.BM.Data.Tracer}

%if style == newcode
\begin{code}
{-# LANGUAGE InstanceSigs #-}

module Cardano.BM.Data.Tracer
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
    -- * examples
     , example2
     , example3
     , example4
     , example5
    ) where

import           Control.Monad (void)
import           Data.Functor.Contravariant (Op (..))
import           Data.Text (Text, pack, unpack)

import           Cardano.BM.Data.LogItem (LogNamed (..), LoggerName,
                     NamedLogItem, LogObject (..), LOContent (..),
                     PrivacyAnnotation (..),
                     PrivacyAndSeverityAnnotated (..), mkLOMeta)
import           Cardano.BM.Data.Severity (Severity (..))
import           Cardano.BM.Tracer.Class
import           Cardano.BM.Tracer.Output
import           Cardano.BM.Tracer.Transformers

\end{code}
%endif

This module extends the basic |Tracer| with one that keeps a list of

\begin{code}

renderNamedItemTracing :: Show a => Tracer m String -> Tracer m (LogNamed a)
renderNamedItemTracing = contramap $ \item ->
    unpack (lnName item) ++ ": " ++ show (lnItem item)

renderNamedItemTracing' :: Show a => Tracer m String -> Tracer m (NamedLogItem a)
renderNamedItemTracing' = contramap $ \item ->
    unpack (loName item) ++ ": " ++ show (loContent item) ++ ", (meta): " ++ show (loMeta item)

\end{code}

\begin{code}
named :: Tracer m (LogNamed a) -> Tracer m a
named = contramap (LogNamed mempty)
\end{code}

Add a new name to the logging context
\begin{code}
appendNamed :: LoggerName -> Tracer m (LogNamed a) -> Tracer m (LogNamed a)
appendNamed name = contramap $ (\(LogNamed oldName item) ->
    LogNamed (name <> "." <> oldName) item)

appendNamed' :: LoggerName -> Tracer m (LogObject a) -> Tracer m (LogObject a)
appendNamed' name = contramap $ (\(LogObject oldName meta item) ->
    LogObject (name <> "." <> oldName) meta item)

\end{code}

The function |toLogObject| can be specialized for various environments
\begin{code}
class Monad m => ToLogObject m where
  toLogObject :: Tracer m (LogObject a) -> Tracer m a

instance ToLogObject IO where
    toLogObject :: Tracer IO (LogObject a) -> Tracer IO a
    toLogObject tr = Tracer $ Op $ \a -> do
        lo <- LogObject <$> pure ""
                        <*> (mkLOMeta Debug Public)
                        <*> pure (LogMessage a)
        tracingWith tr lo

\end{code}

\begin{spec}
To be placed in ouroboros-network.

instance (MonadFork m, MonadTimer m) => ToLogObject m where
    toLogObject (Tracer (Op tr) = Tracer $ Op $ \a -> do
        lo <- LogObject <$> pure ""
                        <*> (LOMeta <$> getMonotonicTime  -- must be evaluated at the calling site
                                    <*> (pack . show <$> myThreadId)
                                    <*> pure Debug
                                    <*> pure Public)
                        <*> pure (LogMessage a)
        tr lo

\end{spec}

\begin{code}
tracingNamed :: Show a => Tracer IO (NamedLogItem a) -> Tracer IO a
tracingNamed = toLogObject

example2 :: IO ()
example2 = do
    let logTrace = appendNamed' "example2" (renderNamedItemTracing' stdoutTracer)

    void $ callFun2 logTrace

callFun2 :: Tracer IO (LogObject Text) -> IO Int
callFun2 logTrace = do
    let logTrace' = appendNamed' "fun2" logTrace
    tracingWith (tracingNamed logTrace') "in function 2"
    callFun3 logTrace'

callFun3 :: Tracer IO (LogObject Text) -> IO Int
callFun3 logTrace = do
    tracingWith (tracingNamed (appendNamed' "fun3" logTrace)) "in function 3"
    return 42

\end{code}

A |Tracer| transformer creating a |LogObject| from |PrivacyAndSeverityAnnotated|.
\begin{code}
logObjectFromAnnotated :: Show a
    => Tracer IO (LogObject Text)
    -> Tracer IO (PrivacyAndSeverityAnnotated a)
logObjectFromAnnotated (Tracer (Op tr)) = Tracer $ Op $ \(PSA sev priv a) -> do
    lometa <- mkLOMeta sev priv
    tr $ LogObject "" lometa (LogMessage $ pack $ show a)

\end{code}

\begin{code}
example3 :: IO ()
example3 = do
    let logTrace =
            logObjectFromAnnotated $ appendNamed' "example3" $ renderNamedItemTracing' stdoutTracer

    tracingWith logTrace $ PSA Info Confidential ("Hello" :: String)
    tracingWith logTrace $ PSA Warning Public "World"

\end{code}

\begin{code}
filterAppendNameTracing' :: Monad m
    => m (LogNamed a -> Bool)
    -> LoggerName
    -> Tracer m (LogNamed a)
    -> Tracer m (LogNamed a)
filterAppendNameTracing' test name = (appendNamed name) . (condTracingM test)

example4 :: IO ()
example4 = do
    let appendF = filterAppendNameTracing' oracle
        logTrace = appendF "example4" (renderNamedItemTracing stdoutTracer)

    tracingWith (named logTrace) ("Hello" :: String)

    let logTrace' = appendF "inner" logTrace
    tracingWith (named logTrace') "World"

    let logTrace'' = appendF "innest" logTrace'
    tracingWith (named logTrace'') "!!"
  where
    oracle :: Monad m => m (LogNamed a -> Bool)
    oracle = return $ ((/=) "example4.inner.") . lnName

\end{code}

\begin{code}

-- severity anotated
example5 :: IO ()
example5 = do
    let logTrace =
            condTracingM oracle $
                logObjectFromAnnotated $
                    appendNamed' "test5" $ renderNamedItemTracing' stdoutTracer

    tracingWith logTrace $ PSA Debug Confidential ("Hello"::String)
    tracingWith logTrace $ PSA Warning Public "World"

  where
    oracle :: Monad m => m (PrivacyAndSeverityAnnotated a -> Bool)
    oracle = return $ \(PSA sev _priv _) -> (sev > Debug)

\end{code}
