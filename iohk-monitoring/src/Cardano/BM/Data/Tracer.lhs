
\subsection{Cardano.BM.Data.Tracer}
\label{code:Cardano.BM.Data.Tracer}

%if style == newcode
\begin{code}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Cardano.BM.Data.Tracer
    ( Tracer (..)
    , ToLogObject (..)
    , ToObject (..)
    , traceWith
    -- , Contravariant(..)
    -- * tracer transformers
    , natTracer
    , nullTracer
    , stdoutTracer
    , debugTracer
    , showTracing
    -- * conditional tracing
    , condTracing
    , condTracingM
    ) where

import           Data.Aeson (Object, ToJSON (..), Value (..), encode)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)

import           Cardano.BM.Data.LogItem (LogObject (..), LOContent (..),
                     PrivacyAnnotation (..), mkLOMeta)
import           Cardano.BM.Data.Severity (Severity (..))
import           Control.Tracer

\end{code}
%endif

This module extends the basic |Tracer| with one that keeps a list of context names to
create the basis for |Trace| which accepts messages from a Tracer and ends in the |Switchboard|
for further processing of the messages.

\begin{scriptsize}
\begin{verbatim}
   +-----------------------+
   |                       |
   |    external code      |
   |                       |
   +----------+------------+
              |
              |
        +-----v-----+
        |           |
        |  Tracer   |
        |           |
        +-----+-----+
              |
              |
  +-----------v------------+
  |                        |
  |        Trace           |
  |                        |
  +-----------+------------+
              |
  +-----------v------------+
  |      Switchboard       |
  +------------------------+

  +----------+ +-----------+
  |Monitoring| |Aggregation|
  +----------+ +-----------+

          +-------+
          |Logging|
          +-------+

+-------------+ +------------+
|Visualisation| |Benchmarking|
+-------------+ +------------+

\end{verbatim}
\end{scriptsize}

\subsubsection{ToLogObject - transforms a logged item to LogObject}
\label{code:ToLogObject}\index{ToLogObject}
\label{code:toLogObject}\index{ToLogObject!toLogObject}
The function |toLogObject| can be specialized for various environments
\begin{code}
class Monad m => ToLogObject m where
  toLogObject :: (ToObject a, ToObject b) => Tracer m (LogObject a) -> Tracer m b

instance ToLogObject IO where
    toLogObject :: (ToObject a, ToObject b) => Tracer IO (LogObject a) -> Tracer IO b
    toLogObject tr = Tracer $ \a -> do
        lo <- LogObject <$> pure ""
                        <*> (mkLOMeta Debug Public)
                        <*> pure (LogStructured $ encode a)
        traceWith tr lo

\end{code}

\begin{spec}
To be placed in ouroboros-network.

instance (MonadFork m, MonadTimer m) => ToLogObject m where
    toLogObject tr = Tracer $ \a -> do
        lo <- LogObject <$> pure ""
                        <*> (LOMeta <$> getMonotonicTime  -- must be evaluated at the calling site
                                    <*> (pack . show <$> myThreadId)
                                    <*> pure Debug
                                    <*> pure Public)
                        <*> pure (LogMessage a)
        traceWith tr lo

\end{spec}

\subsubsection{ToObject - transforms a logged item to JSON}\label{code:ToObject}\index{ToObject}\label{code:toObject}\index{ToObject!toObject}
Katip requires JSON objects to be logged as context. This
typeclass provides a default instance which uses |ToJSON| and
produces an empty object if 'toJSON' results in any type other than
|Object|. If you have a type you want to log that produces an Array
or Number for example, you'll want to write an explicit instance
here. You can trivially add a |ToObject| instance for something with
a ToJSON instance like:
\begin{spec}
instance ToObject Foo
\end{spec}

\begin{code}
class ToJSON a => ToObject a where
    toObject :: a -> Object
    default toObject :: a -> Object
    toObject v = case toJSON v of
        Object o -> o
        s@(String _) -> HM.singleton "string" s
        _        -> mempty

instance ToObject () where
    toObject _ = mempty

instance ToObject String
instance ToObject Text
instance ToJSON a => ToObject (LogObject a)
instance ToJSON a => ToObject (LOContent a)

\end{code}
