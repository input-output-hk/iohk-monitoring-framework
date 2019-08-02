
\subsection{Cardano.BM.Data.Tracer}
\label{code:Cardano.BM.Data.Tracer}

%if style == newcode
\begin{code}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Cardano.BM.Data.Tracer
    ( Tracer (..)
    , Transformable (..)
    , ToLogObject (..)
    , ToObject (..)
    , traceWith
    -- * tracer transformers
    , natTracer
    , nullTracer
    , stdoutTracer
    , debugTracer
    , showTracing
    , trStructured
    -- * conditional tracing
    , condTracing
    , condTracingM
    ) where


import           Control.Monad.IO.Class (MonadIO (..))

import           Data.Aeson (Object, ToJSON (..), Value (..), encode)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text, pack, unpack)
import           Data.Word (Word64)

import           Cardano.BM.Data.Aggregated
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
    toLogObject :: (ToObject a, Transformable a m b) => Tracer m (LogObject a) -> Tracer m b

instance ToLogObject IO where
    toLogObject :: (MonadIO m, ToObject a, Transformable a m b) => Tracer m (LogObject a) -> Tracer m b
    toLogObject tr =
        trTransformer tr

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
        Object o     -> o
        s@(String _) -> HM.singleton "string" s
        _            -> mempty

instance ToObject () where
    toObject _ = mempty

instance ToObject String
instance ToObject Text
instance ToJSON a => ToObject (LogObject a)
instance ToJSON a => ToObject (LOContent a)

\end{code}

\subsubsection{A transformable Tracer}

Parameterised over the source Tracer (\emph{b}) and
the target Tracer (\emph{a}).

\begin{code}
class Monad m => Transformable a m b where
    trTransformer :: Tracer m (LogObject a) -> Tracer m b
    default trTransformer :: Tracer m (LogObject a) -> Tracer m b
    trTransformer _ = nullTracer

trFromIntegral :: (Integral b, MonadIO m) => Tracer m (LogObject a) -> Text -> Tracer m b
trFromIntegral tr name = Tracer $ \arg ->
        traceWith tr =<<
            LogObject <$> pure ""
                      <*> (mkLOMeta Debug Public)
                      <*> pure (LogValue name $ PureI $ fromIntegral arg)

trFromReal :: (Real b, MonadIO m) => Tracer m (LogObject a) -> Text -> Tracer m b
trFromReal tr name = Tracer $ \arg ->
        traceWith tr =<<
            LogObject <$> pure ""
                      <*> (mkLOMeta Debug Public)
                      <*> pure (LogValue name $ PureD $ realToFrac arg)

instance Transformable a IO Int where
    trTransformer tr = trFromIntegral tr "int"
instance Transformable a IO Integer where
    trTransformer tr = trFromIntegral tr "integer"
instance Transformable a IO Word64 where
    trTransformer tr = trFromIntegral tr "integer"
instance Transformable a IO Double where
    trTransformer tr = trFromReal tr "dbl"
instance Transformable a IO Float where
    trTransformer tr = trFromReal tr "flt"
instance Transformable Text IO Text where
    trTransformer tr = Tracer $ \arg ->
        traceWith tr =<<
            LogObject <$> pure ""
                      <*> (mkLOMeta Debug Public)
                      <*> pure (LogMessage arg)
instance Transformable String IO String where
    trTransformer tr = Tracer $ \arg ->
        traceWith tr =<<
            LogObject <$> pure ""
                      <*> (mkLOMeta Debug Public)
                      <*> pure (LogMessage arg)
instance Transformable Text IO String where
    trTransformer tr = Tracer $ \arg ->
        traceWith tr =<<
            LogObject <$> pure ""
                      <*> (mkLOMeta Debug Public)
                      <*> pure (LogMessage $ pack arg)
instance Transformable String IO Text where
    trTransformer tr = Tracer $ \arg ->
        traceWith tr =<<
            LogObject <$> pure ""
                      <*> (mkLOMeta Debug Public)
                      <*> pure (LogMessage $ unpack arg)

-- this instance is overlapping!
-- instance ToJSON j => Transformable a IO j where
--     trTransformer tr = Tracer $ \arg ->
--         traceWith tr =<<
--             LogObject <$> pure ""
--                       <*> (mkLOMeta Debug Public)
--                       <*> pure (LogStructured $ encode arg)

trStructured :: (MonadIO m, ToJSON b) => Tracer m (LogObject a) -> Tracer m b
trStructured tr = Tracer $ \arg ->
        traceWith tr =<<
            LogObject <$> pure ""
                      <*> (mkLOMeta Debug Public)
                      <*> pure (LogStructured $ encode arg)

\end{code}
