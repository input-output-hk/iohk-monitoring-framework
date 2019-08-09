
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
    -- * severity transformers
    , severityDebug
    , severityInfo
    , severityNotice
    , severityWarning
    , severityError
    , severityCritical
    , severityAlert
    , severityEmergency
    -- * privacy annotation transformers
    , annotateConfidential
    , annotatePublic
    -- * annotate context name
    , addName
    , setName
    ) where


import           Control.Monad.IO.Class (MonadIO (..))

import           Data.Aeson (Object, ToJSON (..), Value (..), encode)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Word (Word64)

import           Cardano.BM.Data.Aggregated
import           Cardano.BM.Data.LogItem (LoggerName, LogObject (..),
                     LOContent (..), LOMeta (..), PrivacyAnnotation (..),
                     mkLOMeta)
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
\label{code:Transformable}\index{Transformable}
\label{code:trTransformer}\index{Transformable!trTransformer}
Parameterised over the source |Tracer| (\emph{b}) and
the target |Tracer| (\emph{a}).

\begin{code}
class Monad m => Transformable a m b where
    trTransformer :: Tracer m (LogObject a) -> Tracer m b
    default trTransformer :: Tracer m (LogObject a) -> Tracer m b
    trTransformer _ = nullTracer

trFromIntegral :: (Integral b, MonadIO m) => Text -> Tracer m (LogObject a) -> Tracer m b
trFromIntegral name tr = Tracer $ \arg ->
        traceWith tr =<<
            LogObject <$> pure ""
                      <*> (mkLOMeta Debug Public)
                      <*> pure (LogValue name $ PureI $ fromIntegral arg)

trFromReal :: (Real b, MonadIO m) => Text -> Tracer m (LogObject a) -> Tracer m b
trFromReal name tr = Tracer $ \arg ->
        traceWith tr =<<
            LogObject <$> pure ""
                      <*> (mkLOMeta Debug Public)
                      <*> pure (LogValue name $ PureD $ realToFrac arg)

instance Transformable a IO Int where
    trTransformer = trFromIntegral "int"
instance Transformable a IO Integer where
    trTransformer = trFromIntegral "integer"
instance Transformable a IO Word64 where
    trTransformer = trFromIntegral "word64"
instance Transformable a IO Double where
    trTransformer = trFromReal "double"
instance Transformable a IO Float where
    trTransformer = trFromReal "float"
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
                      <*> pure (LogMessage $ T.pack arg)
instance Transformable String IO Text where
    trTransformer tr = Tracer $ \arg ->
        traceWith tr =<<
            LogObject <$> pure ""
                      <*> (mkLOMeta Debug Public)
                      <*> pure (LogMessage $ T.unpack arg)

trStructured :: (MonadIO m, ToJSON b) => Tracer m (LogObject a) -> Tracer m b
trStructured tr = Tracer $ \arg ->
        traceWith tr =<<
            LogObject <$> pure ""
                      <*> (mkLOMeta Debug Public)
                      <*> pure (LogStructured $ encode arg)

\end{code}

\subsubsection{Transformers for setting severity level}
\label{code:setSeverity}
\label{code:severityDebug}
\label{code:severityInfo}
\label{code:severityNotice}
\label{code:severityWarning}
\label{code:severityError}
\label{code:severityCritical}
\label{code:severityAlert}
\label{code:severityEmergency}
\index{setSeverity}\index{severityDebug}\index{severityInfo}
\index{severityNotice}\index{severityWarning}\index{severityError}
\index{severityCritical}\index{severityAlert}\index{severityEmergency}
The log |Severity| level of a LogObject can be altered.
\begin{code}
setSeverity :: Severity -> Tracer m (LogObject a) -> Tracer m (LogObject a)
setSeverity sev tr = Tracer $ \lo@(LogObject _nm meta@(LOMeta _ts _tid _sev _pr) _lc) ->
                                traceWith tr $ lo { loMeta = meta { severity = sev } }

severityDebug, severityInfo, severityNotice,
  severityWarning, severityError, severityCritical,
  severityAlert, severityEmergency  :: Tracer m (LogObject a) -> Tracer m (LogObject a)
severityDebug     = setSeverity Debug
severityInfo      = setSeverity Info
severityNotice    = setSeverity Notice
severityWarning   = setSeverity Warning
severityError     = setSeverity Error
severityCritical  = setSeverity Critical
severityAlert     = setSeverity Alert
severityEmergency = setSeverity Emergency

\end{code}

\subsubsection{Transformers for setting privacy annotation}
\label{code:setPrivacy}
\label{code:annotateConfidential}
\label{code:annotatePublic}
\index{setPrivacy}\index{annotateConfidential}\index{annotatePublic}
The privacy annotation (|PrivacyAnnotation|) of the LogObject can
be altered with the following functions.
\begin{code}
setPrivacy :: PrivacyAnnotation -> Tracer m (LogObject a) -> Tracer m (LogObject a)
setPrivacy prannot tr = Tracer $ \lo@(LogObject _nm meta@(LOMeta _ts _tid _sev _pr) _lc) ->
                                traceWith tr $ lo { loMeta = meta { privacy = prannot } }

annotateConfidential, annotatePublic :: Tracer m (LogObject a) -> Tracer m (LogObject a)
annotateConfidential = setPrivacy Confidential
annotatePublic = setPrivacy Public

\end{code}

\subsubsection{Transformers for adding a name to the context}
\label{code:setName}
\label{code:addName}\index{setName}\index{addName}
This functions set or add names to the local context naming of |LogObject|.
\begin{code}
setName :: LoggerName -> Tracer m (LogObject a) -> Tracer m (LogObject a)
setName nm tr = Tracer $ \lo@(LogObject _nm _meta _lc) ->
                                traceWith tr $ lo { loName = nm }

addName :: LoggerName -> Tracer m (LogObject a) -> Tracer m (LogObject a)
addName nm tr = Tracer $ \lo@(LogObject nm0 _meta _lc) ->
                                if T.null nm0
                                then
                                    traceWith tr $ lo { loName = nm }
                                else
                                    traceWith tr $ lo { loName = nm0 <> "." <> nm }
 
\end{code}