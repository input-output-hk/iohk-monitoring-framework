
\subsection{Cardano.BM.Backend.Kafka}
\label{code:Cardano.BM.Backend.Kafka}

%if style == newcode
\begin{code}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Cardano.BM.Backend.Kafka
    (
      KafkaStream
    -- * Plugin
    , plugin
    ) where

import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar,
                     readMVar, withMVar, tryTakeMVar)
import           Control.Concurrent.STM (atomically, retry)
import qualified Control.Concurrent.STM.TBQueue as TBQ
import           Control.Exception.Safe (throwM)
import           Control.Monad (void, when)
import           Data.Aeson (FromJSON, ToJSON (..), Value, encode, (.=))
import qualified Data.ByteString.Lazy.Char8 as BS8
import           Data.Maybe (catMaybes)
import           Data.Text (Text, split)
import qualified Data.Text.IO as TIO
import qualified Kafka.Producer as K
import           System.IO (stderr)

import           Cardano.BM.Configuration (Configuration, getTextOption, getTextOptionOrDefault)
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Severity
import           Cardano.BM.Plugin
import qualified Cardano.BM.Trace as Trace

\end{code}
%endif

\subsubsection{Plugin definition}
\begin{code}
plugin :: (IsEffectuator s a, ToJSON a, FromJSON a)
       => Configuration -> Trace.Trace IO a -> s a -> IO (Plugin a)
plugin config trace sb = do
    be :: Cardano.BM.Backend.Kafka.KafkaStream a <- realizefrom config trace sb
    return $ BackendPlugin
               (MkBackend { bEffectuate = effectuate be, bUnrealize = unrealize be })
               (bekind be)
\end{code}

\subsubsection{Structure of KafkaStream}\label{code:KafkaStream}\index{KafkaStream}
\begin{code}
type KafkaMVar a = MVar (KafkaInternal a)
newtype KafkaStream a = KafkaStream
    { getK :: KafkaMVar a }

data KafkaInternal a = KafkaInternal
    { kQueue    :: TBQ.TBQueue (Maybe (LogObject a))
    , kDispatch :: Async.Async ()
    }

\end{code}

\subsubsection{KafkaStream is an effectuator}\index{KafkaStream!instance of IsEffectuator}
Function |effectuate| is called to pass in a |LogObject| to forward to Kafka.
In case the queue is full, all new items are dropped.
\begin{code}
instance IsEffectuator KafkaStream a where
    effectuate kstream item = do
        kref <- readMVar (getK kstream)
        nocapacity <- atomically $ TBQ.isFullTBQueue (kQueue kref)
        if nocapacity
        then handleOverflow kstream
        else atomically $ TBQ.writeTBQueue (kQueue kref) (Just item)

    handleOverflow _ = TIO.hPutStrLn stderr "Notice: Kafkas's queue full, dropping log items!"

\end{code}

\subsubsection{|KafkaStream| implements |Backend| functions}\index{KafkaStream!instance of IsBackend}

|KafkaStream| is an |IsBackend|
\begin{code}
instance (ToJSON a, FromJSON a) => IsBackend KafkaStream a where
    bekind _ = UserDefinedBK "KafkaStream"

    realize _ = fail "KafkaStream cannot be instantiated by 'realize'"

    realizefrom config sbtrace _ = do
        kref <- newEmptyMVar
        let kstream = KafkaStream kref
        let qSize = 8192
        queue <- atomically $ TBQ.newTBQueue qSize
        dispatcher <- spawnDispatcher config queue sbtrace
        -- link the given Async to the current thread, such that if the Async
        -- raises an exception, that exception will be re-thrown in the current
        -- thread, wrapped in ExceptionInLinkedThread.
        Async.link dispatcher
        putMVar kref $ KafkaInternal
                        { kQueue = queue
                        , kDispatch = dispatcher
                        }
        return kstream

    unrealize kstream = do
        let clearMVar :: MVar b -> IO ()
            clearMVar = void . tryTakeMVar

        (dispatcher, queue) <- withMVar (getK kstream) (\kref ->
                                return (kDispatch kref, kQueue kref))
        -- send terminating item to the queue
        atomically $ TBQ.writeTBQueue queue Nothing
        -- exit the dispatcher
        res <- Async.waitCatch dispatcher
        either throwM return res
        clearMVar $ getK kstream

\end{code}

\subsubsection{Asynchronously reading log items from the queue and their processing}
\begin{code}
spawnDispatcher :: forall a. ToJSON a
                => Configuration
                -> TBQ.TBQueue (Maybe (LogObject a))
                -> Trace.Trace IO a
                -> IO (Async.Async ())
spawnDispatcher config evqueue sbtrace =
    Async.async (initState >>= qProc)
  where
    ktrace = Trace.appendName "#Kafka" sbtrace

    initState :: IO (Trace.Trace IO a, Maybe K.KafkaProducer)
    initState = do
        kproducer <- createProducer
        return (ktrace, kproducer)

    createProducer = do
        kprops <- getKprops config
        kp <- K.newProducer kprops
        return $ case kp of
                    Left _ -> Nothing
                    Right p -> Just p

    getKprops :: Configuration -> IO K.ProducerProperties
    getKprops config = do
        kservers0 <- getTextOption config "kafka_servers"
        let kservers = case kservers0 of
                        Nothing -> [K.BrokerAddress "localhost:9092"]
                        Just s -> map K.BrokerAddress $ split (==',') s
        return (K.brokersList kservers <> K.logLevel K.KafkaLogDebug)

    {-@ lazy qProc @-}
    qProc :: (Trace.Trace IO a, Maybe K.KafkaProducer) -> IO ()
    qProc (tr, Nothing) = do
        threadDelay 2500000
        initState >>= qProc
    qProc state@(tr, Just kproducer) = do
        qitems <- atomically $ do
                    list <- TBQ.flushTBQueue evqueue
                    when (null list) retry   -- blocks until new items av.
                    return list
        let l1 = length qitems
        let los = catMaybes qitems
        let l2 = length los
        when (l2 > 0) $ do
            topic <- getTextOptionOrDefault config "kafka_topic" "unknown_topic"
            res <- K.produceMessageBatch kproducer $ map (encodeMessage (K.TopicName topic)) los
            when (length res > 0) $ do
                    mle <- mkLOMeta Error Public
                    Trace.traceNamedObject tr (mle, LogError "some messages failed to deliver to topic")
                    threadDelay 500000
        if (l1 > l2)
        then K.closeProducer kproducer
        else qProc state

    encodeMessage :: ToJSON a => K.TopicName ->LogObject a -> K.ProducerRecord
    encodeMessage topic lo =
        let m = BS8.toStrict $ encode lo
        in K.ProducerRecord
                    { K.prTopic = topic
                    , K.prPartition = K.UnassignedPartition
                    , K.prKey = Nothing
                    , K.prValue = Just m
                    }

\end{code}
