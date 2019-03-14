
\subsection{Cardano.BM.Tracer.CallGraph}
\label{code:Cardano.BM.Tracer.CallGraph}

%if style == newcode
\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Cardano.BM.Tracer.CallGraph
    ( NamedItem
    , namedTrace
    , appendNamedTracing
    , filterAppendNameTracing
    , filterNamedTracing
    , renderNamedTracing
    ) where

import           Cardano.BM.Data.LogItem (LoggerName)
import           Cardano.BM.Tracer.Class
import           Data.Functor.Contravariant (Op (..))

\end{code}
%endif

Add a new name context to the current call graph context

\begin{code}
type NamedItem a = Maybe ([LoggerName], a)

namedTrace :: Tracer m (NamedItem a) -> Tracer m a
namedTrace = contramap (\v -> Just ([], v))

appendNamedTracing :: LoggerName -> Tracer m (NamedItem a) -> Tracer m (NamedItem a)
appendNamedTracing name = contramap $ fmap (\(a,b) -> (name : a, b))

filterNamedTracing' :: ([LoggerName] -> Bool) -> Tracer m (NamedItem a) -> Tracer m (NamedItem a)
filterNamedTracing' test = contramap $ \case
  Nothing -> Nothing
  Just v  -> if test (fst v) then Just v else Nothing

filterAppendNameTracing :: ([LoggerName] -> Bool) -> LoggerName -> Tracer m (NamedItem a) -> Tracer m (NamedItem a)
filterAppendNameTracing test name = (appendNamedTracing name) . (filterNamedTracing' test)

filterNamedTracing :: (Monad m) => m ([LoggerName] -> Bool) -> Tracer m (NamedItem a) -> Tracer m (NamedItem a)
filterNamedTracing tester (Tracer (Op tr)) = Tracer $ Op $ \case
  Nothing
    -> pure ()
  Just (a,b) -- 'a' is the rest of the names from this point to the leaf
    -> do
    test <- tester
    if test a then tr (Just (a,b)) else tr Nothing

renderNamedTracing :: (Show a) => Tracer m String -> Tracer m (NamedItem a)
renderNamedTracing = contramap $ \case
  Just (a,b) -> show a ++ ": " ++ show b
  Nothing -> ""

\end{code}
