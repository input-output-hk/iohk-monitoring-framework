
\subsection{Cardano.BM.Tracer}
\label{code:Cardano.BM.Tracer}

%if style == newcode
\begin{code}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.BM.Tracer
    (
      Tracer (..)
    -- * observing
    , bracketObserve
    -- * examples
    , example
    , exampleWithChoose
    ) where

import           Data.Functor.Contravariant (contramap)
import           Data.Functor.Contravariant.Divisible (Divisible (..),
                     Decidable (..))
import           Data.Void (Void)
import           Data.Word (Word64)
import           GHC.Clock (getMonotonicTimeNSec)

import           Control.Tracer (Tracer (..), nullTracer, showTracing,
                                     stdoutTracer, traceWith)
import           Control.Tracer.Observe (Observable (..), ObserveIndicator (..))

\end{code}
%endif

\subsubsection{Divisible and Decidable instances of |Tracer|}
A |Divisible| contravariant functor is the contravariant analogue of
|Applicative|. A |Divisible| contravariant functor has the ability to
be composed "beside" another contravariant. It gives a way to combine
two contravariant functors that focus on different parts of a
structure.
(see \url{https://hackage.haskell.org/package/contravariant-1.5/docs/Data-Functor-Contravariant-Divisible.html#g:1})

\begin{code}
instance Applicative m => Divisible (Tracer m) where
  divide  :: (a -> (b, c)) -> Tracer m b -> Tracer m c -> Tracer m a
  divide f (Tracer g) (Tracer h) = Tracer $ \a -> case f a of
    (b, c) -> g b *> h c

  conquer :: Tracer m a
  conquer = nullTracer

\end{code}
A |Decidable| contravariant functor is the contravariant analogue of
|Alternative|. Noting the superclass constraint that the
contravariant functor must also be |Divisible|, a |Decidable| functor
has the ability to "fan out" input, under the intuition that
contravariant functors consume input. It chooses the appropriate
contravariant functor for a data structure that is an alternative
choice (sum) of two different parts.
(see \url{https://hackage.haskell.org/package/contravariant-1.5/docs/Data-Functor-Contravariant-Divisible.html#g:2})

\begin{code}
instance Applicative m => Decidable (Tracer m) where
  lose :: (a -> Void) -> Tracer m a
  lose _ = nullTracer

  choose :: (a -> Either b c) -> Tracer m b -> Tracer m c -> Tracer m a
  choose f (Tracer g) (Tracer h) = Tracer $ either g h . f

\end{code}

\subsubsection{bracketObserve}\label{code:bracketObserve}\index{bracketObserve}
\begin{code}
bracketObserve :: forall m t b . Monad m
               => (m t, Tracer m (Observable t))
               -> m b
               -> m b
bracketObserve (getTime, tr) action = do

    let transform :: Tracer m (Observable t) -> Tracer m ObserveIndicator
        transform trace =  Tracer $ \observeIndicator -> do
            now <- getTime
            traceWith trace $ Obs observeIndicator now

        tr' = transform tr

    traceWith tr' ObserveBefore
    res <- action
    traceWith tr' ObserveAfter

    return res

\end{code}

\subsubsection{example}
\begin{code}
data AddSub a = Add a
              | Sub a
              deriving Show

type Time = Word64

example :: IO Int
example = do
    let trInt :: Tracer IO (AddSub Int)
        trInt = showTracing stdoutTracer
        trObserve :: Tracer IO (Observable Time)
        trObserve = showTracing stdoutTracer

    _ <- bracketObserve (getMonotonicTimeNSec, trObserve) (actionAdd trInt)
    bracketObserve (getMonotonicTimeNSec, trObserve) (actionSub trInt)

  where
    actionAdd :: Tracer IO (AddSub Int) -> IO Int
    actionAdd tr = do
        let res = 1+2
        traceWith tr $ Add res
        return res
    actionSub :: Tracer IO (AddSub Int) -> IO Int
    actionSub tr = do
        let res = 1-2
        traceWith tr $ Sub res
        return res

exampleWithChoose :: IO Int
exampleWithChoose = do
    let trInt :: Tracer IO (AddSub Int)
        trInt = showTracing stdoutTracer
        trObserve :: Tracer IO (Observable (AddSub Time))
        trObserve = showTracing stdoutTracer

        trace :: Tracer IO (Either (Observable (AddSub Time)) (AddSub Int))
        trace = choose id trObserve trInt

    _ <- bracketObserve (Add <$> getMonotonicTimeNSec, contramap Left trace) $ actionAdd $ contramap Right trace
    bracketObserve (Sub <$> getMonotonicTimeNSec, contramap Left trace) $ actionSub $ contramap Right trace

  where
    actionAdd :: Tracer IO (AddSub Int) -> IO Int
    actionAdd tr = do
        let res = 1+2
        traceWith tr $ Add res
        return res
    actionSub :: Tracer IO (AddSub Int) -> IO Int
    actionSub tr = do
        let res = 1-2
        traceWith tr $ Sub res
        return res

instance Show (Observable (AddSub Time)) where
  show (Obs ind a) = show ind ++ " " ++ show a

\end{code}
