
\subsection{BaseTrace}

%if False
\begin{code}
{-# LANGUAGE RankNTypes        #-}

module Cardano.BM.BaseTrace
    (
      BaseTrace (..)
    , natTrace
    , noTrace
    , traceWith
    ) where

import           Data.Functor.Contravariant (Contravariant (..), Op (..))
\end{code}
%endif

\subsubsection{Contravariant}
A covariant is a functor: F A $\to$ F B \\
A contravariant is a functor: F B $\to$ F A \\

|Op a b| implements the inverse to 'arrow' "|getOp :: b -> a|", which when applied
to a |BaseTrace| of type "|Op (m ()) s|", yields "|s -> m ()|". In our case,
|Op| accepts an action in a monad |m| with input type |LogNamed LogObject|
(see 'Trace').

\label{code:BaseTrace}
\begin{code}

newtype BaseTrace m s = BaseTrace { runTrace :: Op (m ()) s }

\end{code}

\subsubsection{contramap}
A covariant functor defines the function "|fmap :: (a -> b) -> f a -> f b|".
In case of a contravariant functor, it is the dual function
"|contramap :: (a -> b) -> f b -> f a|" which is defined.

In the following instance, |runTrace| extracts type "|Op (m ()) s|" to which
contramap applies |f|,
thus "|f s -> m ()|". The constructor |BaseTrace| restores "|Op (m ()) (f s)|".
\begin{code}

instance Contravariant (BaseTrace m) where
    contramap f = BaseTrace . contramap f . runTrace

\end{code}

\subsubsection{traceWith}\label{code:traceWith}
Accepts a |Trace| and some payload |s|. First it gets the contravariant
from the |Trace| as type "|Op (m ()) s|" and, after "|getOp :: b -> a|" which
translates to "|s -> m ()|", calls the action on the |LogNamed LogObject|.

\begin{code}

traceWith :: BaseTrace m s -> s -> m ()
traceWith = getOp . runTrace

\end{code}

\subsubsection{natTrace}\label{code:natTrace}
Natural transformation from monad |m| to monad |n|.
\begin{code}

natTrace :: (forall x . m x -> n x) -> BaseTrace m s -> BaseTrace n s
natTrace nat (BaseTrace (Op tr)) = BaseTrace $ Op $ nat . tr

\end{code}

\subsubsection{noTrace}\label{code:noTrace}
A |Trace| that discards all inputs.
\begin{code}

noTrace :: Applicative m => BaseTrace m a
noTrace = BaseTrace $ Op $ const (pure ())

\end{code}
