
\subsection{Cardano.BM.Data.MonitoringEval}

%if style == newcode
\begin{code}

module Cardano.BM.Data.MonitoringEval
  ( MEvExpr (..)
  , MEvOp (..)
  , VarName
  )
  where

import qualified Data.HashMap.Strict as HM

import           Cardano.BM.Data.Aggregated
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Severity
import           Cardano.BM.Configuration.Model (Configuration)

\end{code}
%endif

\subsubsection{Expressions}
Evaluation in monitoring will evaluate expressions
\begin{code}
type VarName = String
data MEvExpr = CompM VarName MEvOp Measurable
             | CompS VarName MEvOp Severity
             | AND MEvExpr MEvExpr
             | OR MEvExpr MEvExpr
             | NOT MEvExpr
             deriving (Show, Read)

data MEvOp = MGT | MGE | MLT | MLE | MEQ | MNE
           deriving (Show, Read)

\end{code}

\subsubsection{Evaluate expression}\label{code:Environment}\label{code:EnvValue}\label{evaluate}
This is an interpreter of |MEvExpr|.
\begin{code}
type Environment = HM.HashMap VarName EnvValue
data EnvValue = MVal Measurable | SVal Severity

\end{code}

The actual interpreter of an expression returns |True|
if the expression is a valid model in the |Environment|,
otherwise returns |False|.
\begin{code}
evaluate :: Environment -> MEvExpr -> Bool
evaluate ev expr = case expr of
    CompM vn op m -> evalOp op (getMeasurable ev vn) m
    CompS vn op s -> evalOp op (getSeverity ev vn) s
    AND e1 e2     -> evaluate ev e1 && evaluate ev e2
    OR e1 e2      -> evaluate ev e1 && evaluate ev e2
    NOT e         -> not $ evaluate ev e

\end{code}

A parametrised function to compare two values each of class |Ord|.
\begin{code}
evalOp :: (Ord a) => MEvOp -> a -> a -> Bool
evalOp MGT m1 m2 = m1 > m2
evalOp MGE m1 m2 = m1 >= m2
evalOp MEQ m1 m2 = m1 == m2
evalOp MNE m1 m2 = m1 /= m2
evalOp MLT m1 m2 = m1 < m2
evalOp MLE m1 m2 = m1 <= m2

\end{code}

Helper functions to extract named values from the |Environment|.
\begin{code}
getSeverity :: Environment -> VarName -> Severity
getSeverity ev vn = do
    case HM.lookup vn ev of
        Nothing -> Debug  -- not expected
        Just (SVal s') -> s'

getMeasurable :: Environment -> VarName -> Measurable
getMeasurable ev vn = do
    case HM.lookup vn ev of
        Nothing -> PureI (-394194783483399491091)  -- not expected
        Just (MVal m') -> m'

\end{code}



\begin{code}
test1 :: MEvExpr
test1 = CompM "some" MGT (Microseconds 2000)

test2 :: MEvExpr
test2 = CompS "other" MEQ Error

test3 :: MEvExpr
test3 = OR test1 (NOT test2)
\end{code}