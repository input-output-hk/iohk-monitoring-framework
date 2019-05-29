
\subsection{Cardano.BM.Data.MonitoringEval}

%if style == newcode
\begin{code}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Cardano.BM.Data.MonitoringEval
  ( MEvExpr (..)
  , MEvPreCond
  , Operator (..)
  , MEvAction (..)
  , VarName
  , Environment
  , parseMaybe
  , evaluate
  )
  where

import Prelude hiding (Ordering (..))

import           Control.Applicative ((<|>))
import           Control.Monad (void)
import           Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import qualified Data.Attoparsec.Text as P
import           Data.Char (isSpace)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text, pack, unpack)
import           Data.Word (Word64)

import           Cardano.BM.Data.Aggregated
import           Cardano.BM.Data.Severity

\end{code}
%endif

\subsubsection{Operators}\label{code:Operator}
Operators used to construct expressions.
\begin{code}

data Operator = GE -- >=
              | EQ -- ==
              | NE -- /=, !=, <>
              | LE -- <=
              | LT -- <
              | GT -- >
              deriving (Eq)

instance Show Operator where
    show GE = ">="
    show EQ = "=="
    show NE = "/="
    show LE = "<="
    show LT = "<"
    show GT = ">"

fromOperator :: Operator -> (Measurable -> Measurable -> Bool)
fromOperator GE = (>=)
fromOperator EQ = (==)
fromOperator NE = (/=)
fromOperator LE = (<=)
fromOperator LT = (<)
fromOperator GT = (>)

\end{code}

\subsubsection{Expressions}\label{code:MEvExpr}
Evaluation in monitoring will evaluate expressions
\begin{code}
type VarName = Text
data MEvExpr = Compare VarName (Operator, Measurable)
             | AND MEvExpr MEvExpr
             | OR  MEvExpr MEvExpr
             | NOT MEvExpr

            -- parsing: "(some >= (2000 µs))"  =>  Compare "some" (GE, (Microseconds 2000))
            -- parser "((lastreported >= (5 s)) Or ((other >= (0 s)) And (some > (1500 µs))))"

-- Precondition for monitoring is the same logical expression,
-- but it is an optional expression.
type MEvPreCond = Maybe MEvExpr

instance Eq MEvExpr where
    (==) (Compare vn1 p1) (Compare vn2 p2) = (vn1 == vn2) && (p1 == p2)
    (==) (AND e11 e12)    (AND e21 e22)    = (e11 == e21 && e12 == e22)
    (==) (OR e11 e12)     (OR e21 e22)     = (e11 == e21 && e12 == e22)
    (==) (NOT e1)         (NOT e2)         = (e1 == e2)
    (==) _                _                = False

instance FromJSON MEvExpr where
    parseJSON (String s) =
        case parseEither s of
            Left e     -> error e
            Right expr -> pure expr
    parseJSON _ = error "cannot parse such an expression!"

instance ToJSON MEvExpr where
    toJSON expr = String $ pack $ show expr

instance Show MEvExpr where
    show (Compare vn (op, x)) = "(" ++ (unpack vn) ++ " " ++ show op ++ " (" ++ show x ++")" ++ ")"
    show (AND e1 e2)          = "(" ++ (show e1) ++ ") And (" ++ (show e2) ++ ")"
    show (OR e1 e2)           = "(" ++ (show e1) ++ " Or "  ++ (show e2) ++ ")"
    show (NOT e)              = "Not (" ++ (show e) ++ ")"

\end{code}

\subsubsection{Monitoring actions}\label{code:MEvAction}
If evaluation of a monitoring expression is |True|, then a set of actions are
executed for alerting.
\begin{code}
data MEvAction = CreateMessage Severity Text
               | SetGlobalMinimalSeverity Severity
               deriving (Eq)

instance FromJSON MEvAction where
    parseJSON (String s) =
        case parseEitherAction s of
            Left e     -> error e
            Right expr -> pure expr
    parseJSON _ = error "cannot parse such an action!"

instance ToJSON MEvAction where
    toJSON = String . pack . show

instance Show MEvAction where
    show (CreateMessage sev msg)        = "(CreateMessage" ++ " " ++ show sev ++ " " ++ show msg ++ ")"
    show (SetGlobalMinimalSeverity sev) = "(SetGlobalMinimalSeverity" ++ " " ++ show sev ++ ")"
\end{code}

\subsubsection{Parsing an expression from textual representation}\label{code:parseEither}\label{code:parseMaybe}
\begin{code}
parseEither :: Text -> Either String MEvExpr
parseEither t =
    let r = P.parse parseExpr t
    in
    P.eitherResult r

parseMaybe :: Text -> Maybe MEvExpr
parseMaybe t =
    let r = P.parse parseExpr t
    in
    P.maybeResult r

openPar, closePar :: P.Parser ()
openPar = void $ P.char '('
closePar = void $ P.char ')'
token :: Text -> P.Parser ()
token s = void $ P.string s

\end{code}

\subsubsection{Parsing an action from textual representation}\label{code:parseEitherAction}
\begin{code}
parseEitherAction :: Text -> Either String MEvAction
parseEitherAction t =
    let r = P.parse parseAction t
    in
    P.eitherResult r

\end{code}

\label{code:parseExpr}
An expression is enclosed in parentheses. Either it is a negation, starting with 'Not',
or a binary operand like 'And', 'Or', or a comparison of a named variable.
\begin{code}
parseExpr :: P.Parser MEvExpr
parseExpr = do
    openPar
    P.skipSpace
    e <- do
            (nextIsChar 'N' >> parseNot)
        <|> (nextIsChar '(' >> parseBi)
        <|> parseComp
    P.skipSpace
    closePar
    return e

\end{code}

\label{code:parseAction}
An action is enclosed in parentheses. Currently only 'CreateMessage' is supported.
\begin{code}
parseAction :: P.Parser MEvAction
parseAction = do
    openPar
    P.skipSpace
    a <- do
            (nextIsChar 'C' >> parseActionCreateMessage)
        <|> (nextIsChar 'S' >> parseActionSetMinSeverity)
    P.skipSpace
    closePar
    return a

parseActionCreateMessage :: P.Parser MEvAction
parseActionCreateMessage = do
    void $ P.string "CreateMessage"
    P.skipSpace
    severity <- parsePureSeverity
    P.skipSpace
    void $ P.char '\"'
    alertMessage <- P.takeWhile1 (/='\"')
    void $ P.char '\"'
    return $ CreateMessage severity alertMessage

parseActionSetMinSeverity :: P.Parser MEvAction
parseActionSetMinSeverity = do
    void $ P.string "SetGlobalMinimalSeverity"
    P.skipSpace
    severity <- parsePureSeverity
    P.skipSpace
    return $ SetGlobalMinimalSeverity severity

parsePureSeverity :: P.Parser Severity
parsePureSeverity =
        (P.string "Debug"     >> return Debug)
    <|> (P.string "Info"      >> return Info)
    <|> (P.string "Notice"    >> return Notice)
    <|> (P.string "Warning"   >> return Warning)
    <|> (P.string "Error"     >> return Error)
    <|> (P.string "Critical"  >> return Critical)
    <|> (P.string "Alert"     >> return Alert)
    <|> (P.string "Emergency" >> return Emergency)

\end{code}

\label{code:nextIsChar}
\begin{code}
nextIsChar :: Char -> P.Parser ()
nextIsChar c = do
    c' <- P.peekChar'
    if c == c'
    then return ()
    else fail $ "cannot parse char: " ++ [c]

parseBi :: P.Parser MEvExpr
parseBi = do
    e1 <- parseExpr
    P.skipSpace
    op <-     (token "And" >> return AND)
          <|> (token "Or" >> return OR)
    P.skipSpace
    e2 <- parseExpr
    return (op e1 e2)

parseNot :: P.Parser MEvExpr
parseNot = do
    token "Not"
    P.skipSpace
    e <- parseExpr
    P.skipSpace
    return (NOT e)

parseComp :: P.Parser MEvExpr
parseComp = do
    vn <- parseVname
    P.skipSpace
    op <- parseOp
    P.skipSpace
    m <- parseMeasurable
    return $ Compare vn (op, m)

parseVname :: P.Parser VarName
parseVname = do
    P.takeTill (isSpace)

parseOp :: P.Parser Operator
parseOp = do
        (P.string ">=" >> return GE)
    <|> (P.string "==" >> return EQ)
    <|> (P.string "/=" >> return NE)
    <|> (P.string "!=" >> return NE)
    <|> (P.string "<>" >> return NE)
    <|> (P.string "<=" >> return LE)
    <|> (P.string "<"  >> return LT)
    <|> (P.string ">"  >> return GT)

parseMeasurable :: P.Parser Measurable
parseMeasurable = do
    openPar
    P.skipSpace
    m <- parseMeasurable'
    P.skipSpace
    closePar
    return m
parseMeasurable' :: P.Parser Measurable
parseMeasurable' =
        parseTime
    <|> parseBytes
    <|> parseSeverity
    <|> (P.decimal >>= return . PureI)
    <|> (P.double >>= return . PureD)

parseTime :: P.Parser Measurable
parseTime = do
    n <- P.decimal
    P.skipSpace
    tryUnit n
  where
    tryUnit :: Word64 -> P.Parser Measurable
    tryUnit n =
            (P.string "ns" >> return (Nanoseconds n))
        <|> (P.string "µs" >> return (Microseconds n))
        <|> (P.string "s"  >> return (Seconds n))

parseBytes :: P.Parser Measurable
parseBytes = do
    n <- P.decimal
    P.skipSpace
    tryUnit n
  where
    tryUnit :: Word64 -> P.Parser Measurable
    tryUnit n =
            (P.string "kB"    >> return (Bytes (n * 1000)))
        <|> (P.string "bytes" >> return (Bytes n))
        <|> (P.string "byte"  >> return (Bytes n))
        <|> (P.string "MB"    >> return (Bytes (n * 1000 * 1000)))
        <|> (P.string "GB"    >> return (Bytes (n * 1000 * 1000 * 1000)))

parseSeverity :: P.Parser Measurable
parseSeverity =
        (P.string "Debug"     >> return (Severity Debug))
    <|> (P.string "Info"      >> return (Severity Info))
    <|> (P.string "Notice"    >> return (Severity Notice))
    <|> (P.string "Warning"   >> return (Severity Warning))
    <|> (P.string "Error"     >> return (Severity Error))
    <|> (P.string "Critical"  >> return (Severity Critical))
    <|> (P.string "Alert"     >> return (Severity Alert))
    <|> (P.string "Emergency" >> return (Severity Emergency))
\end{code}

\subsubsection{Evaluate expression}\label{code:Environment}\label{code:evaluate}
This is an interpreter of |MEvExpr| in an |Environment|.
\begin{code}
type Environment = HM.HashMap VarName Measurable

\end{code}

The actual interpreter of an expression returns |True|
if the expression is valid in the |Environment|,
otherwise returns |False|.
\begin{code}
evaluate :: Environment -> MEvExpr -> Bool
evaluate ev expr =
    case expr of
        Compare vn (op, m2) ->
                     case getMeasurable ev vn of
                        Nothing -> False
                        Just m1 -> (fromOperator op) m1 m2
        AND e1 e2 -> (evaluate ev e1) && (evaluate ev e2)
        OR e1 e2  -> (evaluate ev e1) || (evaluate ev e2)
        NOT e     -> not (evaluate ev e)

\end{code}

Helper functions to extract named values from the |Environment|.
\begin{code}
getMeasurable :: Environment -> VarName -> Maybe Measurable
getMeasurable ev vn = HM.lookup vn ev

\end{code}
