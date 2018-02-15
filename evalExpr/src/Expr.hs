module Expr where

import           Data.Char   (isDigit, ord)
import           Data.String

type SyntaxError = String

data Expr = Val Double
          | Plus Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr
          | Minus Expr
          | Abs Expr
          | Var String
  deriving (Eq, Show)

instance IsString Expr where
  fromString = Var

instance Num Expr where
  (+) = Plus

  fromInteger i = Val (fromInteger i)

  (-)    = Sub
  (*)    = Mul
  negate = Minus
  abs    = Abs
  signum = undefined

-- 1/ I/O
repl :: IO ()
repl = getLine >>= interpret
  where
    interpret "exit" = return ()
    interpret s      = putStrLn (arithmetic s) >> repl

arithmetic :: String -> String
arithmetic s =
  case exprParser s of
    Left er  -> er
    Right ex -> show $ evalExpr ex

-- 3/ Evaluateur
evalExpr :: Expr -> Double
evalExpr (Plus v1 v2) = evalExpr v1 + evalExpr v2
evalExpr (Mul  v1 v2) = evalExpr v1 * evalExpr v2
evalExpr (Sub  v1 v2) = evalExpr v1 - evalExpr v2
evalExpr (Div  v1 v2) = evalExpr v1 / evalExpr v2
evalExpr (Pow  v1 v2) = evalExpr v1 `pow` evalExpr v2
evalExpr (Val  v1)     = v1

-- 2/ Parseur
exprParser :: String -> Either SyntaxError Expr
exprParser input =
  case arithParser input of
    Left err -> Left err
    Right v  -> Right $ fst v

arithParser :: String -> Either SyntaxError (Expr, String)
arithParser cs =
  do (v1, cs')    <- intParser  cs
     case cs' of
       [] -> return (v1, [])
       _  -> do
         (plus, cs'') <- operatorParser cs'
         (v2, cs''')  <- arithParser cs''
         return (plus v1 v2, cs''')


intParser :: String -> Either SyntaxError (Expr, String)
intParser (c:rest)
  | isDigit c = intParser' (fromIntegral(ord c - ord '0')) rest
  | otherwise = Left ("syntax error in '" ++ (c:rest) ++ "', expected Expr digit")
  where
    intParser' :: Double -> String -> Either SyntaxError (Expr, String)
    intParser' n (c:cs)
      | isDigit c = intParser' (n* 10 + (fromIntegral (ord c - ord '0'))) cs
      | otherwise = Right (Val n, c:cs)
    intParser' n [] = Right (Val n, [])
intParser []  = Left ("syntax error in '" ++ [] ++ "', expected a digit")

operatorParser :: String -> Either SyntaxError (Expr -> Expr -> Expr, String)
operatorParser ('+':rest) = Right (Plus, rest)
operatorParser ('*':rest) = Right (Mul, rest)
operatorParser ('-':rest) = Right (Sub, rest)
operatorParser ('/':rest) = Right (Div, rest)
operatorParser ('^':rest) = Right (Pow, rest)
operatorParser s          = Left ("syntax error in '" ++ s ++ "', expected an operator ")

-- Fonctions
pow :: Double -> Double -> Double
pow base pui =
  if pui > 0
     then pow2 base (pui-1) base
     else base

pow2 :: Double -> Double -> Double -> Double
pow2 base pui res =
  if pui > 0
     then pow2 base (pui-1) (res*base)
     else res



