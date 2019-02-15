module Expr where

import Control.Applicative ((<|>))
import NanoParsec

data Expr
        = Add Expr Expr
        | Mul Expr Expr
        | Sub Expr Expr
        | Lit Integer
        | Id String
        deriving Show

{- Parsing -}

--        expr ::= term { addop term }*
expr :: Parser Expr
expr = term `chainl1` addop

--        term ::= factor { mulop factor }*
term :: Parser Expr
term = factor `chainl1` mulop

exprId :: Parser String
exprId = identifier ['a'..'z'] (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_'])

--        factor = "(" expr ")" | number.
factor :: Parser Expr
factor =  Lit <$> number
      <|> Id <$> exprId
      <|> parens expr

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reserved x *> pure f

--         mulop  = "*"
mulop :: Parser (Expr -> Expr -> Expr)
mulop = infixOp "*" Mul

--         addop  = "+" | "-"
addop :: Parser (Expr -> Expr -> Expr)
addop = (infixOp "+" Add) <|> (infixOp "-" Sub)


{- Parsing the environment -}

type Assignment = (String, Integer)
type Env = [Assignment]

assignment :: Parser Assignment
assignment = pure (,) <*> (exprId <* reserved "=") <*> number

env :: Parser Env
env = list assignment ";"

expEnv :: Parser (Expr, Env)
expEnv = pure (,) <*> skipSpace expr <*> (reserved "where" *> env <|> pure [])

run :: String -> (Expr, Env)
run = runParser expEnv

{- Evaluation -}
find :: String -> Env -> Integer
find x e = head [v | (y,v) <- e, y == x]

eval :: Expr -> (Env -> Integer)
eval (Lit i)     = pure i
eval (Id x)      = find x
eval (Add e1 e2) = pure (+) <*> eval e1 <*> eval e2
eval (Sub e1 e2) = pure (-) <*> eval e1 <*> eval e2
eval (Mul e1 e2) = pure (*) <*> eval e1 <*> eval e2
