module NanoParsec(
  Parser,
  runParser,
  number,
  parens,
  reserved,
  identifier,
  chainl,
  chainl1,
  list,
  skipSpace
  ) where

import Control.Applicative (Alternative (empty, many, some), (<|>))
import Data.Char (isDigit)
import Data.Function ((&))

newtype Parser a = Parser { parse :: String -> [(a,String)] }

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, rs)]   -> error $ "Parser did not consume entire stream. Remainder: " ++ rs
    _           -> error "Parser error."

instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative Parser where
  pure a = Parser (\s -> [(a,s)])
  (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Alternative Parser where
  empty = Parser (const [])

  p <|> q = Parser $ \s ->
    case parse p s of
      []  -> parse q s
      res -> res

-- |The `satisfy` function is the entry point for building parsers.
--  It produces a parser for a character satisfying the given predicate.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
  []     -> []
  (c:cs) -> if p c then [(c,cs)] else []

-- |Parser for any of the characters in the given string
oneOf :: [Char] -> Parser Char
oneOf s = satisfy (`elem` s)

-- |Parser for whitespace
spaces :: Parser String
spaces = many $ oneOf " \n\r"

-- |Given a parser it returns the parser which additionally removes trailing
--  whitespace.
skipSpace :: Parser a -> Parser a
skipSpace p =  p <* spaces

-- |Parser for the given character
char :: Char -> Parser Char
char c = satisfy (c ==)

-- |Parser for the given string
string :: String -> Parser String
string []     = pure []
string (c:cs) = pure (:) <*> char c <*> string cs

-- |Parser for the given reserved word (removing trailing whitespace)
reserved :: String -> Parser String
reserved s = skipSpace (string s)

-- |Parser for an identifier given the list of characters admitted for the
--  start of the identifier and those admitted for the remainder
identifier :: [Char]    -- ^characters admitted for the first letter of the id
           -> [Char]    -- ^characters admitted for the remainder letters
           -> Parser String
identifier startChar bodyChar = skipSpace $
  pure (:) <*> oneOf startChar <*> many (oneOf bodyChar)

-- |Parser for a digit
digit :: Parser Char
digit = satisfy isDigit

-- |Parser for a natural number
natural :: Parser Integer
natural = read <$> some digit

-- |Parser for an integer number
number :: Parser Integer
number = negate <$> (char '-' *> natural)
      <|> natural

-- |Parser for a parenthesized expression
parens :: Parser a -> Parser a
parens m = skipSpace (char '(')  *> m <* skipSpace (char ')')

-- |`chainl p op a` parses an expression of the form `a bop a bop ... bop a`
--  consisting of zero or more terms separated by binary operators.
--  [@p@]: is a parser for the operands
--  [@op@]: is a parser for the binary operation symbol.
--          Its value is a binary operator function
--  defaulting to `a` if no occurences are found.
chainl :: Parser a              -- ^ the parser for the operands
       -> Parser (a -> a -> a)  -- ^ the parser for the binary symbol
       -> a                     -- ^ default value if no opeand present
       -> Parser a
chainl p op = xchainl p op id

xchainl :: Parser a              -- ^ the parser for the operands
        -> Parser (a -> b -> b)  -- ^ the parser for the binary symbol
        -> (a -> b)              -- ^ function for transforming the last operand
        -> b                     -- ^ default value if no opeand present
        -> Parser b
xchainl p op f b = (xchainl1 p op f) <|> pure b

-- |`chainl1 p op` is the one-or-more version of `chainl`.
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = xchainl1 p op id

xchainl1 :: Parser a -> Parser (a -> b -> b) -> (a -> b) -> Parser b
xchainl1 p op f = rest
  where rest = pure (&) <*> p <*>
               -- parse p and yield its result as the first argument to the
               -- function obtained from below
                (
                   pure flip <*> (spaces *> op) <*> rest
                   -- parse op, then recurse on the remainder and then pass the
                   -- obtained value as the second argument to the result of op
                <|>
                   pure f
                   -- if there is no op to continue with, yield the identity
                )

list :: Parser a -> String -> Parser [a]
list p sep = xchainl p (reserved sep *> pure (:)) (:[]) []
