-- For Advent of Code 2021
--
-- By Truman Collins
-- December 2, 2021

{-# LANGUAGE LambdaCase #-}

module Parsers (
  Parser,
  parse,
  item,
  sat,
  digit,
  lower,
  upper,
  letter,
  alphanum,
  alphanumSp,
  ident,
  identWithSp,
  space,
  newline,
  spaceNotNL,
  char,
  string,
  nat,
  int,
  token,
  identifier,
  natural,
  integer,
  symbol,
  commaIdent,
  cslOfIdents,
  commaNat,
  cslOfNats,
  nats,
  parseNItems,

  badParseOfInput,
  badParseOfInputLines
) where

import Data.Char
import Control.Applicative


-- Parser functions.

-- A parser of a given type (a) is a function that takes a string and return of list of pairs of
-- things of type (a) and remaining strings. An empty list indicates the parseing function failed,
-- and a result with more than one pair indicates that there was more than one valid parse of a
-- thing of type (a) at the start of the input string.

-- This is necessary to allow the Parser type to be made into instances of classes. P is a dummy
-- constructor.

newtype Parser a = P (String -> [(a, String)])

-- Using this newtype, a parsing function can be applied to a string using a function that removes
-- the dummy constructor. Before the function can be applied, it needs to be removed from the
-- parser context. Just return the function.

parse :: Parser a -> String -> [(a, String)]
parse (P p) = p

-- Define a parser called item which just takes the first character and fails for empty input.

item :: Parser Char
item = P (\case
             []       -> []
             (x : xs) -> [(x, xs)])
--item = P (\str -> case str of
--                    []       -> []
--                    (x : xs) -> [(x, xs)])

-- This makes Parser part of the Functor class.
-- Now: parse (fmap toUpper item) "abc"
--      [('A', "bc")]

instance Functor Parser where
  fmap g p = P(\inp -> case parse p inp of
                         [] -> []
                         [(v, out)] -> [(g v, out)]
                         _          -> [])  -- This case won't ever happen, but removes warning.

-- Pure just adds its argument to the result in the parse pair.
-- for <*>, this will return the first and third character.
--   three :: Parser (Char, Char)
--   three = pure g <*> item <*> item <*> item
--     where g x y z = (x, z)

instance Applicative Parser where
  pure v = P (\inp -> [(v, inp)])
  pg <*> px = P (\inp -> case parse pg inp of
                           [] -> []
                           [(g, out)] -> parse (fmap g px) out
                           _          -> [])  -- This case won't ever happen, but removes warning.

-- Make this an instance of Monad. Now:
-- three :: Parser (Char, Char)
-- three = do
--   x <- item
--   item
--   z <- item
--   return (x, z)

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inp -> case parse p inp of
                         [] -> []
                         [(v, out)] -> parse (f v) out
                         _          -> [])  -- This case won't ever happen, but removes warning.

-- This is the alternative class associated with Applicative.

instance Alternative Parser where
  empty = P (const [])
  p <|> q = P (\inp -> case parse p inp of
                         [] -> parse q inp
                         [(v, out)] -> [(v,out)]
                         _          -> [])  -- This case won't ever happen, but removes warning.

-- Define a parser that succeeds for a single character if it
-- satisfies the given predicate.
-- For example:
-- digit :: Parser Char
-- digit = sat isDigit

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else empty

-- Now some basic parsers:

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

-- Parse an alpha-numeric character or space.

alphanumSp :: Parser Char
alphanumSp = sat isAlphaNum <|> sat (== ' ')

-- Parse an identifier beginning with a letter and alpha-numeric after that.

ident :: Parser String
ident = do
  x <- letter
  xs <- many alphanum
  return (x : xs)

-- Similar to the above, but it can start with a number too and have spaces in it.

identWithSp :: Parser String
identWithSp = do
  x <- alphanum
  xs <- many alphanumSp
  return (x : xs)

-- Consume spaces.

space :: Parser ()
space = do
  _ <- many (sat isSpace)
  return ()

-- Consume one or more newline characters.

newline :: Parser ()
newline = do
  _ <- many (sat (== '\n'))
  return ()

-- Consume whitespace except newlines.

spaceNotNL :: Parser ()
spaceNotNL = do
  _ <- many (sat isSpaceNotNL)
  return ()
  where
    isSpaceNotNL :: Char -> Bool
    isSpaceNotNL ch
      | isSpace ch && ch /= '\n' = True
      | otherwise = False

-- Match a specific character in the input.

char :: Char -> Parser Char
char x = sat (== x)

-- Match a specific string in the input.

string :: String -> Parser String
string [] = return []
string (x : xs) = do
  _ <- char x
  _ <- string xs
  return (x:xs)

-- Parse and return a natural number.

nat :: Parser Int
nat = do
  xs <- some digit
  return (read xs)

-- Parse and return an integer with optional negative sign.

int :: Parser Int
int = do
    _ <- char '-'
    n <- nat
    return (-n)
  <|>
    nat

-- Given a parser, return a new one that consumes any whitespace before and after the thing parsed.

token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

-- Tokenized things.

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

-- Parse a comma followed by an identifier, and return that string.

commaIdent :: Parser String
commaIdent = do
  _ <- symbol ","
  identifier

-- Parse a comma-separated list of identifiers and return the list of them.

cslOfIdents :: Parser [String]
cslOfIdents = do
  n  <- identifier
  ns <- many commaIdent
  return (n : ns)

-- Parse a comma followed by a natural number, and return that number.

commaNat :: Parser Int
commaNat = do
  _ <- symbol ","
  natural

-- Parse a comma-separated list of natural numbers and return the list of them.

cslOfNats :: Parser [Int]
cslOfNats = do
  n  <- natural
  ns <- many commaNat
  return (n : ns)

-- To parse a list of comma-separated numbers with the whole list in brackets:
-- parse nats "  [1, 2, 3 ] "
-- [([1,2,3], "")]

nats :: Parser [Int]
nats = do
  _ <- symbol "["
  ns <- cslOfNats
  _ <- symbol "]"
  return ns

-- Parse n items defined by the parser passed in, and return the in an in-order list.

parseNItems :: Int -> Parser a -> Parser [a]
parseNItems 0 _ = return []
parseNItems n parseFn = do
  thingParsed <- parseFn
  remainingThings <- parseNItems (n - 1) parseFn
  return (thingParsed : remainingThings)
  
-- Return false if the input resulted in a single valid parse.

badParseOfInput :: [(a, String)]-> Bool
badParseOfInput xs = null xs || (not . null . tail) xs || (not . null . snd . head) xs

-- Return false if all of the lines resulted in a single valid parse.

badParseOfInputLines :: [[(a, String)]]-> Bool
badParseOfInputLines xs = any null xs || (not . all (null . tail)) xs || (not . all (null . snd . head)) xs
