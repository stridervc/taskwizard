module Parser
  ( Parser
  , Filter (..)
  , eval
  ) where

import Task
import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a,String)])

instance Functor Parser where
  fmap g p = P (\inp -> case parse p inp of
    []        -> []
    [(v,out)] -> [(g v, out)])

instance Applicative Parser where
  pure v = P (\inp -> [(v,inp)])

  pg <*> px = P (\inp -> case parse pg inp of
    []        -> []
    [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
  p >>= f = P (\inp -> case parse p inp of
    []        -> []
    [(v,out)] -> parse (f v) out)

instance Alternative Parser where
  empty = P (\inp -> [])

  p <|> q = P (\inp -> case parse p inp of
    []        -> parse q inp
    [(v,out)] -> [(v,out)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
  []      -> []
  (x:xs)  -> [(x,xs)])

-- if a character satisfies predicate, return it parsed
sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else empty

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

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do
  char x
  string xs
  return (x:xs)

-- an identifier is one or more alphanumerical characters
ident :: Parser String
ident = do
  x <- alphanum
  xs <- many alphanum
  return (x:xs)

-- a natural number of 1 or more digits
nat :: Parser Integer
nat = do
  xs <- some digit
  return $ read xs

-- zero or more spaces
space :: Parser ()
space = do
  many (sat isSpace)
  return ()

-- a natural number optionally preceded by a '-'
int :: Parser Integer
int = do
    char '-'
    n <- nat
    return (-n)
  <|> nat

-- a token is anything preceded and followed by zero or more spaces
-- eg, a word in a sentence, no spaces in the token
token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

identifier :: Parser String
identifier = token ident

natural :: Parser Integer
natural = token nat

integer :: Parser Integer
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

projectP :: Parser Project
projectP =
  do
    symbol "proj"
    symbol ":"
    p <- identifier
    return p

-- Either a task id or search text
data Filter = Fid ID
            | Ftext String
            | Fproject Project
            deriving (Eq, Show)

tfilter :: Parser Filter
tfilter =
  do
    i <- integer
    return (Fid i)
  <|> do
    p <- projectP
    return (Fproject p)
  <|> do
    t <- identifier
    return (Ftext t)

filters :: Parser [Filter]
filters =
  do
    f <- tfilter
    fs <- many (do
      symbol ","
      tfilter)
    return (f:fs)

type Arguments = String

arguments :: Parser Arguments
arguments = some item

type Command = String

-- [filter[,filter[,..]]] [command] [arguments]
expr :: Parser (Command, [Filter], Arguments)
expr =
  do
    c <- symbol "help"
    return (c, [], "")
  <|> do
    c <- symbol "projects"
    return (c, [], "")
  -- commands that require filters and no arguments
  <|> do
    fs <- filters
    c <-  (   symbol "start"
          <|> symbol "stop"
          <|> symbol "done"
          <|> symbol "delete"
          <|> symbol "show"
          )
    return (c,fs,"")
  -- commands that require both a filter and arguments
  <|> do
    fs <- filters
    c <- symbol "modify"
    args <- arguments
    return (c, fs, args)
  -- list with filters and command
  <|> do
    fs <- filters
    c <- (symbol "list" <|> symbol "ls")
    return ("list", fs, "")
  -- commands that require no filters but do require arguments
  <|> do
    c <- symbol "add"
    args <- arguments
    return (c, [], args)
  <|> do
    c <- (symbol "list" <|> symbol "ls")
    return ("list", [], "")
  <|> do
    fs <- filters
    return ("list", fs, "")

eval :: String -> Either String (Command, [Filter], Arguments)
eval xs = case (parse expr xs) of
  [(p,[])]  -> Right p
  [(_,out)] -> Left $ "Unused input: " ++ out
  []        -> Left $ "Invalid input"
