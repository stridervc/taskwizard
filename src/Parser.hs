module Parser
  ( Parser
  , Filter (..)
  , eval
  ) where

import Task
import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
  []      -> []
  (x:xs)  -> [(x,xs)])

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

ident :: Parser String
ident = do
  x <- alphanum
  xs <- many alphanum
  return (x:xs)

nat :: Parser Integer
nat = do
  xs <- some digit
  return $ read xs

space :: Parser ()
space = do
  many (sat isSpace)
  return ()

int :: Parser Integer
int = do
    char '-'
    n <- nat
    return (-n)
  <|> nat

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

command :: Parser String
command =
  do
    c <- symbol "help"
    return c
  <|> do
    c <- symbol "list"
    return c
  <|> do
    c <- symbol "ls"
    return "list"
  <|> do
    c <- symbol "add"
    return c
  <|> do
    c <- symbol "start"
    return c
  <|> do
    c <- symbol "stop"
    return c
  <|> do
    c <- symbol "done"
    return c
  <|> do
    c <- symbol "delete"
    return c
  <|> do
    c <- symbol "modify"
    return c
  <|> do
    c <- symbol "show"
    return c
  <|> do
    c <- symbol "projects"
    return c

tfilter :: Parser Filter
tfilter =
  do
    i <- integer
    return (Fid i)
  <|> do
    t <- ident
    return (Ftext t)

filters :: Parser [Filter]
filters =
  do
    f <- tfilter
    fs <- many (do
      symbol ","
      tfilter)
    return (f:fs)

arguments :: Parser String
arguments = some item

data Filter = Fid ID
            | Ftext String
            deriving (Eq, Show)

type Command = String
type Arguments = String

-- [filter[,filter[,..]]] [command] [arguments]
expr :: Parser (Command, [Filter], Arguments)
expr =
  do
    fs <- filters
    c <- command
    args <- arguments
    return (c, fs, args)
  <|> do
    fs <- filters
    c <- command
    return (c, fs, "")
  <|> do
    c <- command
    args <- arguments
    return (c, [], args)
  <|> do
    c <- command
    return (c, [], "")
  <|> do
    fs <- filters
    return ("", fs, "")

eval :: String -> (Command, [Filter], Arguments)
eval xs = case (parse expr xs) of
  [(p,[])]  -> p
  [(_,out)] -> error ("Unused input: " ++ out)
  []        -> error ("Invalid input")
