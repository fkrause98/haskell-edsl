-- Nombre: Francisco Krause Arnim
-- APELLIDO: Krause Arnim
-- Universidad: Universidad de Buenos Aires
-- Libreta Universitaria: 99/19
-- Email: fkrausear@gmail.com
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parser where

import Control.Monad
import Data.Char (isDigit, isSpace, ord)

data UProp
  = Or UProp UProp
  | And UProp UProp
  | Not UProp
  | Eq UProp UProp
  | Lt UProp UProp
  | Var String
  deriving (Show)

-- | Parser type
newtype Parser a = P {runP :: String -> [(a, String)]}

instance Functor Parser where
  fmap f p = P $ \cs -> [(f a, cs') | (a, cs') <- runP p cs]

instance Applicative Parser where
  pure a = P (\cs -> [(a, cs)])

  -- (<*>) ::  Parser (a -> b) -> Parser a -> Parser b
  (P p) <*> (P q) = P $ \cs ->
    [ (f a, cs'') | (f, cs') <- p cs, (a, cs'') <- q cs'
    ]

instance Monad Parser where
  return a = P $ \cs -> [(a, cs)]
  (P p) >>= f = P $ \cs -> concat [runP (f a) cs' | (a, cs') <- p cs]

-- | Parsers primitivos
pFail :: Parser a
pFail = P $ \cs -> []

(<|>) :: Parser a -> Parser a -> Parser a
(P p) <|> (P q) = P $ \cs -> case p cs ++ q cs of
  [] -> []
  (x : xs) -> [x]

item :: Parser Char
item = P $ \cs -> case cs of
  "" -> []
  (c : cs) -> [(c, cs)]

pSat :: (Char -> Bool) -> Parser Char
pSat p = do
  c <- item
  if p c
    then return c
    else pFail

pSym :: Char -> Parser Char
pSym c = pSat (== c)

-- | p* (many) cero o más veces p
pList :: Parser a -> Parser [a]
pList p =
  do
    a <- p
    as <- pList p
    return (a : as)
    <|> return []

-- | p+ (some) una o más veces p
pListP :: Parser a -> Parser [a]
pListP p = do
  a <- p
  as <- pList p
  return (a : as)

-- | parsear una lista de dígitos

-- parsear un dígito y retornar el entero correspondiente
-- isDigit c = (c >= '0') && (c <= '9')

digit :: Parser Int
digit = do
  c <- pSat Data.Char.isDigit
  return (ord c - ord '0')

digits :: Parser [Int]
digits = pListP digit

whitespace :: Parser String
whitespace = P $ \input -> [(takeWhile Data.Char.isSpace input, dropWhile isSpace input)]

propParser :: Parser UProp
propParser = do
  whitespace
  prop <- orParser <|> termParser
  whitespace
  return prop

termParser :: Parser UProp
termParser = do
  whitespace
  prop <- andParser <|> factorParser
  whitespace
  return prop

factorParser :: Parser UProp
factorParser =
  do
    whitespace
    notParser
    <|> parParser propParser
    <|> eqParser
    <|> ltParser
    <|> varParser

notParser :: Parser UProp
notParser = do
  pSym '~'
  Not <$> propParser

eqParser :: Parser UProp
eqParser = do
  pSym '('
  p1 <- propParser
  pSym '='
  p2 <- propParser
  return $ Eq p1 p2

ltParser :: Parser UProp
ltParser = do
  pSym '('
  p1 <- propParser
  pSym '<'
  p2 <- propParser
  pSym ')'
  return $ Lt p1 p2

varParser :: Parser UProp
varParser = do
  digitsList <- digits
  let numStr = map (toEnum . (+ ord '0')) digitsList -- Convert the list of Ints back to a String
  return $ Var numStr

orParser :: Parser UProp
orParser = do
  term <- termParser
  pSym '\\'
  pSym '/'
  prop <- propParser
  return $ Or term prop

andParser :: Parser UProp
andParser = do
  factor <- factorParser
  pSym '/'
  pSym '\\'
  term <- termParser
  return $ And factor term

parParser :: Parser a -> Parser a
parParser p = do
  pSym '('
  result <- p
  pSym ')'
  return result

parseProp :: String -> [(UProp, String)]
parseProp = runP propParser
