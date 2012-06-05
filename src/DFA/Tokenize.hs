module DFA.Tokenize 
  ( tokenize
  , Token(..)
  ) where

import Data.Char
import Control.Applicative ((<$>), (<*>), (<*), (*>))

import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Pos

data Token = TNewline
		   | TArrow
		   | TPound
		   | TColon
		   | TComma
		   | TString String
	deriving (Show, Eq)

dfatoken :: Parser Token
dfatoken  =  tArrow
         <|> tPound
         <|> tColon
         <|> tComma
         <|> tString
  where
    tArrow  = string "->" >> return TArrow
    tPound  = char '#' >> return TPound
    tColon  = char ':' >> return TColon
    tComma  = char ',' >> return TComma
    tString = TString <$> many1 alphaNum

comment :: Parser ()
comment = do
  try $ string "//"
  skipMany $ satisfy (/= '\n')
  return ()

skipJunk :: Parser ()
skipJunk  =  skipMany $ comment <|> skipMany1 (satisfy isSpace)

lexeme :: Parser Token
lexeme = do
  tok <- dfatoken
  skipJunk
  return tok

tokenize :: Parser [Token]
tokenize = many1 (skipJunk >> lexeme)