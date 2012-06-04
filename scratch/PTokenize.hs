module PTokenize (Token(..), tokenize) where

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
    tString = TString <$> (many1 alphaNum)

comment :: Parser ()
comment = string "//" >> many (noneOf "\n") >> newline >> return ()

skipJunk  =  comment
         <|> spaces

tokenize :: Parser [Token]
tokenize = endBy dfatoken skipJunk
