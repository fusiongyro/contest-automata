module PTokenize where -- (Token(..), tokenize) where

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
    tString = TString <$> (many1 alphaNum)

comment :: Parser ()
comment = do
  try $ string "//"
  skipMany $ satisfy (/= '\n')
  return ()

skipJunk  =  skipMany (comment <|> simpleSpace)
    where
      simpleSpace = skipMany1 (satisfy isSpace)

lexeme = do
  tok <- dfatoken
  skipJunk
  return tok

tokenize :: Parser [Token]
tokenize = many1 (skipJunk >> lexeme)

--
sample :: String
sample = "q0, q1, q2, q3\n\
\#\n\
\1,0\n\
\#\n\
\q0:1->q3\n\
\q0:0->q1\n\
\\n\
\// This is a comment\n\
\\n\
\q1:1->q2\n\
\q1:0->q3\n\
\\n\
\q2:1->q3\n\
\q2:0->q1\n\
\\n\
\q3:1->q3\n\
\q3:0->q3\n\
\\n\
\#\n\
\q0\n\
\#\n\
\q0, q2\n"
