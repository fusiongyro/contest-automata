module DFA.Parse (parseDFA) where

import Control.Monad (void)
import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Data.Maybe
import Data.List

import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Prim hiding (State)
import Text.Parsec.Pos
import Text.Parsec.Error

import Data.Graph.Inductive.Utils
import DFA.Tokenize
import DFA.AST

type TokenParser = GenParser Token ()
type TokenParserSt u = GenParser Token u

tokenEater
  :: (Stream s m a, Show a) => (a -> Bool) -> ParsecT s u m a
tokenEater matcher = 
  tokenPrim show (\oldSp _ _ -> setSourceLine oldSp (1 + sourceLine oldSp))
  (\x -> if matcher x then Just x else Nothing)
  
tok :: Token -> TokenParser Token
tok t = tokenEater (==t)

stringTok :: TokenParser String
stringTok = (\(TString s) -> s) <$> tokenEater stringLike
  where
    stringLike :: Token -> Bool
    stringLike (TString _) = True
    stringLike _ = False

parseCommaSeparatedStrings :: TokenParser [String]
parseCommaSeparatedStrings = sepBy stringTok (tok TComma)

parseStates   = parseCommaSeparatedStrings
parseAlphabet = parseCommaSeparatedStrings

parseTransition :: TokenParser Transition
parseTransition = 
  (,,) <$> stringTok <* tok TColon <*> stringTok <* tok TArrow <*> stringTok

parseTransitions :: TokenParser [Transition]
parseTransitions = many1 parseTransition

parseDFA' :: TokenParser DFA
parseDFA' = do
  states          <- parseStates       <* tok TPound
  alphabet        <- parseAlphabet     <* tok TPound
  transitions     <- parseTransitions  <* tok TPound
  initial         <- stringTok         <* tok TPound
  acceptingStates <- parseStates
  return $ makeDFA states alphabet initial acceptingStates transitions
    
parseDFA :: String -> Either ParseError DFA
parseDFA s = do
  tokens <- parse tokenize "" s
  parse parseDFA' "" tokens
