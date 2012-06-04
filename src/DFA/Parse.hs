module DFA.Parse where

import Control.Monad (void)
import Control.Applicative ((<$>), (<*>), (<*), (*>))

import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Pos
import Text.Parsec.Error

import DFA.Tokenize
import DFA.AST

type TokenParser = GenParser Token ()
type TokenParserSt u = GenParser Token u

tokenEater
  :: (Stream s m a, Show a) => (a -> Bool) -> ParsecT s u m a
tokenEater matcher = 
  tokenPrim show (\oldSp _ _ -> do
    setSourceLine oldSp (1 + sourceLine oldSp))
  (\x -> if matcher x then Just x else Nothing)
  
tok :: Token -> TokenParser Token
tok t = tokenEater (==t)

stringTok :: TokenParser String
stringTok = tokenEater stringLike >>= return . (\(TString s) -> s)
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
  T <$> stringTok <* (tok TColon) <*> stringTok <* (tok TArrow) <*> stringTok

parseTransitions :: TokenParser [Transition]
parseTransitions = many1 parseTransition

parseDFA :: TokenParser DFA
parseDFA = do
  parseStates
  tok TPound
  alphabet <- parseAlphabet
  tok TPound
  transitions <- parseTransitions
  tok TPound
  initialState <- stringTok
  tok TPound
  acceptingStates <- parseStates
  return $ DFA initialState acceptingStates alphabet transitions
  
readDFA :: String -> Either ParseError DFA
readDFA s = do
  tokens <- parse tokenize "" s
  parse parseDFA "" tokens
