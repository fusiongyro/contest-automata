module PParse where

import Control.Monad (void)
import Control.Applicative ((<$>), (<*>), (<*), (*>))

import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Pos

import PTokenize

type TokenParser = GenParser Token ()
type TokenParserSt u = GenParser Token u

data Transition = T String String String
  deriving (Show, Eq)

data DFA = DFA 
  { initialState :: String
  , acceptingStates :: [String]
  , alphabet :: [String]
  , transitions :: [Transition]
  }

tokenEater
  :: (Stream s m a, Show a) => (a -> Bool) -> ParsecT s u m a
tokenEater matcher = 
  tokenPrim show (\oldSp _ _ -> do
    setSourceLine oldSp (1 + sourceLine oldSp))
  (\x -> if matcher x then Just x else Nothing)
  
tok :: Token -> TokenParser Token
tok t = tokenEater (==t)

tokenToString :: Token -> String
tokenToString (TString s) = s

tokenString :: TokenParser String
tokenString = tokenEater stringLike >>= return . tokenToString
  where
    stringLike :: Token -> Bool
    stringLike (TString _) = True
    stringLike _ = False

tokenConst :: Token -> TokenParser ()
tokenConst t = void $ tok t

tokenColon, tokenArrow, tokenPound :: TokenParser ()
tokenColon = tokenConst TColon
tokenArrow = tokenConst TArrow
tokenPound = tokenConst TPound

-- parseTest (tok TColon) [TComma]
parseCommaSeparatedStrings :: TokenParser [String]
parseCommaSeparatedStrings = sepBy tokenString (tok TComma)

parseStates   = parseCommaSeparatedStrings
parseAlphabet = parseCommaSeparatedStrings

parseTransition :: TokenParser Transition
parseTransition = 
  T <$> tokenString <* tokenColon <*> tokenString <* tokenArrow <*> tokenString

parseTransitions :: TokenParser [Transition]
parseTransitions = many1 parseTransition

parseDFA :: TokenParser DFA
parseDFA = do
  parseStates
  tokenPound
  alphabet <- parseAlphabet
  tokenPound
  transitions <- parseTransitions
  tokenPound
  acceptingStates <- parseStates
  tokenPound
  initialState <- tokenString
  return $ DFA initialState acceptingStates alphabet transitions