module DFA.Parse where

import Control.Monad (void)
import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Data.Maybe
import Data.List

import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Pos
import Text.Parsec.Error

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

import DFA.Tokenize
import DFA.AST

type TokenParser = GenParser Token ()
type TokenParserSt u = GenParser Token u

sample2 = sample

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
  (,,) <$> stringTok <* (tok TColon) <*> stringTok <* (tok TArrow) <*> stringTok

parseTransitions :: TokenParser [Transition]
parseTransitions = many1 parseTransition

generateGraph :: [String] -> [Transition] -> Gr String String
generateGraph nodes transitions = mkGraph labeledNodes labeledTransitions
    where
      labeledNodes       = zipWith (,) [1..] nodes
      get n              = fst $ fromJust $ find (\(i, name) -> name == n) labeledNodes
      labeledTransitions = map (\(start,reading,end) -> (get start, get end, reading)) transitions

nodeLabeled :: (Eq a, Graph g) => g a b -> a -> Maybe (LNode a)
nodeLabeled g label = find (\(_,lab) -> label == lab) (labNodes g)

generateDFA :: [String] -> [String] -> [Transition] -> String -> [String] -> DFA
generateDFA states alphabet transitions initialState acceptingStates = 
  DFA init accept alphabet graph
      where
        graph = (generateGraph states transitions)
        init = fromJust $ nodeLabeled graph initialState
        accept = map (fromJust . nodeLabeled graph) acceptingStates

parseDFA :: TokenParser DFA
parseDFA = do
  states <- parseStates
  tok TPound
  alphabet <- parseAlphabet
  tok TPound
  transitions <- parseTransitions
  tok TPound
  initialState <- stringTok
  tok TPound
  acceptingStates <- parseStates
  return $ generateDFA states alphabet transitions initialState acceptingStates
  
readDFA :: String -> Either ParseError DFA
readDFA s = do
  tokens <- parse tokenize "" s
  parse parseDFA "" tokens
