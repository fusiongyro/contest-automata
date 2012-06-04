module DFA.Parse where

import Control.Monad (void)
import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Data.Maybe
import Data.List

import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Prim hiding (State)
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

generateGraph :: [State] -> [Transition] -> Gr State String
generateGraph nodes transitions = mkGraph labeledNodes labeledTransitions
    where
      labeledNodes       = zipWith (,) [1..] nodes
      get n              = fst $ fromJust $ find (\(i, (State name _)) -> name == n) labeledNodes
      labeledTransitions = map (\(start,reading,end) -> (get start, get end, reading)) transitions

--nodeLabeled :: (Eq a, Graph g) => g a b -> a -> Maybe (LNode a)
--nodeLabeled g label = find (\(_,lab) -> label == lab) (labNodes g)

generateDFA :: [String] -> [String] -> [Transition] -> String -> [String] -> DFA
generateDFA stateNames alphabet transitions initialState acceptingStates = 
  DFA init alphabet graph
      where
        graph = (generateGraph states transitions)
        states = map (\n -> State n (n `elem` acceptingStates)) stateNames
        init = fromJust $ find (\(_, (State name _)) -> name == initialState) (labNodes graph)
        --accept = map (labeled graph) acceptingStates

labeled :: Graph gr => gr a b -> Node -> a
labeled graph = lab' . context graph

parseDFA :: TokenParser DFA
parseDFA = generateDFA <$> states <*> syms <*> trans <*> initial <*> parseStates
    where
      states  = parseStates <* tok TPound
      syms    = parseAlphabet <* tok TPound
      trans   = parseTransitions <* tok TPound
      initial = stringTok <* tok TPound

parseDFA' = do
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
