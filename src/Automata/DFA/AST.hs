module Automata.DFA.AST 
  ( Name
  , Symbol
  , Alphabet
  , Transition
  , Edge
  , DFA
  , makeDFA
  , nodes
  , edgesFrom
  , isAccepting
  , initialState
  , transitionFrom
  , evaluateDFA
  , toGraphViz) where

import Data.List
import Data.Maybe
import Text.Printf

import qualified Data.Set as S
import qualified Data.Map as M

type Name     = String
type Symbol   = String
type Alphabet = [Symbol]

type Transition = (Name, Symbol, Name)
type Edge = (Symbol, Name)

type EdgeMap = M.Map Name [Edge]

data DFA = DFA 
  { initialState     :: Name
  , alphabet         :: Alphabet
  , acceptingStates  :: S.Set Name
  , edges            :: EdgeMap
  } deriving (Show, Eq)

-- | Get all the nodes in this DFA
nodes :: DFA -> [Name]
nodes (DFA _ _ _ edges) = M.keys edges

-- | Get all the edges from this state in this DFA
edgesFrom :: DFA -> Name -> [Edge]
edgesFrom (DFA _ _ _ edges) name = M.findWithDefault [] name edges

-- | Find the new state given a DFA, a start state and a symbol
transitionFrom :: DFA -> Name -> Symbol -> Name 
transitionFrom dfa start reading = snd $ fromJust $ find matchingSymbol out
  where
    out = edgesFrom dfa start
    matchingSymbol (thisSymbol, _) = reading == thisSymbol

-- | True if this is an accepting state in this DFA
isAccepting :: DFA -> Name -> Bool
isAccepting (DFA _ _ acceptingStates _) state = S.member state acceptingStates

-- | Inserts a transition into the edge map (for construction).
insertTransition :: EdgeMap -> Transition -> EdgeMap
insertTransition map (from, reading, to) = 
  M.insertWith (++) from [(reading, to)] map

-- | True if this DFA accepts the supplied list of input symbols
evaluateDFA :: DFA -> [String] -> Bool
evaluateDFA dfa input = isAccepting dfa finalState
    where
      finalState = foldl (transitionFrom dfa) (initialState dfa) input

toGraphViz :: DFA -> String
toGraphViz (DFA initialState _ acceptingStates edges) =
    printf "digraph G {\n%s\n}" (unlines body)
        where
          body = M.foldrWithKey edgesToGraphViz [] edges
          edgesToGraphViz from edges rest = unlines (map (edgeToGraphViz from) edges) : rest
          edgeToGraphViz from (reading, to) = printf "%s -> %s [label=%s]" from to reading

-- | Generate the DFA from the stuff we have parsed.
makeDFA :: [Name] -> Alphabet -> Name -> [Name] -> [Transition] -> DFA
makeDFA stateNames alphabet initialState rawAcceptingStates transitions = 
  -- TODO: probably should validate this DFA's data to ensure 
  -- nothing is missing
  DFA initialState alphabet acceptingStates edges
    where
      acceptingStates = S.fromList rawAcceptingStates
      edges = foldl insertTransition M.empty transitions