module DFA.AST where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

type Transition = (String, String, String)

data DFA = DFA 
  { initialState :: LNode String
  , acceptingStates :: [LNode String]
  , alphabet :: [String]
  , graph :: Gr String String
  }

instance Show DFA where
  show (DFA init accept alphabet g) = "DFA " ++ show (alphabet, init, accept, labEdges g)