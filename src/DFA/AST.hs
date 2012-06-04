module DFA.AST where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

type Transition = (String, String, String)

data State = State { name :: String, isAccepting :: Bool }
             deriving (Show, Eq)

data DFA = DFA 
  { initialState :: LNode State
  , alphabet :: [String]
  , graph :: Gr State String
  }

instance Show DFA where
  show (DFA init alphabet g) = "DFA " ++ show (alphabet, init, labEdges g)
