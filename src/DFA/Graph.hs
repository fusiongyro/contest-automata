module DFA.Graph where

import Data.Maybe
import Data.List
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph

import DFA.AST

toGraph :: DFA -> Gr String String
toGraph dfa = mkGraph nodes edges
    where
      allNodes = nub $ map (\(T s _ _) -> s) (transitions dfa) ++ map (\(T _ _ s) -> s) (transitions dfa)
      nodes = zipWith (,) [1..] allNodes
      nodeId name = fromJust $ find (\(_,n1) -> n1 == name) nodes
      edges = map (\(T start read end) -> (fst (nodeId start), fst (nodeId end), read)) (transitions dfa)
