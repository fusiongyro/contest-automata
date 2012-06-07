module Automata.DFA.Evaluate (evaluateDFA) where

import Data.List
import Data.Maybe

import Data.Graph.Inductive.Utils
import Automata.DFA.AST

-- True if the DFA accepts this input.
evaluateDFA :: DFA -> [String] -> Bool
evaluateDFA (DFA initialState _ graph) input = isAccepting finalState
    where
      finalState :: State
      (_, finalState) = foldl (step graph) initialState input

-- | With respect to the supplied graph, find the next node
-- based on the current node and the current symbol.
step :: DFAGraph -> StateNode -> String -> StateNode
step graph (myId, _) reading =
  case find (matches myId reading) (labEdges graph) of
    Just (_, dest, _) -> (dest, label graph dest)
    Nothing           -> error $
      "can't find transition from " ++ show myId ++ " on reading " ++ reading

-- | Locate the appropriate edge for the given node/symbol combination
matches :: Node -> String -> DFAEdge -> Bool
matches myId reading (thisId, x, readingThis) = 
  (myId, reading) == (thisId, readingThis)