module DFA.Codegen (generate) where

import Data.List
import Text.Printf

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Utils

import DFA.AST

generate :: DFA -> String
generate dfa@(DFA initialState alphabet graph) = 
  unlines $ map generator (labNodes graph)
    where
      -- Generates the function for each node in the graph
      generator :: StateNode -> String
      generator (myId, me) = unlines $ genFunction (myId, me)
      
      -- Given a node, generate its function by generating the base case
      -- and the inductive cases
      genFunction :: StateNode -> [String]
      genFunction node = genBaseCase node : genEdgeCases node
      
      -- Given a node, generate the base case for it
      genBaseCase :: StateNode -> String
      genBaseCase (myId, me) = 
        printf 
          "%s [] = %s" 
          (name me)
          (if isAccepting me then "Accept" else "Reject")
      
      -- Given a node, generate the inductive cases for it using the edges 
      -- of the graph
      genEdgeCases :: StateNode -> [String]
      genEdgeCases node@(myId, _) = map (genEdgeCase node) (out graph myId)
    
      -- Given a node and an edge, generate the corresponding inductive case
      genEdgeCase :: StateNode -> DFAEdge -> String
      genEdgeCase (myId, me) (_, toId, reading) = 
        printf 
          "%s (\"%s\":xs) = %s xs"
          (name me)
          reading
          (name (label graph toId))