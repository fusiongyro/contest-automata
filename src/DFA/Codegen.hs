module DFA.Codegen where

import Data.List
import Text.Printf

import Data.Graph.Inductive.Graph

import DFA.AST

label :: Graph gr => gr a b -> Node -> a
label graph = lab' . context graph

generate :: DFA -> String
generate dfa@(DFA initialState alphabet graph) = 
    unlines $ map generator (labNodes graph)
        where
          generator :: StateNode -> String
          generator (myId, me) = unlines $ generateFunction (myId, me)
          
          generateFunction :: StateNode -> [String]
          generateFunction node = generateBaseCase node : generateEdgeCases node
          
          generateBaseCase :: StateNode -> String
          generateBaseCase (myId, me) = 
            printf 
              "%s [] = %s" 
              (name me)
              (if isAccepting me then "Accept" else "Reject")
          
          generateEdgeCases :: StateNode -> [String]
          generateEdgeCases node@(myId, _) = map (generateEdgeCase node) (out graph myId)

          generateEdgeCase :: StateNode -> DFAEdge -> String
          generateEdgeCase (myId, me) (_, toId, reading) = 
            printf 
              "%s (\"%s\":xs) = %s xs"
              (name me)
              reading
              (name (label graph toId))