module Automata.DFA.Codegen (dfaToHaskell) where

import Data.Function
import Data.List
import Text.Printf

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Utils

import Automata.DFA.AST

filetop = unlines [ "module Main where"
                  , ""
                  , "main = do"
                  , "  input <- read $ getContents :: [String]"
                  , "  putStrLn $ initialState input" 
                  ]

header :: DFA -> String
header dfa@(DFA (_, (State initialState _)) _ _) = 
    printf "%s\ninitialState = %s\n" filetop initialState

dfaToHaskell :: DFA -> String
dfaToHaskell dfa@(DFA initialState alphabet graph) = 
  unlines $ header dfa : map generator (labNodes graph)
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
          "%s [] = \"%s\"" 
          (name me)
          (if isAccepting me then "Accept" else "Reject")
      
      -- Given a node, generate the inductive cases for it using the edges 
      -- of the graph
      genEdgeCases :: StateNode -> [String]
      genEdgeCases node@(myId, _) = map (genEdgeCase node) edges
          where
            edges = sortBy (compare `on` third) (out graph myId)
            third (_, _, th) = th
    
      -- Given a node and an edge, generate the corresponding inductive case
      genEdgeCase :: StateNode -> DFAEdge -> String
      genEdgeCase (myId, me) (_, toId, reading) = 
        printf 
          "%s (\"%s\":xs) = %s xs"
          (name me)
          reading
          (name (label graph toId))