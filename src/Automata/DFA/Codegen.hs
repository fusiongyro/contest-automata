module Automata.DFA.Codegen (dfaToHaskell) where

import Data.Function
import Data.List
import Text.Printf

import Automata.DFA.AST

filetop = unlines [ "module Main where"
                  , ""
                  , "main = do"
                  , "  contents <- lines `fmap` getContents"
                  , "  mapM_ (putStrLn . initialState . read) contents"
                  , ""
                  ]

header :: DFA -> String
header dfa = printf "%sinitialState = %s\n" filetop $ initialState dfa

dfaToHaskell :: DFA -> String
dfaToHaskell dfa = 
  unlines $ header dfa : map generator (nodes dfa)
    where
      -- Generates the function for each node in the graph
      generator :: Name -> String
      generator node = unlines $ genFunction node
      
      -- Given a node, generate its function by generating the base case
      -- and the inductive cases
      genFunction :: Name -> [String]
      genFunction node = genBaseCase node : genEdgeCases node
      
      -- Given a node, generate the base case for it
      genBaseCase :: Name -> String
      genBaseCase node = 
        printf 
          "%s [] = \"%s\"" 
          node
          (if isAccepting dfa node then "Accept" else "Reject")
      
      -- Given a node, generate the inductive cases for it using the edges 
      -- of the graph
      genEdgeCases :: Name -> [String]
      genEdgeCases node = map (genEdgeCase node) (edgesFrom dfa node)
    
      -- Given a node and an edge, generate the corresponding inductive case
      genEdgeCase :: Name -> Edge -> String
      genEdgeCase fromNode (reading, toNode) = 
        printf 
          "%s (\"%s\":xs) = %s xs"
          fromNode
          reading
          toNode