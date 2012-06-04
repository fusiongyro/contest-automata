module DFA.Codegen where

import Data.List
import Text.Printf

import Data.Graph.Inductive.Graph

import DFA.AST

--isAccepting :: DFA -> LNode String -> Bool
--isAccepting (DFA _ accepting _ _) lnode = lnode `elem` accepting

label :: Graph gr => gr a b -> Node -> a
label graph = lab' . context graph

generate :: DFA -> String
generate dfa@(DFA initialState alphabet graph) = 
    unlines $ ufold generator [] graph
        where
          generator (_, myId, me, outbound) c = (unlines $ genlinks myId me outbound) : c
          genlinks myId me outbound = finalState : transitionStates
              where
                finalState       = accept myId me
                transitionStates = map (transition me) outbound

--          accept :: Node -> State -> String
          accept myId (State me accepting) = printf "%s [] = %s" me (if accepting then "Accept" else "Reject")

--          transition :: Node -> LEdge String -> String
          transition (State from _) (reading, destination) = 
              printf "%s (\"%s\":xs) = %s xs" from reading (name (label graph destination))
