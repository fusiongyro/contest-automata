module DFA.Codegen where

import Data.List
import Text.Printf

import Data.Graph.Inductive.Graph

import DFA.AST

{-
generate :: DFA -> String
generate doc = unlines $ map generateFunction' $ labNodes (graph doc)
  where
    generateFunction' myState  = generateFunction myState (isAcceptingState myState doc) [ t | t <- labEdges (graph doc), inState t myState ]
    isAcceptingState state doc = state `elem` (acceptingStates doc)
    states doc = nub (map (\(s, _, _) -> s) (labEdges (graph doc)) ++ map (\(_, _, s) -> s) (labEdges (graph doc)))
    inState (s, _, _) t = s == t

generateFunction :: String -> Bool -> [Transition] -> String
generateFunction name isAccepting transitions = unlines (emptyCase : transCases)
  where
    emptyCase = name ++ " [] = " ++ (if isAccepting then "Accept" else "Fail")
    transCases = map generateCase transitions
    generateCase (_, reading, toState) = name ++ " (" ++ reading ++ ":xs) = " ++ toState ++ " xs"
-}

generate :: DFA -> String
generate (DFA initialState acceptingStates alphabet graph) = unlines $ ufold generator [] graph
    where
      label = lab' . context graph
      generator (_, myId, me, outbound) c = (unlines $ genlinks myId me outbound) : c
      genlinks myId me outbound = genAccept myId me : map (genlink me) outbound
      genAccept myId me = printf "%s [] = %s" me (if isAccepting (myId, me) then "Accept" else "Reject")
      isAccepting lnode = lnode `elem` acceptingStates
      genlink from (reading, destination) = 
          printf "%s (\"%s\":xs) = %s xs" from reading (label destination)
