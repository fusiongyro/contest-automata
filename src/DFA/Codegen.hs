module DFA.Codegen where

import Data.List

import DFA.AST

generate :: DFA -> String
generate doc = unlines $ map generateFunction' $ states doc
  where
    generateFunction' myState  = generateFunction myState (isAcceptingState myState doc) [ t | t <- transitions doc, inState t myState ]
    isAcceptingState state doc = state `elem` (acceptingStates doc)
    states doc = nub (map (\(T s _ _) -> s) (transitions doc) ++ map (\(T _ _ s) -> s) (transitions doc))
    inState (T s _ _) t = s == t

generateFunction :: String -> Bool -> [Transition] -> String
generateFunction name isAccepting transitions = unlines (emptyCase : transCases)
  where
    emptyCase = name ++ " [] = " ++ (if isAccepting then "Accept" else "Fail")
    transCases = map generateCase transitions
    generateCase (T _ reading toState) = name ++ " (" ++ reading ++ ":xs) = " ++ toState ++ " xs"
