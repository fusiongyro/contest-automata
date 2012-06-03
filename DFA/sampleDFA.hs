module Main where

import Data.Maybe
import qualified Data.Map as M

evaluate :: String -> String -> Bool
evaluate state [] = state `elem` acceptingStates
evaluate state (x:xs) = evaluate newStateName xs
  where
    newStateName = (fromJust $ M.lookup state stateMap) x

acceptingStates = ["q0", "q2"]

stateMap = M.fromList [("q0", q0)
                      ,("q1", q1)
                      ,("q2", q2)
                      ,("q3", q3)]

-- generated states below
q0, q1, q2, q3 :: Char -> String
q0 '1' = "q3"
q0 '0' = "q1"

q1 '1' = "q2"
q1 '0' = "q3"

q2 '1' = "q3"
q2 '0' = "q1"

q3 '1' = "q3"
q3 '0' = "q3"
