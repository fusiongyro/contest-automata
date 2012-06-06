module Automata.Automaton where

class Automaton a where
  -- | Parse a document into an automaton
  parseMachine     :: String -> Either String a
  evaluateMachine  :: a -> [String] -> Bool
  graphMachine     :: a -> String
  machineToHaskell :: a -> String

-- | Verify an automaton by running it against input with known behavior. 
-- True if the automaton is valid.
verifyMachine :: (Automaton a) => a -> [[String]] -> [Bool] -> Bool
verifyMachine machine input expectedOutput = 
  map (evaluateMachine machine) input == expectedOutput
