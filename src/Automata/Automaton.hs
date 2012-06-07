module Automata.Automaton where

class Automaton a where
  -- | Evaluates this automaton with the given input sequence, 
  -- returning true if it arrived in an accepting state
  evaluateMachine  :: a -> [String] -> Bool

  -- | Convert this automaton into GraphViz input text
  graphMachine     :: a -> String

  -- | Convert this automaton into executable Haskell code 
  -- suitable for standalone compilation
  machineToHaskell :: a -> String

-- | Verify an automaton by running it against input with known behavior. 
-- True if the automaton is valid.
verifyMachine :: (Automaton a) => a -> [[String]] -> [Bool] -> Bool
verifyMachine machine input expectedOutput = 
  map (evaluateMachine machine) input == expectedOutput
