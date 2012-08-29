module Automata (
    module Automata.Automaton
  , module Automata.DFA
  , MachineType(..)
  , Machine(..)
  , parseMachine
  , ) where

import Control.Applicative

import Automata.Automaton
import Automata.DFA

-- This is a demonstration only. Will need to be fleshed out more once we have
-- PDA and Turing machine implementations.

data MachineType = DFAType | OtherType deriving (Show, Read, Eq)

data Machine = DFAMachine DFA | Other

instance Automaton Machine where
  evaluateMachine  (DFAMachine d) = evaluateMachine d
  evaluateMachine  (Other)        = error "Other machine types are not supported at this time, sorry."
                   
  graphMachine     (DFAMachine d) = graphMachine d
  graphMachine     (Other)        = error "Other machine types are not supported at this time, sorry."
  
  machineToHaskell (DFAMachine d) = machineToHaskell d
  machineToHaskell (Other)        = error "Other machine types are not supported at this time, sorry."

parseMachine :: MachineType -> String -> Either String Machine
parseMachine DFAType s = case parseDFA s of
  Right dfa -> Right $ DFAMachine dfa
  Left  err -> Left err