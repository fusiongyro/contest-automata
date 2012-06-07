module Automata where

import Automata.Automaton
import Automata.DFA

-- This is a demonstration only. Will need to be fleshed out more once we have
-- PDA and Turing machine implementations.

data Machine = DFAMachine DFA | Other

instance Automaton Machine where
  evaluateMachine (DFAMachine d) = evaluateMachine d
  evaluateMachine (Other) = error "hi"
  
--  parseMachine s = case (parseMachine s :: Either String DFA) of
--    Left _ -> Right Other
--    Right dfa -> Right $ DFAMachine dfa
