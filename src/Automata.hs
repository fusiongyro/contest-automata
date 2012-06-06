module Automata where

import Automata.Automaton
import Automata.DFA

data Machine = DFAMachine DFA | Other

instance Automaton Machine where
  evaluateMachine (DFAMachine d) = evaluateMachine d
  evaluateMachine (Other) = error "hi"
  
  parseMachine s = case (parseMachine s :: Either String DFA) of
    Left _ -> Right Other
    Right dfa -> Right $ DFAMachine dfa
