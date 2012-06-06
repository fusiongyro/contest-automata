module Automata.DFA (dfaToHaskell, parseDFA) where

import Automata.Automaton

import Automata.DFA.AST
import Automata.DFA.Codegen
import Automata.DFA.Parse
import Automata.DFA.Tokenize
import Automata.DFA.Evaluate

instance Automaton DFA where
  parseMachine     = parseDFA
  machineToHaskell = dfaToHaskell
  evaluateMachine  = evaluateDFA
  graphMachine     = undefined
