module Automata.DFA (dfaToHaskell, parseDFA, DFA) where

import Automata.Automaton

import Automata.DFA.AST
import Automata.DFA.Codegen
import Automata.DFA.Parse
import Automata.DFA.Tokenize

instance Automaton DFA where
  machineToHaskell = dfaToHaskell
  evaluateMachine  = evaluateDFA
  graphMachine     = undefined
