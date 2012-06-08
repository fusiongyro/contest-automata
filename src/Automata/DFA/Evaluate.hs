module Automata.DFA.Evaluate (evaluateDFA) where

import Data.List
import Data.Maybe

import Automata.DFA.AST

-- True if the DFA accepts this input.
evaluateDFA :: DFA -> [String] -> Bool
evaluateDFA dfa input = isAccepting dfa finalState
    where
      finalState = foldl (transitionFrom dfa) (initialState dfa) input