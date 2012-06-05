module Main where

import DFA.Parse
import DFA.Codegen

main = interact $ tryGenerate . parseDFA
    where
      tryGenerate (Left s) = show s
      tryGenerate (Right dfa) = generate dfa