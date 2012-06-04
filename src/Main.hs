module Main where

import DFA.Parse
import DFA.Codegen

main = interact (tryGenerate . readDFA)
    where
      tryGenerate (Left s) = show s
      tryGenerate (Right dfa) = generate dfa