module Main where

import DFA.Parse

main = interact (show . readDFA)