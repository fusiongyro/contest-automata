module DFA.AST where

data Transition = T String String String
  deriving (Show, Eq)

data DFA = DFA 
  { initialState :: String
  , acceptingStates :: [String]
  , alphabet :: [String]
  , transitions :: [Transition]
  }
  deriving (Show, Eq)

