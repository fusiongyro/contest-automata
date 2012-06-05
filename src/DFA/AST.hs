module DFA.AST 
  ( module Data.Graph.Inductive.Graph
  , module Data.Graph.Inductive.Tree
  , Name
  , Symbol
  , Alphabet
  , StateNode
  , DFAEdge
  , Transition
  , DFAGraph
  , State(..)
  , DFA(..)
  , makeDFA) where

import Data.List
import Data.Maybe

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree

type Name     = String
type Symbol   = String
type Alphabet = [Symbol]

type StateNode = LNode State
type DFAEdge   = LEdge Symbol

type Transition = (Name, Symbol, Name)

type DFAGraph = Gr State Symbol

data State = State { name :: Name, isAccepting :: Bool }
  deriving (Show, Eq)

data DFA = DFA 
  { initialState :: StateNode
  , alphabet     :: Alphabet
  , graph        :: DFAGraph
  }

instance Show DFA where
  show (DFA init alphabet g) = "DFA " ++ show (alphabet, init, labNodes g, labEdges g)

-- Generate the DFA from the stuff we have parsed.
makeDFA :: [Name] -> Alphabet -> Name -> [Name] -> [Transition] -> DFA
makeDFA stateNames alphabet initialStateName acceptingStates transitions = 
  DFA initialState alphabet graph
    where
      graph :: DFAGraph
      graph = mkGraph nodes edges
      
      states :: [State]
      states = map makeState stateNames
      
      makeState :: Name -> State
      makeState name = State name (name `elem` acceptingStates)
      
      nodes :: [StateNode]
      nodes = zip [0..] states
      
      edges :: [DFAEdge]
      edges = map makeTransition transitions
      
      makeTransition :: Transition -> DFAEdge
      makeTransition (from, reading, to) = (fst $ theNode from, fst $ theNode to, reading)
      
      theNode :: String -> StateNode
      theNode named = fromJust $ find (byStateNodeNamed named) nodes
      
      byStateNodeNamed :: String -> StateNode -> Bool
      byStateNodeNamed thisName (_, State named _) = thisName == named
      
      initialState :: StateNode
      initialState = theNode initialStateName
