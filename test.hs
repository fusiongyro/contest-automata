module Main where

data Transition = T Char Node
    deriving (Show, Eq)
type Node = [Transition]
type Graph = G [Node]

graph :: Graph
graph = [q0, q1, q2, q3]
    where
        q0 = [T '1' q3, T '0' q1]
        q1 = [T '1' q2, T '0' q3]
        q2 = [T '1' q3, T '0' q1]
        q3 = [T '1' q3, T '0' q3]
