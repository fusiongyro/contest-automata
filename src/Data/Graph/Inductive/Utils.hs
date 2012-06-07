module Data.Graph.Inductive.Utils where

import Data.Graph.Inductive.Graph

-- | Find the label for this Node
label :: Graph gr => gr a b -> Node -> a
label graph = lab' . context graph
