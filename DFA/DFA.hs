module DFA where

import Control.Applicative hiding (many, (<|>))

import Text.Parsec.Prim
import Text.Parsec.Char hiding (spaces)
import Text.Parsec.Combinator
import Text.Parsec.String


{-

Implementation of a simple DFA checker.

By Daniel Lyons <fusion@storytotell.org>, for the NMT ACM.

-}

-- This is a sample input file:
-- \// This is a comment\n\

sample :: String
sample = "q0, q1, q2, q3\n\
\#\n\
\1,0\n\
\#\n\
\q0:1->q3\n\
\q0:0->q1\n\
\\n\
\q1:1->q2\n\
\q1:0->q3\n\
\\n\
\q2:1->q3\n\
\q2:0->q1\n\
\\n\
\q3:1->q3\n\
\q3:0->q3\n\
\\n\
\#\n\
\q0\n\
\#\n\
\q0, q2\n"

{-

The file consists of the following sections:

    States
    #
    Alphabet
    #
    Transitions
    #
    Start State
    #
    Accepting States

States begin with q and are numbered. A state transition looks like
this:

    [State]:[Reading symbol] -> [New state]

Thus, q0:1->q3 means, in state q0, if you read 1 transition to state q3.

The state, alphabet and accepting state sections of the input file are
taken to be comma-separated lists of states or symbols.

-}

type Symbol = String
type Name = String
--data State = State Name [Transition]
data Transition = Transition { inState :: Name
                             , reading :: Symbol
                             , transitionTo :: Name
                             }
                deriving (Show, Eq)

data Document = Document { states :: [Name]
                         , alphabet :: [Symbol]
                         , transitions :: [Transition]
                         , startState :: Name
                         , acceptingStates :: [Symbol]
                         }
                deriving (Show)

spaces  =  many (char ' ')
       <|> (string "//" >> many (noneOf "\n"))

parseState :: Parser Name
parseState = do
  l <- letter
  rest <- many alphaNum
  spaces
  return $ l:rest

parseSymbol :: Parser Symbol
parseSymbol = many1 alphaNum

parseCommaSeparatedList item = sepBy (spaces >> item) (char ',') <* spaces

parseStateList :: Parser [Name]
parseStateList = parseCommaSeparatedList parseState <* spaces

parseAlphabet :: Parser [Symbol]
parseAlphabet = parseCommaSeparatedList parseSymbol <* spaces

parseTransition :: Parser Transition
parseTransition = do
  startState <- parseState
  char ':' >> spaces
  reading <- parseSymbol
  spaces
  string "->"
  spaces
  toState <- parseState
  spaces
  return $ Transition startState reading toState

parseTransitionList :: Parser [Transition]
parseTransitionList = endBy parseTransition (many (char '\n'))

parseDocument :: Parser Document
parseDocument = do
  states <- parseStateList <* newline <* separator
  symbols <- parseAlphabet <* newline <* separator
  transitions <- parseTransitionList <* separator
  startState <- parseState <* newline <* separator
  acceptingStates <- parseStateList
  return $ Document states symbols transitions startState acceptingStates
  where
    separator = char '#' >> newline

doc = case parse parseDocument "" sample of Right doc -> doc

generateFunctions :: Document -> String
generateFunctions doc = unlines $ map generateFunction' $ states doc
  where
    generateFunction' myState  = generateFunction myState (isAcceptingState myState doc) [ t | t <- transitions doc, inState t == myState ]
    isAcceptingState state doc = state `elem` (acceptingStates doc)

generateFunction :: Name -> Bool -> [Transition] -> String
generateFunction name isAccepting transitions = unlines (emptyCase : transCases)
  where
    emptyCase = name ++ " [] = " ++ (if isAccepting then "Accept" else "Fail")
    transCases = map generateCase transitions
    generateCase (Transition _ reading toState) = name ++ " (" ++ reading ++ ":xs) = " ++ toState ++ " xs"
