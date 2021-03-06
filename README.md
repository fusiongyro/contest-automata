# DFA

A DFA analysis program. Can:

 - parse a DFA file in our custom format
 - generate Haskell code that implements the DFA
 - execute the DFA
 - verify behavior of a DFA against expected output
 - output GraphViz dot format to generate images from DFAs

## Usage:

  Compile the DFA to Haskell source:

    dfa -c statemachine.dfa

  Evaluate the DFA against a given input:

    dfa -e input.txt statemachine.dfa

  Test the DFA against a given input/output set:

    dfa -t input-output.txt statemachine.dfa

  GraphViz for the DFA:

    dfa -g statemachine.dfa

## Formats

The evaluate input format is Haskell string lists separated by newlines, e.g.:

    ["0", "1"]
    ["0", "1", "0", "1"]

The test input format is similar:

    (["0", "1"], True)
    (["1", "1"], False)
