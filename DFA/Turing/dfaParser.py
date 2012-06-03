##
# Parse an input file and write out a haskell file that tests a NFA
##

##
# Read Input File
# The users NFA, as defined by a a five tuple 
##


import sys

def readInputFile(filename):
    file = open(filename, "r")

    text = ""
    for line in file.readlines():
        if "//" not in line:
            text = text + line.strip() +"\n"

    return text
        
##
# Read Haskell File 
# Haskell file contains the test cases for the NFA and a main to start
# the program 
##
def readHaskellFile(filename):
    text = open(filename, "r").read()

    return text

##
# Write Output File
# The file to be run to see if the user's NFA works
##
def writeFile(filename, text):
    file = open(filename, "w")
    file.write(text)

##
# Parse the Five Tuple 
##
def parseFiveTuple(text):
    textFiveTuple  = ["", "", "", "", ""]
    indexFiveTuple = 0

    for line in text.split():
        if "#" in line:
            indexFiveTuple = indexFiveTuple + 1
        else:
            textFiveTuple[indexFiveTuple] = textFiveTuple[indexFiveTuple] + \
                                            line.strip() + "\n"

    return textFiveTuple

##
# Parse States (Q)
# Example: 
# q1, q2, .., qn 
# For 0 in the input string at q2 go to q3
##
def parseStates(stateText):
    allStates = map(str.strip, stateText.split(","))

    haskellText = ""
    for state in allStates:
        haskellText = haskellText + state + ":: [Int] -> IO()\n"

    return haskellText

##
# Parse Set of Inputs Symbols (Sigma)
# The langauage used in the machine. Example:
# {0,1}*, which contains the strings: "10" "000", "0110", etc
##
def parseSetOfInputSymbols(textSymbols):
    pass

##
# Transition Function (Q*Sigma)
# Example: 
# q3:0->q7, from state q3 on input 0 go to state q7
##
def parseTranisitionFunction(transitionFunctionText):
    allTransitions = map(str.strip, transitionFunctionText.split())

    haskellText = ""
    for transition in allTransitions:
        initState, rawString = transition.split(":")
        a, finalState = rawString.split("->")

        haskellText = haskellText+initState+" ("+a+":xs) = "+finalState+" xs\n"

    return haskellText

        

###
# Initial State
# The State to start the machine at. 
# Example: 
# q9
###
# inputString := the string that contains the start state 
# executeString := the string to be writen to the file to execute 
#
def parseInitialState(inputString):
    initStateText = inputString.split(",")[0].strip()
    startString = "main = mapM "+initStateText+" test\n"

    return startString 

###
# Initial Final 
# The State to start the machine at. 
# Example: 
# q2:q3:q7, a set of accepting final states 
##
def parseFinalStates(finalStatesText, stateText):
    finalStates = map(str.strip, finalStatesText.strip().split(","))
    allStates = map(str.strip, stateText.split(","))

    haskellText = ""
    for final in finalStates:
        allStates.remove(final) # remove the start state
        haskellText = haskellText + final+' [] = print "Solution"\n' 

    for state in allStates:
        haskellText = haskellText + state+' [] = print "Not Solution"\n'
        
    return haskellText 
##
# put function definitions together. Haskell does not allow functions to be 
# randomly assorted in the file. 
##
def weaveFunctionsTogether(endState, otherStates):
    listOfEndStates   = endState.strip().split("\n")
    listOfOtherStates = otherStates.strip().split("\n")

    newFunctionLayout = ""
    for state in listOfEndStates:
        newFunctionLayout = newFunctionLayout + state +"\n"
        for other in listOfOtherStates:
            
            if state.split()[0] in other.split()[0]:
                newFunctionLayout = newFunctionLayout + other+"\n"
                #listOfOtherStates.remove(other)
        newFunctionLayout = newFunctionLayout + "\n"

    return newFunctionLayout
    
def parseTeamName(filename):
	list = filename.split(".", 3)
	return list[1]

if __name__ == "__main__":
    ##
    # inputString := the contents of the file the user made for the NFA
    # fiveTupleString := parseTheFiveTuple(inputString),
    #                    an array with the strings that represent the NFA tuple
    # executeString := executeString + parsed(fiveTupleString), 
    #                  the string to write to the file to later execute
    ##
    inputString = readInputFile(sys.argv[1])
    testString  = readInputFile("input")
    teamname = parseTeamName(sys.argv[1])

    fiveTupleString = parseFiveTuple(inputString)

    haskellFunctionDefinitions = parseStates(fiveTupleString[0])
    haskellMain                = parseInitialState(fiveTupleString[0])
    haskellEndStates           = parseFinalStates(fiveTupleString[3], \
                                                  fiveTupleString[0])
    haskellStateTrnsitions     = parseTranisitionFunction(fiveTupleString[2])

    finalString = haskellFunctionDefinitions+"\n"
    finalString = finalString + testString+"\n"
    finalString = finalString + haskellMain+"\n"
    finalString = finalString + weaveFunctionsTogether(haskellEndStates, \
                                                       haskellStateTrnsitions)
                                                       
    writeFile(teamname+".nfa.hs", finalString) 
