##
# This is the auto-grader for the project.  
##

import sys, shutil, os, dfaParser

##
# Given the teamname and the input (DFA) file name, converts user input
# file into haskell file and execute the file. The output is piped
# to out.dat.
##

def compileRun(teamname, dfa):
	os.system("python dfaParser.py " + dfa)
	os.system("ghc " + teamname + ".nfa.hs")
	os.system("./" + teamname + ".nfa > out.dat")

##
# Checks if the output from the users DFA is correct by comparing it
# with the solution file. (Script avaliable but no way to know teamname
# unless with a 3rd command-line arg)
##

def checkAnswer(teamname):
	#Output the differance between the solution and the outpuf from the produced NFA
	resultFile = "result." + teamname + ".dat"
	os.system("diff out.dat solution > " + resultFile)
	
	#If the result file is empty (its size is 0) the given DFA accpets the input string
	if os.stat(resultFile).st_size == 0:
		print "Correct!"
	else:
		print "Incorrect!"

##
# Parses the input file from the user and returns the teamname as a string
##
def parseTeamName(filename):
	list = filename.split(".", 3)
	return list[1]

if __name__ == "__main__":
	
	##
	# The script is called with 3 arguments. These args are as follows:
	# python auto_grader.py <submission dir> <problem number> <filename>
	# Example:  python auto_grader.py 1 21 xor.nfa
	#						                     
	
	submissionDir = sys.argv[1]
	problemNum = sys.argv[2]
	dfaCode = sys.argv[3]
	
	#Parse teamname from input file name
	teamname = parseTeamName(dfaCode)
	
	#Contruct the path where solution resides
	solutionPath = os.getcwd() + "/" + problemNum + "/solution"
	
	#Contruct the destination path for copy
	newSubDir = os.getcwd() + "/Submissions/" + submissionDir + "/"
	
	#Contruct the input directory path
	inputDir = os.getcwd() + "/" + problemNum + "/" + "input"
	
	#Perform both copy operations
	shutil.copy(dfaCode, newSubDir)
	shutil.copy(inputDir, newSubDir)
	shutil.copy("dfaParser.py", newSubDir)
	shutil.copy("dfaParser.pyc", newSubDir)
	
	#Change the current directory to the submission directory
	os.chdir(newSubDir)
	
	compileRun(teamname, dfaCode)
	
	#Copy over solution of the problem
	shutil.copy(solutionPath, newSubDir)
	
	checkAnswer(teamname)
	
	
