##
# This is the connector for project.
##

import sys, auto_grader, os


if __name__ == "__main__":
	
	##
	# This script simply calls the auto_grader.py with the 2 given command
	# line args and the correct submission dir.
	# Example: python connector.py 2 input.team1.nfa
	##

	problemNum = sys.argv[1]
	dfaCode = sys.argv[2]
	
	#TODO: perform some sort of check for folders in the Submission dir
	submissionDir="1"
	
	os.system("python auto_grader.py " + submissionDir + " " + problemNum
				+ " " + dfaCode)




