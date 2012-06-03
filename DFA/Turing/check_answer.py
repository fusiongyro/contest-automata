
##
# This script compares the output from a users haskell file to the
# correct solution and pipes the output to another file. 
# Note: NOT USED (need to add teamname to out.dat to have result file)
##


import sys

if __name__ == "__main__":
	dfaFile = sys.argv[1]
	correctFile = sys.argv[2]
	teamname = sys.argv[3]
	
	resultFile = "result." + teamname + ".dat"
	
	os.system("diff out.dat solution > " + resultFile)
	
