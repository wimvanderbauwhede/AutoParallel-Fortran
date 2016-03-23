from subprocess import *
import sys

# Simple script to determine a number of lines and a percentage of lines that have been changed between original source code
# and output host code. An attempt to estimate how much of the code was parallelised

def getOriginalLines():
	originalFilename = sys.argv[1]
	lines = Popen("cat " + originalFilename + " | wc -l", stdout=PIPE, shell=True).stdout.read()
	return float(lines)

def getSameLines():
	originalFilename = sys.argv[1]
	newFileName = sys.argv[2]
	lines = Popen("fgrep -x -f " + originalFilename + " " + newFileName + " | wc -l", stdout=PIPE, shell=True).stdout.read()
	return float(lines)

def percentageChange():
	original = getOriginalLines()
	matching = getSameLines()
	percentageChange = (original - matching) / original
	return percentageChange*100

def absoluteChange():
	original = getOriginalLines()
	matching = getSameLines()
	absoluteChange = original - matching
	return int(absoluteChange)

print "Absolute Change: " + str(absoluteChange()) + " lines\tPercentage Change: " + str(percentageChange())