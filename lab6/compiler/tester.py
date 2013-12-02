#! /usr/bin/env python

import os
import sys
import getopt
import re
import subprocess

class col:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'

    def disable(self):
        self.HEADER = ''
        self.OKBLUE = ''
        self.OKGREEN = ''
        self.WARNING = ''
        self.FAIL = ''
        self.ENDC = ''

def validateTestRes(firstLine, asmTokens, fname):
  asmRes = int(asmTokens[0].split()[1])
  asmNumEx = int(asmTokens[1].split()[1])
  asmMemEx = int(asmTokens[2].split()[1])

  if (firstLine[1] == "return"):
    if (int(firstLine[2]) == asmRes):
      testPass(fname, '')
    else:
      testFail(fname, "Unexpected return failure : got %d but expected %s" % (asmRes, firstLine[2]))
  elif (firstLine[1] == "exception"):
    # for now we don't validate what sort of exception - just
    # that some exception occured
    if (asmNumEx | asmMemEx):
      testPass(fname, '')
    else:
      testFail(fname, "Unexpected exception flag %d, %d" % (asmNumEx, asmMemEx))
  else:
    print("poop")


def execC0File(fname):
  infile = open(fname, 'r')
  firstLine = infile.readline().split()
  testType = firstLine[1];
  print(testType)
  print(fname)
  compileProc = subprocess.Popen(['bin/l5c', '--asmjs', fname])
  compileProc.wait()
  if (compileProc.returncode != 0):
    #Pythons ternary operator is such nonsense, what a weird language
    testPass(fname,'') if (testType == 'error') else testFail(fname,"Compilation Failure")
  jsName = os.path.splitext(fname)[0] + '.js'
  print(jsName)
  asmProc = subprocess.Popen(['js', jsName], stdout=subprocess.PIPE)
  asmStdOut = asmProc.communicate()[0]
  asmTokens = asmStdOut.split('\n')
  print(asmTokens)
  if (asmProc.returncode != 0):
    testFail(fname, "Error from SM shell:")
  validateTestRes(firstLine, asmTokens, fname)

def testPass(fname, optMsg):
  print col.OKGREEN + 'PASS: ' + fname + " " + optMsg + col.ENDC
  exit(0)

def testFail(fname, optMsg):
  print col.FAIL + 'FAILURE: ' + fname + " " + optMsg + col.ENDC
  exit(0)

def main():
  try:
    myopts, args = getopt.getopt(sys.argv[1:], "f:")
  except getopt.GetoptError as e:
    print (str(e))
    print("Usage: %s -f c0file" % sys.argv[0])
    sys.exit(2)

  c0file = ''
  for o, a in myopts:
    if o == '-f':
      c0file = a
    else:
      print("Unsupported arg : %s" % o)
  if (len(myopts) <= 0):
    print("Usage: %s -f c0file" % sys.argv[0])
    sys.exit(2)
  execC0File(c0file)

if __name__ == "__main__":
  main()

