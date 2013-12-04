#! /usr/bin/env python

import os
import sys
import getopt
import re
import glob
import subprocess

class col:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    HAPPYPURPLE = '\033[95m'
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


def execC0File(fname, quiet):
  infile = open(fname, 'r')
  firstLine = infile.readline().split()
  testType = firstLine[1];
  compileProc = subprocess.Popen(['bin/l5c', '-l', '15411.h0','--asmjs', fname])
  compileProcOut = compileProc.communicate()[0]
  compileProc.wait()
  if (compileProc.returncode != 0):
    #Pythons ternary operator is such nonsense, what a weird language
    testPass(fname,'') if (testType == 'error') else testFail(fname,compileProcOut + "Compilation Failure")
    return
  if (not quiet):
    print("CompilationMsgs: " + str(compileProcOut))

  jsName = os.path.splitext(fname)[0] + '.js'
  asmProc = subprocess.Popen(['js', '-b', '-j', jsName], stdout=subprocess.PIPE)
  asmStdOut = asmProc.communicate()[0]
  asmTokens = asmStdOut.split('\n')
  if (asmProc.returncode != 0):
    print(asmStdOut)
    testFail(fname, "Error from SM shell:")
  if (not quiet):
    print("ExecutionMsgs: " + str(asmStdOut))
  validateTestRes(firstLine, asmTokens, fname)

def execC0Dir(dname, quiet):
  print(dname)
  if (not os.path.isdir(dname)):
    print("Invalid directory")
    exit(2)
  pth = os.path.realpath(dname)
#  os.chdir(pth)
  files = glob.glob(pth + '/*.l[1-4]')
  for file in files:
    filePath = file
    try:
      execC0File(filePath, True)
    except: 
      testFail(file, 'Unknown test failure')


def testPass(fname, optMsg):
  print col.HAPPYPURPLE + 'PASS: ' + fname + " " + optMsg + col.ENDC

def testFail(fname, optMsg):
  print col.FAIL + 'FAILURE: ' + fname + " " + optMsg + col.ENDC

def main():
  try:
    myopts, args = getopt.getopt(sys.argv[1:], "f:d:q")
  except getopt.GetoptError as e:
    print (str(e))
    print("Usage: %s -f c0file" % sys.argv[0])
    sys.exit(2)

  c0file = ''
  testDir = ''
  quiet = False
  dirEx = False

  for o, a in myopts:
    if o == '-f':
      c0file = a
    elif o == '-q':
      quiet = True
    elif o == '-d':
      dirEx = True
      testDir = a
    else:
      print("Unsupported arg : %s" % o)
  if (len(myopts) <= 0):
    print("Usage: %s -f c0file" % sys.argv[0])
    sys.exit(2)
  if (dirEx):
    execC0Dir(testDir, quiet)
  else:
    execC0File(c0file, quiet)

if __name__ == "__main__":
  main()
