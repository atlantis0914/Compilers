#! /usr/bin/env python

import os
import sys
import getopt
import re
import glob
import subprocess
import threading

class C0ErrTest(object):
    def __init__(self, cmd, firstLine, fname):
        self.cmd = cmd
        self.process = None
        self.firstLine = firstLine
        self.fname = fname

    def run(self, timeout):
        def target():
            self.process = subprocess.Popen(self.cmd, stdout=subprocess.PIPE)
            asmStdOut = self.process.communicate()[0]
            asmTokens = asmStdOut.split('\n')
            if (self.process.returncode == 0):
              validateTestRes(self.firstLine, asmTokens, self.fname)

        thread = threading.Thread(target=target)
        thread.start()

        thread.join(timeout)
        if thread.is_alive():
            print 'Test Timeout: Validating...'
            self.process.terminate()
            thread.join()
            validateTimeout(self.firstLine, self.fname)

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

def validateTimeout(firstLine, fname):
  if (firstLine[1] == "exception"):
    testPass(fname, '')
  else:
    testFail(fname, "Unexpected timeout")

def validateTestRes(firstLine, asmTokens, fname):
  if (len(asmTokens) != 0):
    asmRes = int(asmTokens[0].split()[1])
    asmNumEx = int(asmTokens[1].split()[1])
    asmMemEx = int(asmTokens[2].split()[1])
    asmTimeoutEx = 0
  else:
    asmRes = 0
    asmNumEx = 0
    asmMemEx = 0
    asmTimeoutEx = 1

  if (firstLine[1] == "return"):
    if (int(firstLine[2]) == asmRes and (asmNumEx != 1) and (asmMemEx != 1)):
      testPass(fname, '')
    else:
      testFail(fname, "Unexpected return failure : got %d but expected %s" % (asmRes, firstLine[2]))
  elif (firstLine[1] == "exception"):
    # for now we don't validate what sort of exception - just
    # that some exception occured
    if (asmNumEx | asmMemEx | asmTimeoutEx):
      testPass(fname, '')
    else:
      testFail(fname, "Unexpected exception flag %d, %d" % (asmNumEx, asmMemEx))
  else:
    print("poop")


def execC0File(fname, quiet, errCase):
  infile = open(fname, 'r')
  firstLine = infile.readline().split()
  testType = firstLine[1];
  compileProc = subprocess.Popen(['bin/l4c', '-l', '15411.h0','--asmjs', fname])
  compileProcOut = compileProc.communicate()[0]
  compileProc.wait()
  if (compileProc.returncode != 0):
    #Pythons ternary operator is such nonsense, what a weird language
    testPass(fname,'') if (testType == 'error') else testFail(fname,str(compileProcOut) + "Compilation Failure")
    return
  if (not quiet):
    print("CompilationMsgs: " + str(compileProcOut))

  jsName = os.path.splitext(fname)[0] + '.js'
  asmCmd = ['js', '-b', '-j', jsName]
  if (errCase): 
    err_test = C0ErrTest(asmCmd, firstLine, fname)
    err_test.run(timeout=5)
  else:
    asmProc = subprocess.Popen(['js', '-b', '-j', jsName], stdout=subprocess.PIPE)
    asmStdOut = asmProc.communicate()[0]
    asmTokens = asmStdOut.split('\n')
    if (asmProc.returncode != 0):
      print(asmStdOut)
      testFail(fname, "Error from SM shell:")
    if (not quiet):
      print("ExecutionMsgs: " + str(asmStdOut))
    validateTestRes(firstLine, asmTokens, fname)

def execC0Dir(dname, quiet, errCase):
  print(dname)
  if (not os.path.isdir(dname)):
    print("Invalid directory")
    exit(2)
  pth = os.path.realpath(dname)
  files = glob.glob(pth + '/*.l[1-4]')
  for file in files:
    filePath = file
    try:
      execC0File(filePath, True, errCase)
    except: 
      testFail(file, 'Unknown test failure')


def testPass(fname, optMsg):
  print col.HAPPYPURPLE + 'PASS: ' + fname + " " + optMsg + col.ENDC

def testFail(fname, optMsg):
  print col.FAIL + 'FAILURE: ' + fname + " " + optMsg + col.ENDC

def main():
  try:
    myopts, args = getopt.getopt(sys.argv[1:], "f:d:qe")
  except getopt.GetoptError as e:
    print (str(e))
    print("Usage: %s -f c0file or -d directory. -q to silence, -e for error case" % sys.argv[0])
    sys.exit(2)

  c0file = ''
  testDir = ''
  quiet = False
  dirEx = False
  errCase = False

  for o, a in myopts:
    if o == '-f':
      c0file = a
    elif o == '-q':
      quiet = True
    elif o == '-d':
      dirEx = True
      testDir = a
    elif o == '-e':
      errCase = True
    else:
      print("Unsupported arg : %s" % o)
  if (len(myopts) <= 0):
    print("Usage: %s -f c0file" % sys.argv[0])
    sys.exit(2)
  if (dirEx):
    execC0Dir(testDir, quiet, errCase)
  else:
    execC0File(c0file, quiet, errCase)

if __name__ == "__main__":
  main()
