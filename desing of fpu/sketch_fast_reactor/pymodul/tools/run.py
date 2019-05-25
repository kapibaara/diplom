import subprocess
import sys

def run(commandList):
    print(commandList+"\n \n")
    subprocess.call(commandList)


def trysubprocess(commandList):
    retcode = 0
    try:
        p = subprocess.Popen(commandList, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        retcode = p.returncode
        out, err = p.communicate()
    except OSError, e:
            print >>sys.stderr, "Execution failed:", e
    return retcode

