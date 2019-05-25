import sys
import os

sys.path.append("tools\\")

from run import*
from reader import*
from plotter import*
from ffusion import*

class TModel():
    def __init__(self):
        pass


unkdir = "E:\\Prog\\gitproj\\UNK_BN800\\IAEA_FP0\\"

arg = ["..\\Input\\br1200.dat"]
for i in range(1,8):
    var = unkdir + str(i) + "\\XS_SKETCH.out.222"
    arg.append(var)

target = "..\\Input\\input.dat"

#ffusion(arg,target,'w')

#run("..\\SKETCH.EXE")
#run("..\\POSTPROC.EXE")

rd = TReader()

rd.read([["sketch","..\\output\\power.lst"]])

field = np.array(rd.base[0]).transpose()
Plot2D(field[1],"square540.net")

print(parf(rd.base[0]))










   