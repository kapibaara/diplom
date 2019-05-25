import sys
import os

sys.path.append("..\\tools\\")

import ffusion
import run


class TModel():
    def __init__(self):
        pass


unkdir = "E:\\Prog\\gitproj\\UNK_BN800\\IAEA_ISOT\\"

arg = ["..\\Input\\br1200.dat"]
for i in range(1,8):
    var = unkdir + str(i) + "\\XS_SKETCH.out.222"
    arg.append(var)

target = "..\\Input\\input.dat"

ffusion(arg,target,'w')

run("..\\SKETCH.EXE")

   