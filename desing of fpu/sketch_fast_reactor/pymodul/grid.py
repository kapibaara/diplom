import os
import numpy as np
import scipy as sp

os.chdir("e:\\Prog\\mylibs")
from plotter import*
from reader import*

MAIN_DIR = "E:\\Prog\\stend\\sketch-4"
path = "Output\\PP_STEND.lst "
os.chdir(MAIN_DIR)
rd = TReader()
rd.read([["sketch",path]])

data = (rd.base[0]).transpose()[5].flatten()


# Отображение данных
os.chdir("e:\\prog\\mylibs\\net2d")
fname = "square18_18.net"
Plot2D(data, fname, ind = 4, rad = 0.5*pow(2,0.5), fi = np.pi/4)
