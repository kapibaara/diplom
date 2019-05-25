import os
import sys
import copy
from math import pi, pow, fabs
import scipy as sp
import numpy as np
import time as tm

setpath = "d:\\drobyshev\\Prog\\mylibs"
MainSetpath = "d:\\drobyshev\\Prog\\bn"

os.chdir(setpath)
from plotter import*

MAIN_DIR = MainSetpath

os.chdir(MAIN_DIR+"\\pymodul")
from stend import*


st = TStend(MAIN_DIR)


st.calc(mode = "ss") # страционарный расчет
#print st.sk.out # вывод данных с консоли

st.calc(1, mode = "kin") # нестационарный расчет
#print st.sk.out # вывод данных с консоли

# получаем нейтронные поля в выбранные моменты времени
fluxt = []
for t in [0.0, 1.0]:
    data = st.getf(t)
    flux = np.array([d.transpose() for d in data])
    fluxt.append(flux)

# Отображение данных
os.chdir("d:\\drobyshev\\Prog\\mylibs\\net2d")
fname = "square18_18.net"
g0 = fluxt[0][0][11].flatten()
g1 = fluxt[-1][0][11].flatten()

Plot2D(g0, fname, ind = 4, rad = 0.5*pow(2,0.5), fi = np.pi/4)
Plot2D(g1, fname, ind = 4, rad = 0.5*pow(2,0.5), fi = np.pi/4)
Plot2D(g1-g0, fname, ind = 4, rad = 0.5*pow(2,0.5), fi = np.pi/4)



