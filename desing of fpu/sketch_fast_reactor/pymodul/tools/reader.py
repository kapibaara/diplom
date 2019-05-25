import re            #for regular expresisons
import subprocess    #needed for pipes
import os
import sys
from math import pi, pow, fabs
import scipy as sp
import numpy as np
import time as tm
import shutil

class TReader():
    def __init__(self):
        self.base = [] ## база данных с полями
    def read(self,files):
        # чтение данных из файлов
        for g in files:
            if g[0] == "sketch":
                self.readpowersketch(g[1])
                pass
            if g[0] == "hortica":
                self.readpowerhortica(g[1])
                pass
            if g[0] == "hortica2":
                self.read_hortica_field(g[1])
                pass
        pass
    def readpowersketch(self,filename):
        """Чтение данных в формате SKETCH"""
        if os.path.exists(filename):
            f = open(filename,"r")
        else:
            print "File " + filename + "not found!"
        for line in f:
            if bool(re.search("CHAN ",line)):
                field = []
                f.next()
                for line in f:
                    if bool(re.search("!-----",line)):
                        break
                    l1 = line.split()[1:]
                    field.append(map(float,l1))
        self.base.append(np.array(field))
        f.close()
        pass
    def readpowerhortica(self,filename):
        """Чтение данных в формате HORTICA"""
        power1 = []
        power2 = []
        f = open(filename,"r")
        for line in f:
            l1 = np.array(line.split()[1:-1])
            val = []
            for s in l1:
                if s != '&':
                    val.append(float(s))
            power1.append(np.array([val]).reshape([len(val)/2,2]).transpose()[0])
            power2.append(np.array([val]).reshape([len(val)/2,2]).transpose()[1])
        field1 = np.array(power1)
        field2 = np.array(power2)
        self.base.append(field1)
        self.base.append(field2)
        f.close()
        pass
    def read_hortica_field(self,filename):
        """Чтение данных"""
        f = open(filename,"r")
        val = []
        for line in f:
            val.append([float(s) for s in line.split()])
        self.base.append(np.array(val))


def parf(data):
    """Parameters of the field"""
    val_0 = sum(np.array(data).flatten())  # интегральное значение поля
    val_1 = max(np.array(data).flatten())  # максимальное значение поля
    val_2 = np.mean(np.array(data).flatten()) # среднее значение поля энерговыделения
    val_3 = val_1/val_2 # коэффициент неравномерности поля энерговыделения
    val_4 = np.argmax(np.sum(data,1)) # номер кассеты с максимальным энерговыделением
    return val_0, val_1,val_2,val_3,val_4
    pass

