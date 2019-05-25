from math import erf
import numpy as np
import scipy as sp
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import matplotlib.mlab as mlab
import matplotlib.cbook as cbook
from matplotlib.collections import PatchCollection
from matplotlib.patches import Circle, Wedge, Polygon
import matplotlib.path as mpath
import matplotlib.patches as mpatches
import matplotlib.lines as mlines


#, '.', ',', 'o', 'v', '^', '<', '>', '2', '3', '4', 's', 'p', '*', 'h', 'H', '+', 'x', 'D', 'd', '|', '_']

def ListPlot(data, X = [], AxisLabel = {"x": "x","y": "y"}, figname = "",\
                        LineStyle=[], Color = [], AxesRange = "All", inverse = False):
    """
    Function for drowing one-dimention lists
    "data" - set of points
    """
    if bool(Color):
        color = Color
    else:
        color = ["b","g","m","r","c","m","y","k","w"]
    if bool(LineStyle):
        linestyles = LineStyle
    else:
        linestyles = ['-', '-','-','--', '-.', ':']
    plt.xlabel(AxisLabel["x"])
    plt.ylabel(AxisLabel["y"])
    plt.subplot(111)
    if AxesRange == "All":
        #plt.axis("tight")
        pass
    for i in range(len(data)):
        if len(X)>0:
            if len(X) == len(data[i]):
                if inverse:
                    line = plt.plot(data[i], X, color[np.mod(i,len(color))] + linestyles[np.mod(i,len(linestyles))])
                else:
                    line = plt.plot(X, data[i], color[np.mod(i,len(color))] + linestyles[np.mod(i,len(linestyles))])
                #plt.setp(line, color = color[np.mod(i,len(color))], linestyle = linestyles[np.mod(i,len(linestyles))], linewidth = 2)
                plt.setp(line, linewidth = 2)
                pass
            else:
                print "NOTTICE: length of X not equal data " + str(i)
        else:
            line = plt.plot(data[i], color[np.mod(i,len(color))] + linestyles[np.mod(i,len(linestyles))], linewidth = 2)
            #plt.setp(line, color = color[np.mod(i,len(color))], linestyle = linestyles[np.mod(i,len(linestyles))], linewidth = 2)
            plt.setp(line, linewidth = 2)
            pass
    plt.grid()
    if bool(figname):
        plt.savefig(figname) # save the picture
        plt.clf()
    else:
        plt.show() # show the picture


def Plot(f,x,npoint = 10):
    """
    Drowing one-dimention functions
    f - set of functions
    x - diaposone of x argument
    npoint - number of points
    """
    plt.subplot(111)
    dx = abs(x[1] - x[0])/float(npoint)
    net = dx*np.array(range(10))
    plt.plot(map(f,net),'-r')
    plt.show()


def Histogram(data):
    n, bins, patches = plt.hist(data, 20, normed=1, facecolor='blue', alpha=0.75)
    mu = np.mean(data)
    sigma = np.sqrt(np.cov(data))
    y = mlab.normpdf(bins, mu, sigma)
    l = plt.plot(bins, y, 'r', linewidth=1)
    plt.xlabel('X')
    plt.ylabel('Probability')
    plt.title(r"""mu = %f, sigma = %f""" % (mu, sigma))
    plt.axis([mu - 4*sigma, mu + 4*sigma, 0, 2.0*max(y)])
    plt.grid(True)

    plt.show()

def Grid(x1 = 0.0, x2 = 1.0, dx = 0.1):
    n = int((x2-x1)/dx)
    grid = np.array([x1 + i*(x2-x1)/(n-1) for i in range(n)])
    return grid



class TCell2D():
    def __init__(self,rad = 1.0, ind = 4, fi = np.pi/4):
        self.rad = rad
        self.ind = ind # number of corners
        self.fi = fi
        pass
    def getCOO(self):
        dfi = 2*np.pi/self.ind
        val = [[np.sin(dfi*i+self.fi),np.cos(dfi*i+self.fi)] for i in range(self.ind)]
        return self.rad*np.array(val)


class TGrid2D():
    def __init__(self,rad = 0.5, ind = 4, fi = np.pi/4, hint = False):
        self.hc = TCell2D(rad, ind, fi)
        self.data = []
        self.coo = []
        self.xylim = np.array([[-1,1],[-1,1]])
        self.hint = hint
        pass
    def show(self,figname=""):
        fig = plt.figure()
        ax = fig.add_subplot(111)
        hexcell = self.hc.getCOO()
        patches = []
        for n in range(len(self.coo)):
            xy = self.coo[n]
            polygon = Polygon(hexcell + np.array(xy), True, label = "dsjf")
            patches.append(polygon)
        colors = self.getcolor()
        p = PatchCollection(patches, cmap=matplotlib.cm.bwr, alpha=0.4)
        maxvalue = max(abs(np.array(self.data.flatten())))
        p.set_array(np.array(self.data))
        p.set_clim([-maxvalue,maxvalue])
        ax.add_collection(p)
        ax.set_xlim(self.xylim[0])
        ax.set_ylim(self.xylim[1])
        if self.hint:
            for n in range(len(self.coo)):
                xpos = self.coo[n][0]-0.5*self.hc.rad
                ypos = self.coo[n][1]-0.5*self.hc.rad
                ax.text(xpos,ypos,str(n+1) + "\n" + "%.0f"%self.data[n])
        plt.colorbar(p)
        if bool(figname):
            plt.savefig(figname) # save the picture
            plt.clf()
        else:
            plt.show() # show the picture
        pass
    def getcolor(self):
        if len(self.data) == 0:
            val = 100*np.random.rand(len(self.coo))
        else:
            maxv, minv = max(self.data.flatten()), min(self.data.flatten())
            val = (np.array(self.data) - (minv+maxv)*0.5)/max(abs(minv),abs(maxv))
        return val


def Plot2D(data, fnet, rad = 1.0, ind = 4, fi = np.pi/4, hint = False, figname = ""):
    f = open(fnet,"r")
    coo = []
    for line in f:
        coo.append([float(line.split()[1]),float(line.split()[2])])
    f.close()
    hg = TGrid2D(rad, ind, fi, hint)
    hg.xylim = np.array([[min(np.array(coo).transpose()[0])-rad, max(np.array(coo).transpose()[0])+rad],\
                         [min(np.array(coo).transpose()[1])-rad, max(np.array(coo).transpose()[1])+rad]])
    hg.data = data
    hg.coo = coo
    hg.show(figname = figname)
    pass
