Na = 0.602

class Material(object):
    def __init__(self, x, M, **kwargs):
        self.x = x
        self.M = M
        for key in kwargs.keys():
            self.__dict__[key] = kwargs[key]

    def append(self, **kwargs):
        for key in kwargs:
            self.__dict__[key] = kwargs[key]

class Fuel():
    def __init__(self, x):
        self.x = x
        self.density = 10.97
        self.U235 = Material(x=self.x, M=235)
        self.U238 = Material(x=(1-self.x), M=238)
        self.O = Material(x=1.0, M=16)


    def calc_nuclear_density(self):
        self.M = (self.U238.M * self.U235.M) / (self.U235.x * self.U238.M + self.U238.x * self.U235.M) + self.O.M * 2
        self.N = self.density * Na / 267
        self.j = (self.N*self.M)/Na
        self.U235.append(N = self.U235.x*self.N)
        self.U238.append(N = self.U238.x*self.N)
        self.O.append(N = 2*self.N)

class B4C(object):
    def __init__(self, x):
        self.x = x
        self.density = 2.5
        self.B10 = Material(x=self.x, M=10)
        self.B11 = Material(x=(1-self.x), M=11)
        self.C = Material(x=1.0, M=12)
        self.M = ((self.B10.M*self.B11.M)/(self.B10.x*self.B11.M + self.B11.x*self.B10.M))*4 + self.C.M

    def calc_nuclear_density(self):
        self.N = self.density * Na / self.M
        self.B10.append(N=self.B10.x * self.N*4)
        self.B11.append(N=self.B11.x * self.N*4)
        self.C.append(N= self.N)

class Gd2o3(object):
    def __init__(self):
        self.x55 = 0.1480
        self.x57 = 0.1565
        self.density = 4.0
        self.Gd55 = Material(x=self.x55, M=155)
        self.Gd57 = Material(x=self.x57, M=157)

        self.O = Material(x=1.0, M=16)
        self.Mgd =(1/ ((self.Gd55.x/self.Gd55.M)+(self.Gd57.x/self.Gd57.M)))
        self.M = 2*self.Mgd + 3*self.O.M

    def calc_nuclear_density(self):
        self.N = self.density * Na / self.M
        self.j = (self.N * self.M) / Na
        self.Gd55.append(N=self.Gd55.x * self.N * 2)
        self.Gd57.append(N=self.Gd57.x * self.N * 2)
        self.O.append(N= self.N*3)

class H2O(object):
    def __init__(self):
        self.density = 0.698
        self.H = Material(x=1.0, M=1)
        self.O = Material(x=1.0, M=16)
        self.M = 2*self.H.M + self.O.M

    def calc_nuclear_density(self):
        self.N = (self.density * Na / self.M)
        self.H.append(N=2*self.N)
        self.O.append(N= self.N)

class Shall(object):
    def __init__(self, x):
        self.x = x
        self.d_Zr = 6.52
        self.Zr = Material(x=x, M=91)
        self.M = 1/(self.Zr.x/self.Zr.M)
        self.density = 1/(self.Zr.x/self.d_Zr)
    def calc_nuclear_density(self):
        self.N = (self.density * Na / self.M)
        self.Zr.append(N=self.N*self.x)

class Shall_B4C(object):
    def __init__(self):
        self.x_Cr = 0.42
        self.x_Ni = 0.56

        self.d_Cr = 7.19
        self.d_Ni = 8.91
        self.Cr = Material(x=self.x_Cr, M=52)
        self.Ni = Material(x=self.x_Ni, M=58.7)
        self.M = 1 / ((self.Cr.x/self.Cr.M)+(self.Ni.x/self.Ni.M))
        self.density = 1 / ((self.Ni.x/self.d_Cr)+(self.Ni.x/self.d_Ni))

    def calc_nuclear_density(self):
        self.N = self.density * Na / self.M
        self.Cr.append(N=self.N * self.Cr.x)
        self.Ni.append(N=self.N * self.Ni.x)
