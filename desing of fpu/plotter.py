import matplotlib.pyplot as plt
from scipy.interpolate import interp1d
import numpy as np



def plot_spline(x, y, xlabel, ylabel):
	f = interp1d(x, y, kind='cubic')
	x = np.linspace(x[0], x[len(x) - 1], num=40, endpoint=True)
	plt.plot(x, f(x))
	plt.grid(True)
	plt.xlabel(xlabel, fontsize=14)
	plt.ylabel(ylabel, fontsize=14)
	plt.show()

def plot_spline_2(x, y1, y2, xlabel, ylabel):
	f1 = interp1d(x, y1, kind='cubic')
	f2 = interp1d(x, y2, kind='cubic')
	x = np.linspace(x[0], x[len(x) - 1], num=40, endpoint=True)
	plt.plot(x, f1(x))
	plt.plot(x, f2(x), '--', color='red')
	plt.grid(True)
	plt.xlabel(xlabel, fontsize=14)
	plt.ylabel(ylabel, fontsize=14)
	plt.show()

qv = 101.5

kzs1 = [0.5, 1.5098, 2.1084, 2.0954, 1.3536, 0.9706, 0.7058, 0.5119, 0.3627, 0.2423, 0.1395]
kr_max1 = 1.104
kvs1 = [kr_max1 * kz for kz in kzs1]

x = [0, 0.13, 0.26, 0.39, 0.52, 0.65, 0.78, 0.91, 1.04, 1.17, 1.30]
y1 = [qv * kv for kv in kvs1]

kzs2 = [0.55, 1.0511, 1.4512, 1.6874, 1.6991, 1.4749, 0.9733, 0.6878, 0.4788, 0.3159, 0.1806]
kr_max = 1.190
kvs2 = [kr_max1 * kz for kz in kzs2]

y2 = [qv * kv for kv in kvs2]

plot_spline_2(x, y2, y1, r'z,$м$', r'$q_v, Вт/см^3$')

#
# plot_spline(x, y, r'z,$м$', r'$q_v, Вт/см^3$')

# keff = [1.090632, 1.090016, 1.088548, 1.085704, 1.080526, 1.071319, 1.055784, 1.034941, 1.013494, 0.999058, 0.994]
# x = [0, 0.13, 0.26, 0.39, 0.52, 0.65, 0.78, 0.91, 1.04, 1.17, 1.30]
# plot_spline(x, keff, r'z,$м$', r'$k_{eff}$')

