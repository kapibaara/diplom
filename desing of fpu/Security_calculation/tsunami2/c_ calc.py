import matplotlib.pyplot as plt
from scipy.interpolate import interp1d
import numpy as np

def plot_spline(x, y, xlabel, ylabel):
	f = interp1d(x, y, kind='cubic')
	x = np.linspace(x[0], x[len(x) - 1], num=40, endpoint=True)
	plt.plot(x, f(x))
	plt.grid(True)
	plt.xlabel(xlabel, fontsize=12)
	plt.ylabel(ylabel, fontsize=12)
	plt.show()


temp_f = [300, 400, 500, 600, 700, 800, 900, 1000]
c_f = [236.98, 265.21, 282.53, 294.03, 301.81, 307.02, 310.50, 312.86]

# plot_spline(temp_f, c_f, r'$T_{топ},$ $^\circ$C', r'$C_p, Дж/(кг*К)$')