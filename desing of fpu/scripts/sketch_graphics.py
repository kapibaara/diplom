from scripts.getera_graphics import read_file, plot_spline
from scipy.interpolate import interp1d
import numpy as np
import matplotlib.pyplot as plt
import os

cur_path = os.path.split(__file__)[0]
burn_file = os.path.join(cur_path, r"./results/burn.txt")
time_file = os.path.join(cur_path, r"./results/time.txt")
keff_file = os.path.join(cur_path, r"./results/keff.txt")
cen_pel_ratio_file = os.path.join(cur_path, r"./results/cen_pel_ratio.txt")
per_pel_ratio_file = os.path.join(cur_path, r"./results/per_pel_ratio.txt")

def get_data_list(file):
    data = read_file(file)[0].split(" ")
    return list(map(lambda x: float(x), data))

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

burn = get_data_list(burn_file)
time = get_data_list(time_file)
keff = get_data_list(keff_file)

cen_pel_ratio = list(map(lambda x: float(x * 10), get_data_list(cen_pel_ratio_file)))
per_pel_ratio = list(map(lambda x: float(x * 10), get_data_list(per_pel_ratio_file)))

plot_spline(time, keff, "t, сутки", "Keff")
plot_spline_2(time, cen_pel_ratio, per_pel_ratio, "t, сутки", "погружение пэлов, %")


