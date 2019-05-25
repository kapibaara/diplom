from scipy.interpolate import interp1d
import sys
import numpy as np
import matplotlib.pyplot as plt

# Constants
kinf0 = 1.11781
keff0 = 1.115111
P = keff0 / kinf0
kinf_separator = "  ***k(inf)="
burn_separator = " *** average burn up ="
time_separator = "dtim="


def get_data(file_name):
	data = []
	with open(file_name, 'r') as fd:
		for line in fd:
			data.append(line)
	return data


def get_kinf(data):
	kinf = []
	for line in data:
		if kinf_separator in line:
			k = float(line.split(kinf_separator)[1].strip().split()[0].strip())
			kinf.append(k)
	return kinf


def get_burn(data):
	burn = []
	burn.append(0)
	for line in data:
		if burn_separator in line:
			b = float(line.split(burn_separator)[1].strip().split()[0].strip())
			burn.append(b)
	return burn


def get_time(data):
	time = []
	time.append(0)
	for line in data:
		if time_separator in line:
			t = float(line.split(time_separator)[1].strip().split(",")[0].strip())
			time.append(t + time[-1])
	return time


def plot_spline(x, y, xlabel, ylabel):
	f = interp1d(x, y, kind="linear")
	x = np.linspace(x[0], x[len(x) - 1], num=40, endpoint=True)
	plt.plot(x, f(x))
	plt.grid(True)
	plt.xlabel(xlabel, fontsize=14, fontweight='bold')
	plt.ylabel(ylabel, fontsize=14, fontweight='bold')
	plt.show()


if __name__ == "__main__":
	data = get_data(sys.argv[1])

	kinf = get_kinf(data)
	keff = list(map(lambda x: x * P, kinf))
	burn = get_burn(data)
	time = get_time(data)

	plot_spline(time, keff, "t, сутки", r'$k_{eff}$')
	plot_spline(time, burn, "t, сутки", "PT, $МВт⋅сут/кг$")
