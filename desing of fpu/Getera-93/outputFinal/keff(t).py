from scipy.interpolate import interp1d
import numpy as np
import matplotlib.pyplot as plt

# Constants
kinf = 1.15010
keff = 1.153780
P = 1
kinf_separator = "  ***k(inf)="
burn_separator = " *** average burn up ="
pu39_separator = "16 pu39"
pu41_separator = "18 pu41"
gd57_separator = "13 gd57"
time_separator = "dtim="


def get_data(file_name):
	data = []
	with open(file_name, 'r') as fd:
		for line in fd:
			data.append(line)
	return data


def get_kinf(data):
	kinf = []
	counter = 0
	for line in data:
		if kinf_separator in line:
			counter += 1
			if counter == 1 or counter % 2 == 0:
				k = float(line.split(kinf_separator)[1].strip().split()[0].strip())
				if len(kinf) > 0 and k != kinf[-1] or len(kinf) == 0:
					kinf.append(k)
	return kinf


def get_pu39(data):
	pu39 = []
	counter = 0
	for line in data:
		if pu39_separator in line:
			counter += 1
			if 1 < counter < 3 or (counter % 2) == 0:
				continue
			pu39_conc = float(line.split(pu39_separator)[1].strip().split()[0].strip())
			pu39.append(pu39_conc)
	return pu39

def get_pu41(data):
	pu41 = []
	counter = 0
	for line in data:
		if pu41_separator in line:
			counter += 1
			if counter % 2 == 0:
				continue
			pu41_conc = float(line.split(pu41_separator)[1].strip().split()[0].strip())
			pu41.append(pu41_conc)
	return pu41


def get_gd57_svp1(data):
	gd57 = []
	counter = 3
	for line in data:
		if gd57_separator in line:
			counter += 1
			if counter != 4:
				continue
			gd57_conc = float(line.split(gd57_separator)[1].strip().split()[0].strip())
			gd57.append(gd57_conc)
			counter = 0
	return gd57


def get_gd57_svp2(data):
	gd57 = []
	counter = 2
	for line in data:
		if gd57_separator in line:
			counter += 1
			if counter != 4:
				continue
			gd57_conc = float(line.split(gd57_separator)[1].strip().split()[0].strip())
			gd57.append(gd57_conc)
			counter = 0
	return gd57


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
	x = np.linspace(x[0], x[len(x) - 1],  num=40, endpoint=True)
	plt.plot(x, f(x))
	# plt.plot(x, [1 for _ in x], '--', color='red')
	plt.grid(True)
	plt.xlabel(xlabel, fontsize=12)
	plt.ylabel(ylabel, fontsize=12)
	plt.show()


if __name__ == "__main__":
	data = get_data("kp1_cen_NotPel.out")

	kinf = get_kinf(data)
	keff = list(map(lambda x: x * P, kinf))
	burn = get_burn(data)
	time = get_time(data)

	pu39 = get_pu39(data)
	pu41 = get_pu41(data)
	gd57_svp1 = get_gd57_svp1(data)
	gd57_svp2 = get_gd57_svp2(data)


	plot_spline(time, burn, "t, сутки", "PT, $МВт⋅сут/кг$")
	plot_spline(time, keff, "t, сутки", r'$k_{eff}$')
	# plot_spline(time, gd57_svp1, "t, сутки", r"$\rho$ $Gd^{57}$, $1/см^3 \times 10^{24}$")
	# plot_spline(time, gd57_svp2, "t, сутки", r"$\rho$ $Gd^{57}$, $1/см^3 \times 10^{24}$")
	# plot_spline(time, pu39, "t, сутки", "ro pu39")
	# plot_spline(time, pu41, "t, сутки", "ro pu41")