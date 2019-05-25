import matplotlib.pyplot as plt
from scipy.interpolate import interp1d
import os
import numpy as np

path_to_input = "..\\sketch_fast_reactor\\Input\\newsample.dat"
path_to_output = "..\\sketch_fast_reactor\\Output\\SKETCH.lst"
path_to_exec = "..\\sketch_fast_reactor\\SKETCH.exe"

centr_tvs = "## nb = 1"
peref_tvs = "## nb = 2"

center_pattern = " 5 %d*1 %d*2 5    ## nb = 1  1-ТВС центральной зоны   \n"
peref_pattern = " 5 %d*3 %d*4 5    ## nb = 2  2-ТВС переферийной зоны  \n"

def get_data(file_name):
    in_data = []

    with open(file_name, 'r') as fd:
        for line in fd:
            in_data.append(line)
    return in_data

def write_data(data):
    with open(path_to_input, "w") as fd:
        for line in data:
            fd.write(line)

def set_config(data, pel_in, pel_out):
    for i, line in enumerate(data):
        if centr_tvs in line:
            data[i] = center_pattern % (pel_out, pel_in)
        elif peref_tvs in line:
            data[i] = peref_pattern % (pel_out, pel_in)
    return data

def get_keff():
    data = get_data(path_to_output)
    flag = 1

    for line in data:
        if 'k_ef' in line:
            if flag:
                flag = 0
                continue
            keff = float(line.split(':  ')[1].strip())
            return keff



def plot_spline(x, y, xlabel, ylabel):
    f = interp1d(x, y, kind='cubic')
    x = np.linspace(x[0], x[len(x) - 1], num=40, endpoint=True)
    plt.plot(x, f(x))
    plt.plot(x, [1 for _ in x], '--', color='red')
    plt.grid(True)
    plt.xlabel(xlabel, fontsize=14)
    plt.ylabel(ylabel, fontsize=14)
    plt.show()


count_in = 0
count_out = 10

keff = []
z = [0, 0.13, 0.26, 0.39, 0.52, 0.65, 0.78, 0.91, 1.04, 1.17, 1.30]

while(count_in <= 10):
    in_data = get_data(path_to_input)
    in_data = set_config(in_data, count_in, count_out)
    write_data(in_data)
    os.chdir("..\\sketch_fast_reactor")
    os.system(path_to_exec)
    os.chdir("..\\dipping_of_rods")
    keff.append(get_keff())
    count_in += 1
    count_out -= 1

keff[-1] = 0.851373

plt.plot()
plot_spline(z, keff, r'z,$м$', r'$k_{eff}$')
plt.grid(True)
# plot_spline(z, kinf, r'z,$м$', r'$k_{inf}$')
