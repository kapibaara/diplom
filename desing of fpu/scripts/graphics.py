from scipy.interpolate import interp1d
import numpy as np
import matplotlib.pyplot as plt
import os

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


def read_file(file_name):
    lines = []
    with open(file_name, 'r') as f:
        lines = f.readlines()
    return lines


def get_Kinf(lines):
    Kinf = []
    for line in lines:
        if kinf_separator in line:
            splited = line.split(kinf_separator)
            if splited[1].startswith(" "):
                Kinf.append(splited[1].split(" ")[1])
            else:
                Kinf.append(splited[1].split(" ")[0])
    return Kinf


def get_burn_up(data):
    burn = []
    burn.append(0)
    for line in data:
        if burn_separator in line:
            b = float(line.split(burn_separator)[1].strip().split()[0].strip())
            burn.append(b)
    return burn


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
    gd57.pop(0)
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
    gd57.pop(0)
    return gd57


def get_burn_time(lines):
    time = []
    time.append(0)
    for line in lines:
        if time_separator in line:
            splited = line.split(time_separator)
            t = splited[1].split("\n")[0].split(",")[0]
            time.append(int(t) + time[-1])
    return time


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


def plot_spline(x, y, xlabel, ylabel):
    f = interp1d(x, y, kind="linear")
    x = np.linspace(x[0], x[len(x) - 1], num=40, endpoint=True)
    plt.plot(x, f(x))
    plt.grid(True)
    plt.xlabel(xlabel, fontsize=12)
    plt.ylabel(ylabel, fontsize=12)
    plt.show()


if __name__ == "__main__":
    cur_path = os.path.split(__file__)[0]
    getera_out_path = os.path.join(cur_path, r"../Getera-93/prakticeOutput/")
    getera_output_files = os.listdir(getera_out_path)
    getera_output_file = os.path.join(cur_path, getera_out_path, getera_output_files[5])

    data = read_file(getera_output_file)

    time = get_burn_time(data)
    kinf = get_Kinf(data)
    keff = list(map(lambda x: float(x) * 0.9459, kinf))
    print(keff)
    gd57_svp1 = get_gd57_svp1(data)
    gd57_svp2 = get_gd57_svp2(data)
    burn = get_burn_up(data)
    pu39 = get_pu39(data)
    pu41 = get_pu41(data)

    # plot_spline(time, pu39, "t, сутки", "ro pu39")
    # plot_spline(time, pu41, "t, сутки", "ro pu41")
    # plot_spline(time, burn, "t, сутки", "PT, $МВт⋅сут/кг$")
    plot_spline(time, kinf, "t, сутки", "keff")
    # plot_spline(time, gd57_svp1, "t, сутки", r"$\rho$ $Gd^{57}$, $1/см^3 \times 10^{24}$ svp1")
    # plot_spline(time, gd57_svp2, "t, сутки", r"$\rho$ $Gd^{57}$, $1/см^3 \times 10^{24}$ svp2")
