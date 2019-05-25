import numpy as np
import matplotlib.pyplot as plt
import utils
from iapws import IAPWS97
from scipy.interpolate import interp1d
from iapws import IAPWS97

k = [0.4, 0.9659, 1.4674, 1.7507, 1.7729, 1.5234, 0.9626, 0.6583, 0.4465, 0.2889, 0.1633]
kr = 1.164
kv = [kr * i for i in k]
z = [0, 0.13, 0.26, 0.39, 0.52, 0.65, 0.78, 0.91, 1.04, 1.17, 1.30]

q = np.zeros(len(z))
t_water = np.zeros(len(z))
t_sh_outer = np.zeros(len(z))
t_sh_inner = np.zeros(len(z))
t_fuel35 = np.zeros(len(z))
t_fuel14 = np.zeros(len(z))
t_s = [329.04 for i in range(len(t_water))]

def plot_spline(x, y, xlabel, ylabel):
    f = interp1d(x, y, kind='cubic')
    x = np.linspace(x[0], x[len(x) - 1], num=40, endpoint=True)
    plt.plot(x, f(x))
    plt.grid(True)
    plt.xlabel(xlabel, fontsize=14, fontweight='bold')
    plt.ylabel(ylabel, fontsize=14, fontweight='bold')
    plt.show()

def q_distribution_over_the_height_calc():
    global q

    for i in range(len(q)):
        q[i] = float(data["qSr"]) * kv[i]
    plot_spline(z, q, r'z, $м$', r'q, $МВт/м^2$')

def temp_distribution_of_water_calc():
    global t_water

    for i in range(len(z)):
        t_water[i] = data["tVhR"] + data["kG"] * data["Pt"] / (data["kQ"] * data["kR"] * data["Gtn"] * data["Cp"]) * np.trapz(q[0: (i + 1)], z[0: (i + 1)]) * 1e6


def temp_distribution_of_outer_shell():
    global t_sh_outer

    for i in range(len(z)):
        water = IAPWS97(T = t_water[i] + 273.0, P = data["P1c"])
        nu = water.Liquid.nu
        mu = water.Liquid.mu
        Pr = water.Liquid.Prandt
        lambda_water = water.Liquid.k

        Re = Wtvsm * data["d"] / nu
        Nu = 0.023 * Re ** 0.8 * Pr ** 0.4
        a = Nu * lambda_water / data["d"]
        Ra = 1 / a
        t_sh_outer[i] = t_water[i] + q[i] * Ra * 1e6 - 10
        print(Ra)



def temp_distribution_of_inner_shell():
    global t_sh_inner
    RShell = data["deltaShell"] / data["lambdaShell"]
    t_sh_inner = t_sh_outer + q * RShell * 1e6



def temp_distribution_of_fuel():
    global t_fuel35, t_fuel14

    RFuel35 = data["d"] / (4 * data["lambdaFuel35"])
    RFuel14 = data["d"] / (4 * data["lambdaFuel14"])
    t_fuel35 = t_sh_inner + q * RFuel35 * 1e6
    t_fuel14 = t_sh_inner + q * RFuel14 * 1e6


def plot_graphs():
    x = np.linspace(z[0], z[len(z) - 1], num=40, endpoint=True)
    print(x)

    f1 = interp1d(z, t_water, kind='cubic')
    f2 = interp1d(z, t_sh_outer, kind='cubic')
    f3 = interp1d(z, t_sh_inner, kind='cubic')
    f4 = interp1d(z, t_fuel35, kind='cubic')
    f5 = interp1d(z, t_fuel14, kind='cubic')

    plt.plot(x, f1(x), label=r'$t_в$')
    plt.plot(z, t_s, '--', label=r'$t_s$')
    plt.plot(x, f2(x), '-.', label=r'$t_{об.н}$')
    plt.plot(x, f3(x), ':', label=r'$t_{об.вн}$')
    plt.plot(x, f4(x), '.-', label=r'$t_{топ35}$')
    plt.plot(x, f5(x), '-o', label=r'$t_{топ14}$')

    plt.grid(True)
    plt.legend(fontsize=14)
    plt.xlabel(r'z, $м$', fontsize=14, fontweight='bold')
    plt.ylabel(r't, $^oC$', fontsize=14, fontweight='bold')
    plt.show()


data = {}
utils.fill_dict_from_file(data, "..\\Distributions\\neutron_distr.txt")
kV = data["kR"] * data["kZ"]

#Constants calculation
Gtvsm = data["Gtn"] * kr
Wtvsm = data["w"] * kr
water=IAPWS97(P = data["P1c"], x = 0)


if __name__ == "__main__":
    q_distribution_over_the_height_calc()
    temp_distribution_of_water_calc()
    temp_distribution_of_outer_shell()
    temp_distribution_of_inner_shell()
    temp_distribution_of_fuel()
    plot_graphs()
