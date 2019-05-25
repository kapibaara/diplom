import numpy as np
import matplotlib.pyplot as plt
import utils
from iapws import IAPWS97

# x1  calc: kz = 1 / (2 / pi * 1 / x * sin(pi * x / 2))
# x2 calc: 2 * J1(x) * kr = x

x1 = 0.838
x2 = 1.625

deltaZ = (1.3 - x1 * 1.3) / (2 * x1)
deltaR = (1.36121 - 0.566 * x2) / (x2)
kV = 0.


step = 0.01
z = np.arange(0, 1.3 + step, step)
q = np.zeros(z.size)
t_water = np.zeros(z.size)
t_sh_outer = np.zeros(z.size)
t_sh_inner = np.zeros(z.size)
t_fuel35 = np.zeros(z.size)
t_fuel14 = np.zeros(z.size)


def q_distribution_over_the_height_calc(isPlot):
    global q, kV

    kV = data["kR"] * data["kZ"]
    qMax = kV * data["qSr"]
    q = qMax * np.cos(np.pi / 2 * (data["Haz"] - 2 * z) / (data["Haz"] + 2 * deltaZ)) * 1e6

    if isPlot:
        plt.plot(z, q / 1e6)
        plt.grid(True)
        plt.xlabel(r'z, $м$', fontsize=14, fontweight='bold')
        plt.ylabel(r'q, $МВт/м^2$', fontsize=14, fontweight='bold')
        plt.show()

def temp_distribution_of_water_calc(isPlot):
    global t_water

    qMax = kV * data["qSr"] *1e6

    f0 = -1 / np.pi * (data["Haz"] + 2 * deltaZ) * np.sin(np.pi / 2 * ((data["Haz"]) / (data["Haz"] + 2 * deltaZ)))
    f = -1 / np.pi * (data["Haz"] + 2 * deltaZ) * np.sin(np.pi / 2 * ((data["Haz"] - 2 * z) / (data["Haz"] + 2 * deltaZ))) - f0

    t_water = data["tVhR"] + (data["kG"] * data["Pt"] * qMax) / (data["kQ"] * data["kR"] * Gtvsm * data["Cp"]) * f

    if isPlot:
        plt.plot(z, t_water, label=r'$t_в$')
        plt.plot(z, ts, 'r--',  label=r'$t_s$')
        plt.grid(True)
        plt.xlabel(r'z, $м$', fontsize=14, fontweight='bold')
        plt.ylabel(r't, $^oC$', fontsize=14, fontweight='bold')
        plt.legend()
        plt.ylim(290, 340)
        plt.show()


def temp_distribution_of_outer_shell(isPlot):
    global t_sh_outer

    for i in range(z.size):
        water = IAPWS97(T = t_water[i] + 273.0, P = data["P1c"])
        nu = water.Liquid.nu
        mu = water.Liquid.mu
        Pr = water.Liquid.Prandt
        lambda_water = water.Liquid.k

        Re = Wtvsm * data["d"] / nu
        Nu = 0.023 * Re ** 0.8 * Pr ** 0.4
        a = Nu * lambda_water / data["d"]
        Ra = 1 / a
        t_sh_outer[i] = t_water[i] + q[i] * Ra

    if isPlot:
        plt.plot(z, t_sh_outer, label=r'$t_{об.н}$')
        plt.plot(z, ts, 'r--',  label=r'$t_s$')
        plt.grid(True)
        plt.xlabel(r'z, $м$', fontsize=14, fontweight='bold')
        plt.ylabel(r't, $^oC$', fontsize=14, fontweight='bold')
        plt.legend()
        plt.show()

def temp_distribution_of_inner_shell(isPlot):
    global t_sh_inner
    RShell = data["deltaShell"] / data["lambdaShell"]
    t_sh_inner = t_sh_outer + q * RShell

    if isPlot:
        plt.plot(z, t_sh_inner)
        plt.grid(True)
        plt.xlabel(r'z, $м$', fontsize=14, fontweight='bold')
        plt.ylabel(r't, $^oC$', fontsize=14, fontweight='bold')
        plt.show()


def temp_distribution_of_fuel(isPlot):
    global t_fuel35, t_fuel14
    RFuel35 = data["d"] / (4 * data["lambdaFuel35"])
    RFuel14 = data["d"] / (4 * data["lambdaFuel14"])
    t_fuel35 = t_sh_inner + q * RFuel35
    t_fuel14 = t_sh_inner + q * RFuel14

    if isPlot:
        plt.figure(1)
        plt.plot(z, t_fuel35)
        plt.grid(True)
        plt.xlabel(r'z, $м$', fontsize=14, fontweight='bold')
        plt.ylabel(r't, $^oC$', fontsize=14, fontweight='bold')
        plt.figure(2)
        plt.plot(z, t_fuel14)
        plt.grid(True)
        plt.xlabel(r'z, $м$', fontsize=14, fontweight='bold')
        plt.ylabel(r't, $^oC$', fontsize=14, fontweight='bold')
        plt.show()

def calculate_distributions(isPlot):
    # q_distribution_over_the_height_calc(isPlot)
    temp_distribution_of_water_calc(isPlot)
    temp_distribution_of_outer_shell(isPlot)
    temp_distribution_of_inner_shell(isPlot)
    temp_distribution_of_fuel(isPlot)


def main_characteristics_TVSM():

    qVMax = data["qV"] * kV
    qlAvgMax = data["qlAvg"] * data["kZ"]
    Qtvsm = data["Qtvs"] * data["kR"]
    qMax = max(q) * 1e-6
    qAvg = sum(q) / len(q) * 1e-6
    roW = data["roH2O"] * Wtvsm
    t_sh_outer_max = max(t_sh_outer)
    t_sh_inner_max = max(t_sh_inner)
    t_fuel35_max = max(t_fuel35)
    t_fuel14_max = max(t_fuel14)

    upper_ts = []
    for i in range(0, len(z)):
        if t_sh_outer[i] >= ts[0]:
            upper_ts.append(i)
            break
    for i in range(len(z) - 1, -1, -1):
        if t_sh_outer[i] >= ts[0]:
            upper_ts.append(i)
            break
    if len(upper_ts) != 0:
        LBoil = z[upper_ts[1]] - z[upper_ts[0]]
    else:
        LBoil = 0


    out.write(("qVMax: %.3f MVt/m^3\nqlAvgMax: %.3f Vt/cm\nQtvsm: %.3f MVt\nqMax: %.3f MVt/m^2\nqAvg: %.3f MVt/m^2\n" +\
        "Gtvsm: %.3f kg/s\nWtvsm: %.3f kg/s\nroWtvsm: %.3f kg/(m^2*s)\n" +\
        "tOutShMax: %.3f C\ntInShMax: %.3f C\n" +\
        "tFuel35Max:  %.3f C\ntFuel14Max:  %.3f C\nLBoil: %.3f m\n")
        % (qVMax, qlAvgMax, Qtvsm, qMax, qAvg, Gtvsm, Wtvsm, roW, t_sh_outer_max, t_sh_inner_max, t_fuel35_max, t_fuel14_max, LBoil))

data = {}
utils.fill_dict_from_file(data, "..\\Distributions\\distributionsInput.txt")

out = open("..\\Distributions\\distributionOut.txt", "w")

#Constants calculation
Gtvsm = data["Gtn"] * data["kR"]
Wtvsm = data["w"] * data["kR"]
water=IAPWS97(P = data["P1c"], x = 0)
ts = list(map(lambda x: water.T - 273, np.zeros(z.size)))

q_distribution_over_the_height_calc(True)
if __name__ == "__main__":
    calculate_distributions(True)
    main_characteristics_TVSM()

out.write("deltaZ: %.3f m\n" % deltaZ)
out.write("deltaR: %.3f m\n" % deltaR)
out.write("kV: %.3f\n" % kV)

out.close()
