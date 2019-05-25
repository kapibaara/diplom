import utils
import numpy as np
import sys
from scipy.interpolate import interp1d
sys.path.insert(0, 'C:\\Users\\Pavel\\Study\\-Designing-of-the-PEB\\Heat calculation\\Distributions')
from distributions import q
import matplotlib.pyplot as plt
from scipy.interpolate import spline
from iapws import IAPWS97


K = 0
q_cr = []

def calc_K():
    global K

    K1 = ((data["dT"]) / 9.36) ** (-1 / 3)

    K2 = 0.2 + 0.57 * data["x"]

    K3 = 1 + 0.6 * np.exp(-0.01 * data["Haz"] / data["dT"])

    K4 = 1.156

    K = K1 * K2 * K3 * K4


    out = open("heat_crisisOutput.txt", "w")
    out.write("K1: %.3f\nK2: %.3f\nK3: %.3f\nK4: %.3f\nK: %.3f" % (K1, K2, K3, K4, K))
    out.close()

def calc_q_critical():
    global q_cr

    q_cr = []
    ro_w = [1000, 2000, 3000, 4000]
    p = [12, 14, 16]
    roW = data["roWtvsm"]

    x11 = [3.24, 4.20, 4.91, 5.22]
    f11 = interp1d(ro_w, x11)
    x21 = [3.08, 3.83, 4.25, 4.36]
    f21 = interp1d(ro_w, x21)
    x31 = [2.67, 3.22, 3.54, 3.56]
    f31 = interp1d(ro_w, x31)
    f1 = interp1d(p, [f11(roW), f21(roW), f31(roW)])
    q_cr.append(f1(12.7))

    x12 = [2.74, 3.22, 3.52, 3.64]
    f12 = interp1d(ro_w, x12)
    x22 = [2.54, 2.90, 3.17, 3.19]
    f22 = interp1d(ro_w, x22)
    x32 = [2.15, 2.53, 2.74, 2.71]
    f32 = interp1d(ro_w, x32)
    f2 = interp1d(p, [f12(roW), f22(roW), f32(roW)])
    q_cr.append(f2(12.7))

    x13 = [2.30, 2.44, 2.57, 2.50]
    f13 = interp1d(ro_w, x13)
    x23 = [2.09, 2.30, 2.41, 2.39]
    f23 = interp1d(ro_w, x23)
    x33 = [1.76, 2.04, 2.21, 2.20]
    f33 = interp1d(ro_w, x33)
    f3 = interp1d(p, [f13(roW), f23(roW), f33(roW)])
    q_cr.append(f3(12.7))

    x14 = [1.93, 1.89, 1.86 , 1.75]
    f14 = interp1d(ro_w, x14)
    x24 = [1.70, 1.77, 1.83, 1.76]
    f24 = interp1d(ro_w, x24)
    x34 = [1.44, 1.64, 1.75, 1.74]
    f34 = interp1d(ro_w, x34)
    f4 = interp1d(p, [f14(roW), f24(roW), f34(roW)])
    q_cr.append(f4(12.7))

    x15 = [1.55, 1.35, 1.28, 1.22]
    f15 = interp1d(ro_w, x15)
    x25 = [1.34, 1.26, 1.26, 1.23]
    f25 = interp1d(ro_w, x25)
    x35 = [1.11, 1.21, 1.28, 1.30]
    f35 = interp1d(ro_w, x35)
    f5 = interp1d(p, [f15(roW), f25(roW), f35(roW)])
    q_cr.append(f5(12.7))

    q_cr = list(map(lambda x: x * K, q_cr))
    print("==========q_cr==========")
    print(q_cr)


def calc_x_and_q_and_plot():
    step = 0.2
    z = np.arange(0, 1.2 + step, step)
    z = np.append(z, 1.3)

    step_inter = 0.01
    z_inter = np.arange(0, 1.3 + step_inter, step_inter)
    q_inter = interp1d(z_inter, q)

    q_nom = list(map(lambda z: q_inter(z), z))
    print("==========q_nom'==========")
    print(q_nom)

    print("==========%.2fq_nom======" % data["n"])
    q_max_nom = list(map(lambda x: x * float(data["n"]), q_nom))
    print(q_max_nom)

    #x calculation
    param = {}
    resultsDistr = {}
    utils.fill_dict_from_file(param, "../Distributions/distributionsInput.txt")
    utils.fill_dict_from_file(resultsDistr, "../Distributions/distributionOut.txt")
    qMax = resultsDistr["kV"] * param["qSr"] * 1e6
    z = np.append(np.arange(0, 1.2 + 0.2, 0.2), 1.3)
    f0 = -1 * (qMax * (param["Haz"] + 2 * resultsDistr["deltaZ"])) / np.pi  * np.sin(np.pi / 2 * ((param["Haz"]) / (param["Haz"] + 2 * resultsDistr["deltaZ"])))
    f = -1 * (qMax * (param["Haz"] + 2 * resultsDistr["deltaZ"])) / np.pi  * np.sin(np.pi / 2 * ((param["Haz"] - 2 * z) / (param["Haz"] + 2 * resultsDistr["deltaZ"]))) - f0
    water1 = IAPWS97(T = 294.7 + 273, P = 12.7)
    water2 = IAPWS97(P = 12.7, x = 0.0)
    x_in = (water1.h - water2.h) / 1.1554e+006 * 1e3 + 0.01
    x = x_in + (param["kG"] * param["Pt"]) / (param["kQ"] * param["kR"] * param["Gtn"] * 1.1504e+006) * f
    x195 = x_in + (param["kG"] * param["Pt"]) / (param["kQ"] * param["kR"] * param["Gtn"] * 1.1504e+006) * f * float(data["n"])
    print("----------x:----------\n")
    print(x)
    print("---------x%.2f:--------\n" % data["n"])
    print(x195)


    distr_q_x = interp1d(x, q_nom)
    distr_qMax_x = interp1d(x195, q_max_nom)

    #splining
    xnew = np.linspace(min(x), max(x), 10)
    q_smooth = spline(x, distr_q_x(x), xnew)
    x195new = np.linspace(min(x195), max(x195), 20)
    qMax_smooth = spline(x195, distr_qMax_x(x195), x195new)

    x_table = np.arange(-0.2, 0.2 + 0.1, 0.1)
    distr_q_cr = interp1d(x_table, q_cr, kind='cubic')

    x_plot = np.arange(-0.2, 0.2 + 0.02, 0.02)

    plt.plot(x_plot, distr_q_cr(x_plot), label=r'$q_{кр}$')
    plt.plot(x_plot, distr_q_cr(x_plot) * (1 + 3 * 0.15), 'r--', label=r'$q_{кр. откл.}$')
    plt.plot(x_plot, distr_q_cr(x_plot) * (1 - 3 * 0.15), 'r--')
    plt.plot(xnew, q_smooth * 1e-6, 's--', label = r'$q_{ном}$')
    plt.plot(x195new, qMax_smooth * 1e-6, 'o--', label = (r'$%.1fq_{ном}$' % data["n"]))
    plt.xlabel(r'x', fontsize=14, fontweight='bold')
    plt.ylabel(r'q, $МВт/м^2$', fontsize=14, fontweight='bold')
    plt.legend()
    plt.grid(True)
    plt.show()

data = {}
utils.fill_dict_from_file(data, "heat_crisisInput.txt")
calc_K()
calc_q_critical()
calc_x_and_q_and_plot()
