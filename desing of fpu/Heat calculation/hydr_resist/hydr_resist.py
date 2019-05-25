import utils
from iapws import IAPWS97
import math

deltaP_tr = 0
deltaP_local = 0

def frictional_resistance_calc():
    global deltaP_tr

    water = IAPWS97(T = data["tAvgWater"] + 273, P = 12.7)
    nu = water.Liquid.nu
    Re = data["w"] * (data["dG"] * 1e-3) / nu

    ksi0 = (1.82 * math.log(Re, 10) - 1.64) ** (-2)
    ksi = ksi0 * (1.10 + 0.18 * (data["x"] - 1))

    deltaP_tr = ksi * data["Haz"] * data["roH2O"] * data["w"] ** 2 / (2 * data["dG"])

    out.write("Re: %.3f\nksi0: %.3f\nksi: %.3f\ndeltaP_tr: %.3f Pa\n" % (Re, ksi0, ksi, deltaP_tr))
    out.write("\n")

def local_resistance_calc():
    global deltaP_local

    ksi_in = 6
    ksi_out = 4
    ksi_nr = 2
    ksi_vr = 3
    ksi_dr5 = 1.1
    ksi_sum = ksi_in + ksi_out + ksi_nr + ksi_vr + ksi_dr5;
    deltaP_local = ksi_sum * data["roH2O"] * data["w"] ** 2 / 2
    out.write("deltaP_local: %.3f Pa\n" % (deltaP_local))

def result_calc():
    deltaP = deltaP_tr + deltaP_local
    V = data["Gtn"] / data["roH2O"]
    Ntn = V * deltaP / data["etaN"]
    print(Ntn * 1e-3)

    out.write("deltaP: %.3f Pa\nNtn: %.3f Vt\n" % (deltaP, Ntn))


data = {}
utils.fill_dict_from_file(data, "hydr_resistInput.txt")
out = open("hydr_resistOutput.txt", "w");
frictional_resistance_calc()
local_resistance_calc()
result_calc()
out.close()
