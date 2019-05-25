import utils
import math
import numpy as np
import matplotlib.pyplot as plt

def write_header(file, header):
  file.write("======================\n")
  file.write(header + "\n")
  file.write("----------------------\n")

def first_circuit_params_calculation(data):
  file = open("heat_paramsOutput.txt", "w")
  write_header(file, "1st circuit parameters")

  Gr = data["Wsr"] * data["roH2O"] * data["Stn"] / data["kG"] * data["Ntvs"]
  Gtn = Gr * data["kG"] / data["Ntvs"]
  deltaTr = data["Qr"] / (data["Cp"] * Gr)
  tVhR = data["tVihR"] - deltaTr

  file.write("Gr: %.3f kg/s\nGtn: %.3f kg/s\ndeltaTr: %.3f C\ntVhR: %.3f C\n\n" % (Gr, Gtn, deltaTr, tVhR))
  file.close()

def avg_heat_characteristics_calculation(data):
    file = open("heat_paramsOutput.txt", "a")
    write_header(file, "Average heat characteristics")

    qV = (data["Qr"] / data["Vaz"]) / 1e6
    Qtvs = data["kQ"] * data["Qr"] / data["Ntvs"] / 1e6
    qlSr = Qtvs / (data["Ntvel"] * data["Haz"] * 1e-1) * 1e6
    qSr = qlSr / (math.pi * data["dTvel"] * 1e-3) * 1e-4

    file.write("qV: %.3f MVt/m^3\nQtvs: %.3f MVt\nqlSr: %.3f Vt/cm\nqSr: %.3f MVt/m^2\n\n" % (qV, Qtvs, qlSr, qSr))
    file.close()


data = {}
utils.fill_dict_from_file(data, "heat_paramsInput.txt")
first_circuit_params_calculation(data)
avg_heat_characteristics_calculation(data)
