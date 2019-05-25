import numpy as np
import utils


# data - dictionary of all important data for calculation
def calc_efficiency_and_write_to_file(data):
    Gsp = data["Qsp"] / (data["i2"] - data["is2"])
    g1 = (data["ipv"] - data["is2"]) / (data["i1"] - data["is1"])

    A = np.array([[1, 1, data["x2c"] / data["x2"], data["x2c"] / data["x2"]],
                  [0, 0, data["i3"] - data["is3"], -(data["ism1"] - data["isk"])],
                  [data["is2"], data["i2"], data["is3"] + (data["x2c"] / data["x2"] - 1) * data["is2"],
                   data["ism1"] + (data["x2c"] / data["x2"] - 1) * data["is2"]],
                  [(data["we"] * (data["i2"] - data["is2"])) / (data["etam"] * data["etag"] * data["Qsp"]), 0,
                   -(data["i2c"] - data["i3"]), -(data["i2c"] - data["ik"])]])
    B = np.array(
        [1 - g1, 0, data["is2"] - g1 * data["is1"], data["i0"] - data["i1"] + (data["i1"] - data["i2"]) * (1 - g1)])
    X = np.linalg.solve(A, B)

    gsp = X[0]
    gd = X[1]
    g3 = X[2]
    gk = X[3]

    gs = (g3 + gk) * (data["x2c"] / data["x2"] - 1)
    gs2 = g3 + gk
    gs1 = gs + gs2
    g2 = gsp + gd + gs1
    G0 = Gsp / gsp
    G1 = g1 * G0
    G2 = g2 * G0
    Gd = gd * G0
    Gs1 = gs1 * G0
    Gs2 = gs2 * G0
    Gs = gs * G0
    G3 = g3 * G0
    Gk = gk * G0
    ism2 = (g3 * data["is3"] + gk * data["ism1"]) / (g3 + gk)
    etaE = data["we"] / (G0 * (data["i0"] - data["ipv"]) - data["Qsp"])
    etaEATEC = etaE * data["etaoksr"]
    etaTATEC = data["etaoksr"] * data["etatsn"]
    Qr = data["we"] / etaEATEC + (data["etatsn"] * data["Qsp"]) / etaTATEC
    Gtk = data["Qsp"] / (data["isp2vih"] - data["isp2vh"])

    # Write to file
    out_file = open("KPDOut.txt", "w")

    out_file.write("Relative steam consumption\n")
    out_file.write("---------------------------\n")
    out_file.write("gpg2   gpik    g0   g1      g2      g3      gk      gd      gs      gs1     gs2     gsp\n")
    out_file.write("1       0      1   %.3f   %.3f   %.3f   %.3f   %.3f   %.3f   %.3f   %.3f   %.3f\n\n" % (
        g1, g2, g3, gk, gd, gs, gs1, gs2, gsp))
    out_file.write("Absolute steam consumption\n")
    out_file.write("---------------------------\n")
    out_file.write("Gpg2   Gpik    G0      G1       G2      G3       Gk      Gd      Gs      Gs1      Gs2      Gsp\n")
    out_file.write("%.3f   0   %.3f   %.3f   %.3f   %.3f   %.3f   %.3f   %.3f   %.3f   %.3f   %.3f\n\n" % (
        G0, G0, G1, G2, G3, Gk, Gd, Gs, Gs1, Gs2, Gsp))
    out_file.write("Efficiency and heat power of reactor\n")
    out_file.write("---------------------------\n")
    out_file.write("etaE   etaEATEC    etaTATEC    Qr            Gtk\n")
    out_file.write("%.3f    %.3f       %.3f   %.3e   %.3e" % (etaE, etaEATEC, etaTATEC, Qr, Gtk))

    out_file.close()


data = {}
utils.fill_dict_from_file(data, 'KPDinput.txt')
calc_efficiency_and_write_to_file(data)
