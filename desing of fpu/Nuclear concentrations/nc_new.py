import utils
import math

NA = 6.02e23

# UO2+PuO2 - MOX fuel
def calc_fuel_nc():
    # N0UO2 = data["roUO2"] * NA / data["MUO2"]
    # NUO2 = N0UO2 * data["%Fuel"] / 100 * 1e-24
    # NO = 2 * NUO2
    #
    # NAl = data["roAl"] * NA / data["MAl"] * (1 - data["%Fuel"] / 100) * 1e-24
    #
    # N1PuO2 = NUO2 * data["%1PuO2"] / 100
    # N2PuO2 = NUO2 * data["%2PuO2"] / 100
    #
    # N1UO2 = NUO2 * (100 - data["%1PuO2"]) / 100
    # N2UO2 = NUO2 * (100 - data["%2PuO2"]) / 100
    #
    # N1U238 = N1UO2 * 0.993
    # N1U235 = N1UO2 * 0.007
    # N2U238 = N2UO2 * 0.993
    # N2U235 = N2UO2 * 0.007
    #
    # N1Pu239 = N1PuO2 * data["%richPu39"] / 100
    # N1Pu240 = N1PuO2 * data["%richPu40"] / 100
    # N1Pu241 = N1PuO2 * data["%richPu41"] / 100
    #
    # N2Pu239 = N2PuO2 * data["%richPu39"] / 100
    # N2Pu240 = N2PuO2 * data["%richPu40"] / 100
    # N2Pu241 = N2PuO2 * data["%richPu41"] / 100


    richAl = (100 - data['%Fuel']) * 1e-2
    rich1PuO2 = data['%richPu1'] * 1e-2
    rich1UO2 = (data['%Fuel'] - data['%richPu1']) * 1e-2
    rich2PuO2 = data['%richPu2'] * 1e-2
    rich2UO2 = (data['%Fuel'] - data['%richPu2']) * 1e-2

    N1PuO2 = rich1PuO2 * data['roPuO2'] * NA / data['MPuO2'] * 1e-24
    N1Opu = N1PuO2 * 2
    N1Pu239 = N1PuO2 * data['%richPu39'] * 1e-2
    N1Pu240 = N1PuO2 * data['%richPu40'] * 1e-2
    N1Pu241 = N1PuO2 * data['%richPu41'] * 1e-2

    N1UO2 = rich1UO2 * data['roUO2'] * NA / data['MUO2'] * 1e-24
    N1Ou = N1UO2 * 2
    N1U238 = N1UO2 * 0.993
    N1U235 = N1UO2 * 0.007

    N1O = N1Opu + N1Ou

    N2PuO2 = rich2PuO2 * data['roPuO2'] * NA / data['MPuO2'] * 1e-24
    N2Opu = N2PuO2 * 2
    N2Pu239 = N2PuO2 * data['%richPu39'] * 1e-2
    N2Pu240 = N2PuO2 * data['%richPu40'] * 1e-2
    N2Pu241 = N2PuO2 * data['%richPu41'] * 1e-2

    N2UO2 = rich2UO2 * data['roUO2'] * NA / data['MUO2'] * 1e-24
    N2Ou = N2UO2 * 2
    N2U238 = N2UO2 * 0.993
    N2U235 = N2UO2 * 0.007

    N2O = N2Opu + N2Ou

    NAl = richAl * data['roAl'] * NA / data['MAl'] * 1e-24




    out.write("=============================\n")
    out.write("Fuel: UO2 + PuO2")
    out.write("\nN1OTop: %.3e\n\nN1U235: %.3e\nN1U238: %.3e\nN1Pu239: %.3e\nN1Pu240: %.3e\nN1Pu241: %.3e\n"
              % (N1O, N1U235, N1U238, N1Pu239, N1Pu240, N1Pu241))
    out.write("\nN2OTop: %.3e\n\nN2U235: %.3e\nN2U238: %.3e\nN2Pu239: %.3e\nN2Pu240: %.3e\nN2Pu241: %.3e\n"
              % (N2O, N2U235, N2U238, N2Pu239, N2Pu240, N2Pu241))

    out.write("\nNAl: %.3e\n" % (NAl))
    out.write("=============================\n")

# H2O - moderator
def calc_h2o_nc():
    N0H2O = data['roH2O'] * NA / data['MH2O'] * 1e-24
    print("H2O: %.3e" % N0H2O)
    NH = 2 * N0H2O
    NOmod = N0H2O

    out.write("Moderator: H2O\n")
    out.write("NH: %.3e\nNO: %.3e\n" % (NH, NOmod))
    out.write("=============================\n")


# B4C - absorber
def calc_pel_nc():
    NB4C = data['roB4C'] * NA / data['MB4C'] * 1e-24
    print("B4C: %.3e" % NB4C)
    NB = 4 * NB4C
    NB10 = NB * data['%richB10'] / 100
    NC = NB4C

    NCr = data['roCr'] * NA / data['MCr'] * data["%Cr"] / 100 * 1e-24
    NNi = data['roNi'] * NA / data['MNi'] * data["%Ni"] / 100 * 1e-24

    out.write("Pel: B4C\n")
    out.write("NB10: %.3e\nNC: %.3e\n" \
        "\nNCr: %.3f\nNNi: %.3f\n" % (NB10, NC, NCr, NNi))
    out.write("=============================\n")

# Gd2O3 - burnout absorber
def calc_svp_nc():
    NGd2O3 = data['roGd2O3'] * NA / data['MGd2O3'] * 1e-24
    print("Gd2O3: %.3e" % NGd2O3)

    NGd = NGd2O3 * 2
    NGd55 = NGd * data['%richGd155'] / 100
    NGd57 = NGd * data['%richGd157'] / 100

    NO = NGd2O3 * 3

    out.write("Burnout absorber: Gd2O3\n")
    out.write("NGd55: %.3e\nNGd57: %.3e\nNO: %.3e\n" % (NGd55, NGd57, NO))
    out.write("=============================\n")

#B4C, emergency protection bar
def calc_protection_bar_nc():
    NB4C = data['roB4C'] * NA / data['MB4C'] * 1e-24
    NB = 4 * NB4C
    NB10 = NB * data['%richB10AZ'] / 100
    NC = NB4C

    out.write("Emergency protection bar: B4C\n")
    out.write("NB10: %.3e\nNC: %.3e\n" % (NB10, NC))
    out.write("=============================\n")

#NO2
def calc_air_nc():
    NN2O2 = data['roNO2'] * NA / data['MNO2'] * 1e-24
    NO = data["%O"] / 100 * NN2O2
    NN = data["%N"] / 100 * NN2O2
    out.write("Air: N2O2\n")
    out.write("NN: %.3e\nNO: %.3e\n" % (NN, NO))
    out.write("=============================\n")

#99%Zr + 1%Nb
def calc_tvel_shell_nc():
    NZr = data["roZr"] * NA / data["MZr"] * 1e-24

    out.write("Shell of Tvel: Zr 99% + Nb 1%\n")
    out.write("NZr: %.3e\n" % (NZr))
    out.write("=============================\n")


data = {}
utils.fill_dict_from_file(data, 'NCInput_new.txt')

out = open('NCOut_new.txt', 'w')

calc_fuel_nc()
calc_h2o_nc()
calc_pel_nc()
calc_svp_nc()
calc_protection_bar_nc()
calc_air_nc()
calc_tvel_shell_nc()

out.close()
