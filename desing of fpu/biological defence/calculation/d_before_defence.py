import utils

#Djoul (200 MeV)
Ef = 3.2e-11
#Djoul, avg energy of fission specter (2 MeV)
E = 3.2e-13

def before_defence_calc():
    Nf = data["Wt"] * 1e6 / Ef
    Nn = Nf * data["nuF"]
    F = Nn * (data["kinf"] - 1) / (data["S"] * 1e4)
    Fn = F * data["gamma"]
    D0 = Fn * E * data["muEn"] * data["k"]
    print("Nf = {0:.2e} del/sec".format(Nf))
    print("Nn = {0:.2e} neutron/sec".format(Nn))
    print("F = {0:.2e} neutron/sm^2*sec".format(F))
    print("Fn = {0:.2e} neutron/sm^2*sec".format(Fn))
    print("D0 = {} Zv/sec".format(D0))

    return D0

data = {}
utils.fill_dict_from_file(data, "inputNeutron.txt")


if __name__ == "__main__":
    before_defence_calc()
