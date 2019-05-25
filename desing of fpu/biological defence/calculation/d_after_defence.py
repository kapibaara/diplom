import utils
import math

roH2O_tabl = 1
#Zv/S
Dpdd = 6.61376e-10

def after_defence_calc(D_before_def):
    SrH2O = data["SrH2O"] * (data["roH2O"] / roH2O_tabl)
    dBet = - (SrH2O * data["dH2O"] + data["SrSt"] * data["dSt"] + math.log(Dpdd / D_before_def)) / (data['SrBet'])
    print("SrH2O = {} cm^-1".format(SrH2O))
    print("dBet = {} cm".format(dBet))

    return dBet

data = {}
utils.fill_dict_from_file(data, "inputNeutron.txt")
