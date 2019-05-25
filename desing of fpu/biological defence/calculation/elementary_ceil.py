import utils
import math

def calculate_params():
    S_ceil = 3 ** 0.5 * data['a'] ** 2 / 2
    eps_top = math.pi * (data['dTv'] - 2 * data['deltaSh']) ** 2 / 4 / S_ceil
    eps_km = math.pi * data['deltaSh'] * (data['dTv'] - data['deltaSh']) / S_ceil
    eps_col = (S_ceil - math.pi * data['dTv'] ** 2 / 4) / S_ceil
    print("S_ceil = {} mm^2".format(S_ceil))
    print("eps_top = {}".format(eps_top))
    print("eps_km = {}".format(eps_km))
    print("eps_col = {}".format(eps_col))






data = {}
utils.fill_dict_from_file(data, "inputElementary.txt")

if __name__ == '__main__':
    calculate_params()
