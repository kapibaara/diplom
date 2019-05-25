import utils
import math

# Dj
E3 = 3e6 * 1.6e-19
E5 = 5e6 * 1.6e-19

gamma_3MeV = 0.2
gamma_5MeV = 0.15


def after_defence_calc(D03, D05):
	mu_d3 = data['muH2O3D'] * data['dH2O'] + data['muSt3'] * data['dSt'] + data['muBet3'] * data['dBet']
	mu_d5 = data['muH2O5D'] * data['dH2O'] + data['muSt5'] * data['dSt'] + data['muBet5'] * data['dBet']

	mu_d3_2 = data['muH2O3D'] * data['dH2O'] + data['muSt3'] * data['dSt']
	mu_d5_2 = data['muH2O5D'] * data['dH2O'] + data['muSt5'] * data['dSt']

	mu_d3_3 = data['muH2O3D'] * data['dH2O']
	mu_d5_3 = data['muH2O5D'] * data['dH2O']

	Dnr3 = D03 * math.exp(-mu_d3)
	Dnr5 = D05 * math.exp(-mu_d5)

	B3 = B_calc(data['A1Bet3'], data['alpha1Bet3'], data['alpha2Bet3'], mu_d3) + \
		B_calc(data['A1H2O3'], data['alpha1H2O3'], data['alpha2H2O3'], mu_d3_3) - \
		B_calc(data['A1St3'], data['alpha1St3'], data['alpha2St3'], mu_d3_3) + \
		B_calc(data['A1St3'], data['alpha1St3'], data['alpha2St3'], mu_d3_2) - \
		B_calc(data['A1Bet3'], data['alpha1Bet3'], data['alpha2Bet3'], mu_d3_2)

	B5 = B_calc(data['A1Bet5'], data['alpha1Bet5'], data['alpha2Bet5'], mu_d5) + \
		B_calc(data['A1H2O5'], data['alpha1H2O5'], data['alpha2H2O5'], mu_d5_3) - \
		B_calc(data['A1St5'], data['alpha1St5'], data['alpha2St5'], mu_d5_3) + \
		B_calc(data['A1St5'], data['alpha1St5'], data['alpha2St5'], mu_d5_2) - \
		B_calc(data['A1Bet5'], data['alpha1Bet5'], data['alpha2Bet5'], mu_d5_2)

	D3_full = B3 * Dnr3
	D5_full = B5 * Dnr5

	D_full = 3 * (D3_full + D5_full)

	print("Dnr3 = {0:.2e} zv/s".format(Dnr3))
	print("Dnr5 = {0:.2e} zv/s".format(Dnr5))

	print("B3 = {0:.2e}".format(B3))
	print("B5 = {0:.2e}".format(B5))

	print("D3_full = {0:.2e} zv/s".format(D3_full))
	print("D5_full = {0:.2e} zv/s".format(D5_full))
	print("D_full = {0:.2e} zv/s".format(D_full))


def B_calc(A1, alpha1, alpha2, mu_d):
	return A1 * math.exp(-alpha1 * mu_d) + (1 - A1) * math.exp(-alpha2 * mu_d)


data = {}
utils.fill_dict_from_file(data, "inputGamma.txt")

if __name__ == "__main__":
	after_defence_calc(201, 252)
