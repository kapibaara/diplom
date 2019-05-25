import utils
import math

#Dj
E3 = 3e6 * 1.6e-19
E5 = 5e6 * 1.6e-19

gamma_3MeV = 0.2
gamma_5MeV = 0.15


# def mu_calc():
#     results = {}
#
#     muH2O3 = data['muH2O3T'] * data['roH2O'] / data['roH2OT']
#     muH2O5 = data['muH2O5T'] * data['roH2O'] / data['roH2OT']
#
#     results['muH2O3'] = muH2O3
#     results['muH2O5'] = muH2O5
#
#     return results

def before_defence_calc():
	I3 = data['Nf'] * data['nuGamma'] * gamma_3MeV
	I5 = data['Nf'] * data['nuGamma'] * gamma_5MeV

	L = 4 * data['Vaz'] / data['Spov'] * 100

	mu3 = data['muH2O3T'] * data['epsCol'] + data['muFuel3'] * data['epsTop'] + data['muZr3'] * data['epsKM']
	mu5 = data['muH2O5T'] * data['epsCol'] + data['muFuel5'] * data['epsTop'] + data['muZr5'] * data['epsKM']

	Q3 = I3 / L
	Q5 = I5 / L

	N3 = Q3/mu3 * (1 - math.exp(-mu3 * L))
	N5 = Q5/mu5 * (1 - math.exp(-mu5 * L))

	F3 = N3 / data['Spov']
	F5 = N5 / data['Spov']
	Fg3 = F3 * 2
	Fg5 = F5 * 2

	D03 = F3 * E3 * 100e-4 * 1
	D05 = F5 * E5 * 100e-4 * 1

	print("I3 = {0:.2e} gamma/sec".format(I3))
	print("I5 = {0:.2e} gamma/sec".format(I5))

	print("L = {0:.2e} sm".format(L))

	print("mu3 = {0:.2e} sm^-1".format(mu3))
	print("mu5 = {0:.2e} sm^-1".format(mu5))

	print("Q3 = {0:.2e} kv/(s*sm)".format(Q3))
	print("Q5 = {0:.2e} kv/(s*sm)".format(Q5))

	print("N3 = {0:.2e} kv/(s)".format(N3))
	print("N5 = {0:.2e} kv/(s)".format(N5))

	print("F3 = {0:.2e} kv/(m^2 * s)".format(F3))
	print("F5 = {0:.2e} kv/(m^2 * s)".format(F5))

	print("Fg3 = {0:.2e} kv/(m^2 * s)".format(Fg3))
	print("Fg5 = {0:.2e} kv/(m^2 * s)".format(Fg5))

	print("D03 = {0:.2e} zv/s".format(D03))
	print("D05 = {0:.2e} zv/s".format(D05))

	return D03, D05



data = {}
utils.fill_dict_from_file(data, "inputGamma.txt")

if __name__ == "__main__":
	before_defence_calc()
