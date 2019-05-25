import utils
import scipy.integrate as integrate
from scipy.special import j0, j1
from scipy.optimize import root
import numpy as np

import matplotlib.pyplot as plt


def tetta_calc(tau):
	F0 = at * tau / (data['r1'] ** 2)

	A = 1 / 2 * (1 / Bi0 - 1 / Bi)
	B = 1 / (mu ** 2)
	C = integrate.quad(lambda x: qV(x) / data['qV0'] * np.exp(mu ** 2 * x), 0, F0)[0]

	An = (2 * j1(mu) / (mu * (j0(mu) ** 2 + j1(mu) ** 2)))[0]

	D = (An * np.exp(-mu ** 2 * F0))[0]
	res = ((A + B + C) * D)[0]
	return res


def Q_big_calc(tau):
	F0 = at * tau / (data['r1'] ** 2)

	A = 1 / 2 * (1 / Bi0 - 1 / Bi)
	B = 1 / (mu ** 2)
	C = integrate.quad(lambda x: qV(x) / data['qV0'] * np.exp(mu ** 2 * x), 0, F0)[0]

	An = 2 * j1(mu) / (mu * (j0(mu) ** 2 + j1(mu) ** 2))

	D = An * mu * j1(mu) * np.exp(-mu ** 2 * F0)
	res = (A + B + C) * D
	return res


def t_fuel_calc(tau):
	tetta = tetta_calc(tau)
	result = tetta * data['qV0'] * data['r1'] ** 2 / data['lambdaT'] + data['tW']
	return result


def q_calc(tau):
	Q_big = Q_big_calc(tau)
	result = Q_big * data['qV0'] * data['r1']
	return result


def t_sh_out_calc(tau):
	return data['tW'] + q_calc(tau) / alpha


def t_sh_in_calc(tau):
	return data['tW'] + q_calc(tau) * (1 / alpha + data['delta_sh'] / data['lambda_sh'])


# def qV(tau):
# 	qV = data['qV0'] * (0.99 * (np.exp(-tau / data['tau1'])) ** 2 + (
# 			0.1 * ((tau + data['tau2']) / data['tau2']) ** (-0.2)) ** 2) ** 0.5
# 	return qV

def qV(tau):
	qV = 6.5e-2 * data['qV0'] * tau ** (-0.2)
	return qV


def j_fun(x, Bi):
	return j0(x) / j1(x) - x / Bi


def alpha_fun(tau, T):
	return alpha0 * np.exp(-tau / T)


def get_mu(Bi):
	start = 0.001

	mu1 = root(j_fun, start, args=(Bi,)).x
	return mu1


data = {}
utils.fill_dict_from_file(data, "input.txt")

if __name__ == "__main__":
	alpha0 = data['alpha0']
	T = data['T']

	at = data['lambdaT'] / (data['Cp'] * data['ro'])
	k0 = 1 / (data['delta_sh'] / data['lambda_sh'] + 1 / alpha0)
	Bi0 = k0 * data['r1'] / data['lambdaT']

	tau = []
	t_fuel = []
	t_sh_out = []
	t_sh_in = []
	qv = []

	tau_new = 0

	while True:
		tau_new += 0.5
		qv.append(qV(tau_new) / data['qV0'])

		alpha = alpha_fun(tau_new, T)
		k = 1 / (data['delta_sh'] / data['lambda_sh'] + 1 / alpha)
		Bi = k * data['r1'] / data['lambdaT']

		mu = get_mu(Bi)

		t_fuel_new = t_fuel_calc(tau_new)
		# if tau_new < 2500:
		# 	t_fuel_new = t_fuel_calc(tau_new)
		# else:
		# 	t_fuel_new = t_fuel[-1] + 5

		print(
			'tau = {}\nBi = {}.\nMu: {}\ntemp: {}\n' \
			.format(tau_new, Bi, mu, t_fuel_new)
		)

		t_fuel.append(t_fuel_new)
		tau.append(tau_new)

		if t_fuel_new > 600:
			break

	plt.figure(0)
	plt.plot(tau, t_fuel)

	plt.xlabel('t, сек')
	plt.ylabel(r'$T_{топ},^\circ$C')

	plt.grid(True)


	plt.figure(1)
	plt.plot(tau, qv)
	plt.xlabel('t, сек')
	plt.ylabel(r'$q_V/q_{V0}$')
	#plt.title('Энерговыделение от времени')
	plt.grid(True)

	plt.show()
