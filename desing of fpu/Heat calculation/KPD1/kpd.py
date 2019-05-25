import utils

p0 = 3.43e6
t0 = 285
i0 = 2.9399e6
s0 = 6388.7
pk = 0.005e6
tk = 32.875
iks = 1.3777e+005
sks = 476.25
ppv = 6e6
tpv = 170
ipv = 7.2207e5
spv = 2035.3
n = 1
We = 35e6



Tk = tk + 273

def kpd_calc():
    eta_t0 = 1 - Tk * (s0 - sks) / (i0 - iks)
    print("eta_t0: %.3f" % eta_t0)
    eta_tinf = 1 - Tk * (s0 - spv) / (i0 - ipv)
    print("eta_tinf: %.3f" % eta_tinf)
    eta_tn = eta_t0 + n / (n + 1) * (eta_tinf - eta_t0)
    print("eta_tn: %.3f" % eta_tn)
    eta_0i = 0.75
    eta_m = 0.97
    eta_g = 0.98
    eta_t = 0.98
    eta_br = eta_tn * eta_0i * eta_m * eta_g * eta_t
    print("eta_br: %.3f" % eta_br)
    Q = We / eta_br
    print("Qr: %.3f MVt" % (Q * 1e-6))


kpd_calc()
