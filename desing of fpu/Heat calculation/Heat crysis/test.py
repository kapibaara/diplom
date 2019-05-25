from iapws import IAPWS97

water1 = IAPWS97(T = 294.7 + 273, P = 12.7)
water2 = IAPWS97(P = 12.7, x = 0.0)
res = (water1.h - water2.h) / 1.1504e+006 * 1e3

print(res)

print((1.3111e+006 - 1.5195e+006) / 1.1504e+006)
