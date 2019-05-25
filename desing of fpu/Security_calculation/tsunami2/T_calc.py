import numpy as np

betta = 0.0021
lam = 0.0648
lamB = 1e-5
ro = -0.013038

T1 = -(ro - betta - lam * lamB) / (2 * ro * lam) \
     + (((ro - betta - lam * lamB) / (2 * ro * lam)) ** 2 + lamB / (ro * lam)) ** 0.5

T2 = -(ro - betta - lam * lamB) / (2 * ro * lam) \
     - (((ro - betta - lam * lamB) / (2 * ro * lam)) ** 2 + lamB / (ro * lam)) ** 0.5


print("T1: {}".format(T1))
print("T2: {}".format(T2))