import math
import tes
Nel = 70
brutto = 0.26
Kcn = 0.06
n = 3
fi = 0.7
B = 159000
Tk = 8.4
Tcl = 40
Zn = 0.15
Ei = 0.01 * 1e-2
c = 0.714 * 1e-2
y = 0.2 * 1e-2
Cu = 100
Craz = 140
Go5Go9 = 1.0
z0 = 0.25
Cizg = 1500
Cxp = 250
Aren = 3.7 * 1e-2
Kud = 1400
En = 0.11
Fz = 14000
nud = 0.5

W = Nel * 8760 * fi * (1 - Kcn)
Gx = Nel * 365 * fi / (brutto * B)
G0 = Gx * Tk
Gxtot = G0 + Gx * (Tcl - Tk / n)
Gz = Gx * Zn * (1 + Ei)
Gu = Gx * (1 - Zn) * (1 + Ei) ** 2

V095 = (2 * 0.95 - 1) * math.log(0.95 / (1 - 0.95))
Vy = (2 * y - 1) * math.log(y / (1 - y))
Vc = (2 * c - 1) * math.log(c / (1 - c))
f095yc = (0.95 - y) / (c - y)
n095yc = V095 + Vy * (f095yc - 1) - f095yc * Vc
Cu095 = f095yc * Cu + n095yc * Craz
b = Cu095 * Go5Go9 * (1 - z0)

Ct = 1 / W * (Gu * Cu + Gz * b + Gx * (Cizg * (1 + Ei) + Cxp))
Ca = Aren * Kud / (8760 * fi)
Cz = nud * Fz / (8760 * fi * 1000)
Ce = 1.25 * (1.2 * Ct + 4 * Ca + 3.5 * Cz)
RZopen = Ce + En * Kud / (8760 * fi)


print('---- Экономический расчет ПАТЭС ------')
print('W = {:.2f}*10^5  Мвт*час/год'.format(W / 1e5))
print('Ежегодный расход топлива:Gx = {:.2f} т/год'.format(Gx))
print('Начальная загрузка топлива:G0 = {:.2f} т'.format(G0))
print('Полная потребность в топливе Gtot: {:.2f} т'.format(Gxtot))
print('Ежегодня потребность в покупном плутонии Gz: {:.2f} кг/год'.format(Gz * 1000))
print('Ежегодня потребность в природном уране Gu: {:.2f} кг/год'.format(Gu * 1000))
print('Потенциалы разделения:\nV095 {:.2f}\nVy {:.2f}\nVc {:.2f}'.format(V095, Vy, Vc))
print('f5yc: {:.2f}'.format(f095yc))
print('n5yc: {:.2f}'.format(n095yc))
print('Цена обогащенного до 0.95% U: {:.2f} $/кг'.format(Cu095))
print('Цена Pu 239: {:.2f} $/кг'.format(b))
print('Топл. сост. себистоимости Ст: {:.5f} $/КВт*час'.format(Ct))
print('Аморт. сост. Ca: {:.3f} $/КВт*час'.format(Ca))
print('Сост. зарплаты Cz: {:.5f} $/КВт*час'.format(Cz))
print('Себестоимость отпущ. энергии: {:.5f} $/КВт*час'.format(Ce))

tes.tes_calc()
