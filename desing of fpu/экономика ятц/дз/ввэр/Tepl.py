import math

Nel = 1000
brutto = 0.36
Kcn = 0.06
K = 1.05e-3
Go = 70
n = 3
Go5Go9 = 1.0
Xn = 4.4 * 1e-2
Zn = 0
KH = 0.25
B = 40000
Xk = 1.3 * 1e-2
z0 = 0.25
c = 0.714 * 1e-2
y = 0.2 * 1e-2
Ei = 0.01 * 1e-2
Ti = 0.25
Tckl = 0.5
Txp = 0.5
Tvyd = 1
Tcl = 50
Cu = 100
Craz = 140
Cizg = 300
Creg = 1000
fi = 0.85
Aren = 3.7 * 1e-2
Kud = 1400
nud = 0.5
Fz = 14000
Cxp = 250
En = 0.11

# ---- Незамкнутый топливный цикл ------
W = Nel * 8760 * fi * (1 - Kcn) / 1e6
a = K * B
z = a * KH
Gx = Nel * 365 * fi / (brutto * B)
G9 = z * Gx * (1 - Ei)
G9tot = G9 * Tcl

KBC = (1 - a * 1e-3 - z * 1e-3) * (1 - Ei) ** 3 * (Xk - y) / (Xn - y)
fxnyc = (Xn - y) / (c - y)
Gc = fxnyc * Gx * ((1 + Ei) ** 3)
deltaG5 = Gc * c * 1000
KBec = (z * 1e-3 * (1 - Ei)) / (Xn * (1 + Ei) - Xk * (1 - Ei))
Tk = Go / Gx
Gxtot = Go + Gx * (Tcl - Tk / n)
Gctot = Gxtot * fxnyc * (1 + Ei) ** 2
Gy = (fxnyc - 1) * Gx * (1 + Ei) ** 2
Gytot = Gxtot * Gy / Gx

Vxn = (2 * Xn - 1) * math.log(Xn / (1 - Xn))
Vy = (2 * y - 1) * math.log(y / (1 - y))
Vc = (2 * c - 1) * math.log(c / (1 - c))
nxnyc = Vxn + Vy * (fxnyc - 1) - Vc * fxnyc

Ct = (1 / (W * 1e6)) * (Gc * Cu + Gx * nxnyc * Craz * (1 + Ei) ** 2 + Gx * (1 + Ei) * Cizg + Gx * Cxp)
Ca = Aren * Kud / (8760 * fi)
Cz = nud * Fz / (8760 * fi * 1000)
Ce = 1.25 * (1.2 * Ct + 4 * Ca + 3.5 * Cz)
Ctop = (fxnyc * ((1 + Ei) ** 3) * Cu + nxnyc * ((1 + Ei) ** 2) * Craz + (1 + Ei) * Cizg) / (Xn * 1000)

# ---- Замкнутый топливный цикл ------
Gczm = fxnyc * Gx * ((1 + Ei) ** 3) * (1 - KBC)
deltaG5zam = Gczm * c * 1000
f5yc = (0.95 - y) / (c - y)
V5 = (2 * 0.95 - 1) * math.log(0.95 / (1 - 0.95))
n5yc = V5 + Vy * (f5yc - 1) - Vc * f5yc
C5 = Cu * f5yc + n5yc * Craz
b = C5 * Go5Go9 * (1 - z0)
fxnyxk = (Xn - y) / (Xk - y)
Vxk = (2 * Xk - 1) * math.log(Xk / (1 - Xk))
nxnyxk = Vxn + Vy * (fxnyxk - 1) - Vxk * fxnyxk
Cxk = fxnyxk * Cu + nxnyxk * Craz
Ctzam = 1 / (W * 1e6) * (Gczm * Cu + Gx * (1 - KBC) * nxnyc * Craz * (1 + Ei) ** 2 + Gx * (1 + Ei) * Cizg + Gx * Cxp \
                         + Gx * (1 - Ei) * Creg + nxnyxk * Craz * Gx * KBC * (1 + Ei) - b * Gx * z * 1e-3)
Cezam = 1.25 * (1.2 * Ctzam + 4 * Ca + 3.5 * Cz)
Tc = Tk + Tckl + Txp + Tvyd + 3 * Ti + Tk / n

# ------расчет РЗ---------
Tcvn = 3 * Ti + Tk / n
DOC = Tcvn * Gx * 1000 * (Cxk + b * z * 1e-3)
y1 = DOC * 1e-3 / Nel

RZclose = Cezam + En * (y1 + Kud) / (8760 * fi)
RZopen = Ce + En * Kud / (8760 * fi)

print('---- Незамкнутый топливный цикл ------')
print('W = {}*10^6  Мвт*час/год'.format(W))
print('Выгорание a = {} кг осколков/т топлива'.format(a))
print('Ежегодный расход топлива:Gx = {:.2f} т/год'.format(Gx))
print('накопление Pu239 в год: {:.2f} кг/год'.format(G9))
print('Полное накопление Pu239 : {:.1f} кг'.format(G9tot))
print('z: {}'.format(z))
print('КВЦ: {:.3f}'.format(KBC))
print('Коэффициен расхода природного урана:{:.2f}'.format(fxnyc))
print('Ежегодная потребность в природном уране: {:.1f} т/год'.format(Gc))
print('Ежегодная потребность в U235: {:.1f} кг/год'.format(deltaG5))
print('КВэк: {:.2f}'.format(KBec))
print('Кампания топлива: {:.1f} года'.format(Tk))
print('Полная потребность в топливе Gxtot: {:.1f} т'.format(Gxtot))
print('Полная потр в природном уране Gctot: {:.1f} т'.format(Gctot))
print('Ежегодное накопление отв. урана Gy: {:.1f} т/год'.format(Gy))
print('Полное накопление отв. урана Gytot: {:.1f} т'.format(Gytot))
print('Удельная работа разделения: {:.2f} '.format(nxnyc))
print('Потенциалы разделения:\nVxn {:.2f}\nVy {:.2f}\nVc {:.2f}'.format(Vxn, Vy, Vc))
print('Топл. сост. себистоимости Ст: {:.5f} $/КВт*час'.format(Ct))
print('Аморт. сост. Ca: {:.3f} $/КВт*час'.format(Ca))
print('Сост. зарплаты Cz: {:.5f} $/КВт*час'.format(Cz))
print('Себестоимость отпущ. энергии: {:.3f} $/КВт*час'.format(Ce))
print('Цена топлива: {:.2f} $/г.дел'.format(Ctop))

print('---- Замкнутый топливный цикл ------')
print('Ежегодная потребность в природном уране: {:.1f} т/год'.format(Gczm))
print('Ежегодная потребность в U235: {:.1f} кг/год'.format(deltaG5zam))
print('f5yc: {:.1f}'.format(f5yc))
print('V5: {:.2f}'.format(V5))
print('n5yc: {:.1f}'.format(n5yc))
print('Цена обогащенного до 0.95% U: {:.1f} $/кг'.format(C5))
print('Цена Pu 239: {:.1f} $/кг'.format(b))
print('fxnyxk: {:.2f}'.format(fxnyxk))
print('Vxk: {:.2f}'.format(Vxk))
print('nxnyxk: {:.1f}'.format(nxnyxk))
print('Топл. сост. себистоимости Ст: {:.5f} $/КВт*час'.format(Ctzam))
print('Длительность топливного цикла: {:.1f} лет'.format(Tc))
print('------расчет РЗ---------')
print('Doc: {:.1f} $'.format(DOC))
print('y: {:.2f} $/кВт'.format(y1))
print('RZclose: {:.6f} $/КВт*час'.format(RZclose))
print('RZopen: {:.6f} $/КВт*час'.format(RZopen))
