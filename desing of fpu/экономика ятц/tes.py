Nel = 70
brutto = 0.4
Kcn = 0.05
mud = 330
fi = 0.7
Aren = 4 * 1e-2
Kud = 1000
nud = 0.7
Fz = 14000
En = 0.11
trez = 1
Tcl = 50
Kut = 1.38
Zoil = 468.3 #Price of oil fuel

def tes_calc():
    W = Nel * 8760 * fi * (1 - Kcn)
    Gx = Nel * 8760 * fi * mud * Kut * 1e-3 / 1e5
    Gxtot = Gx * (Tcl - trez)

    Ca = Aren * Kud / (8760 * fi)
    Cz = nud * Fz / (8760 * fi * 1000)
    Ct = Zoil * mud * Kut * 1e-6
    Ce = 1.25 * (Ct + 2.5 * Ca + 1.5 * Cz)

    RZ = Ce + En * Kud / (8760 * fi)

    print('\n-----Расчет ТЭС---------')
    print('W = {:.2f}*10^5  Мвт*час/год'.format(W / 1e5))
    print('Gx: {:.2f}*10^5  т/год'.format(Gx))
    print('Gxtot: {:.2f} *10^5'.format(Gxtot))
    print('Аморт. сост. Ca: {:.3f} $/КВт*час'.format(Ca))
    print('Сост. зарплаты Cz: {:.5f} $/КВт*час'.format(Cz))
    print('Себ. топлива: {:.3f} $/КВт*час'.format(Ct))
    print('Себестоимость отпущ. энергии: {:.3f} $/КВт*час'.format(Ce))
