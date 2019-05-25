<%f2 = lambda s: "{:.2f}".format(s)
f6 = lambda s: "{:.7f}".format(s)%>
:poly
 &vvod
 @ 1 tvel  cell@   rcel(1,1)=0.29,0.34,0.48,
                   ncelsos(1,1)=2,3,4
 @ 2 tvel  cell@   rcel(1,2)=0.29,0.34,0.48,
                   ncelsos(1,2)=2,3,4
 @ 3 svp-1 cell@   rcel(1,3)=0.29,0.34,0.48,
                   ncelsos(1,3)=5,3,4,
 @ 4 svp-2 cell@   rcel(1,4)=0.175,0.225,0.48,
                   ncelsos(1,4)=5,3,4,
 @ 5 pal       @   rcel(1,5)=0.29,0.34,0.48,
                   ncelsos(1,5)=6,7,4,
 @ 6 KM        @   rcel(1,6)=0.39,0.48,
                   ncelsos(1,6)=3,4,

  t=0.6590e+03, 0.5970e+03, 0.5830e+03, 0.5830e+03, 0.5830e+03, 0.5830e+03,
@-----------------------------------------------------------------------
 troiz=
 @         1 tv_1      2 tv_2   3 Zr_ob_tv   4 H2O    5 SVP    6 PEL   7 ob_pel
 @ *h*  @ 0.0000000, 0.0000000, 0.0000000, ${f6(el["H2O"]["h"])}, 0.0000000, 0.0000000, 0.0000000,
 @ *o*  @ 0.0000000, 0.0000000, 0.0000000, ${f6(el["H2O"]["o"])}, 0.0000000, 0.0000000, 0.0000000,
 @ zr   @ 0.0000000, 0.0000000, ${f6(el["Zr_ob_tv"]["zr"])}, 0.0000000, 0.0000000, 0.0000000, 0.0000000,
 @ u235 @ ${f6(el["tv_2"]["u235"])}, ${f6(el["tv_2"]["u235"])}, 0.0000000, 0.0000000, 0.0000000, 0.0000000 , 0.0000000,
 @ u238 @ ${f6(el["tv_2"]["u238"])}, ${f6(el["tv_2"]["u238"])}, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000,
 @ o    @ ${f6(el["tv_2"]["o"])}, ${f6(el["tv_2"]["o"])}, 0.0000000, 0.0000000, ${f6(el["SVP"]["o"])}, 0.0000000, 0.0000000,
 @ n    @ 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000,
 @ *c*  @ 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000,
 @ al   @ 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000,
 @ b-10 @ 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, ${f6(el["PEL"]["b-10"])}, 0.0000000,
 @ c    @ 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, ${f6(el["PEL"]["c"])}, 0.0000000,
 @ gd55 @ 0.0000000, 0.0000000, 0.0000000, 0.0000000, ${f6(el["SVP"]["gd55"])}, 0.0000000, 0.0000000,
 @ gd57 @ 0.0000000, 0.0000000, 0.0000000, 0.0000000, ${f6(el["SVP"]["gd57"])}, 0.0000000, 0.0000000,
 @ ni   @ 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, ${f6(el["ob_pel"]["ni"])},
 @ cr   @ 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, ${f6(el["ob_pel"]["cr"])},
 @ pu39 @ 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000,
 @ pu40 @ 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000,
 @ pu41 @ 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000,
@-----------------------------------------------------------------------
 ntcell=1,2,3,4,5,6,
 krat= 51,18,9,6,7,36,
 alout=
       0.5882 , 0.1372 , 0.1176 , 0.0000 , 0.0000 , 0.1586,
       0.3888 , 0.3333 , 0.0000 , 0.2222 , 0.0555 , 0.0000,
       0.6666 , 0.0000 , 0.0000 , 0.0000 , 0.0000 , 0.3333,
       0.0000 , 0.6666 , 0.0000 , 0.0000 , 0.3333 , 0.0000,
       0.0000 , 0.1428 , 0.0000 , 0.2857 , 0.5714 , 0.0000,
       0.2222 , 0.0000 , 0.0833 , 0.0000 , 0.0000 , 0.6944,

 material(1)='chmc',
 &end
*h*
*o*
zr
u235
u238
o
n
*c*
al
b-10
c
gd55
gd57
ni
cr
pu39
pu40
pu41
****
:corr
 &vvod &end
:fier
 &vvod &end
:macro
 &vvod
  ET=10.5E+6,2.15,2.15,0.,
 @NBV=1,1,1,1,1,1,1,1,1,1,1,1,1,1,
 &end
:stop