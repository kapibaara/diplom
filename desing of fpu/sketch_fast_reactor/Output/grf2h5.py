#!/bin/env/python
# -*- coding:utf-8 -*-

import struct
import numpy as np
from contextlib import contextmanager
import h5py
import argparse
import os

def fbread(f, fmt):
    u"чтение двоичного файла по заданной строке формата fmt фрмат из struct"
    buf=f.read(4)
    prefix=struct.unpack("i", buf)[0]
    l = struct.calcsize(fmt)
    buf = f.read(l)
    tpl=struct.unpack(fmt, buf)
    buf=f.read(4)
    postfix=struct.unpack("i", buf)[0]
    assert prefix == postfix == l
    return tpl

def bread(f, fmt):
    u"чтение двоичного файла по заданной строке формата fmt фрмат из struct"
    l = struct.calcsize(fmt)
    buf = f.read(l)
    return struct.unpack(fmt, buf)

def faread(f,dtype,count):
    buf=f.read(4)
    prefix=struct.unpack("i", buf)[0]
    res=np.fromfile(f,dtype=dtype,count=count)
    buf=f.read(4)
    postfix=struct.unpack("i", buf)[0]
    assert prefix == postfix
    return res

@contextmanager
def fblock(f):
    buf=f.read(4)
    prefix=struct.unpack("i", buf)[0]
    yield
    buf=f.read(4)
    postfix=struct.unpack("i", buf)[0]
    assert prefix == postfix

def is_numb(v):
    tt=type(v)
    return (tt is np.ndarray) or (tt is int) or (tt is float) or (tt is list)

def clear_dict(dkt):
    dsk=dict()
    dsk.update(dkt)
    trm=[i for i in dsk if not is_numb(dsk[i]) ]
    for i in trm:
        del dsk[i]
    return dsk


def head(f,l=20):
    p=f.tell()
    s=f.read(l)
    f.seek(p)
    print s
    return s

fnm="SKETCH1.grf"

def grf2h5(fnm,fout):
    f=open(fnm,"rb")
    #f.seek(0)
    lhdr,=fbread(f,'i')
    hdr = [fbread(f,"80s")[0].strip() for i in range(lhdr)]
    lhdr,=fbread(f,'i')
    problem_title = [fbread(f,"80s")[0].strip() for i in range(lhdr)]
    GMT_CRD_TYPE, = fbread(f,'4s')
    N_POLY, NH, NZR, NZ, NXR, NYR, NX, NY, NCHM, NDD,N_BUNDLE_TYPE = fbread(f,"i"*11)
    NP_Reactor_Core, NZR_Core = fbread(f,"ii")
    NNODE, NG, MD, N_FEEDBACK = fbread(f,"iiii")
    NN_CRod, NN_CRod_Comp, NN_CRod_El, NN_CRod_Type, NN_Crod_Bundle = fbread(f,"i"*5)
    NN_FRD_FUEL, NN_FRD_CLAD, NN_FRD_TOTAL = fbread(f,"iii")
    NN_RT_HC_TRAC, NN_RT_FD_TRAC, NN_Z_HC_TRAC, NN_Z_FD_TRAC = fbread(f,"iiii")
    REACTOR_TYPE, = fbread(f,"3s")
    lhdr, = fbread(f,"i")
    name_st_scal = [fbread(f,"80s")[0].strip() for i in range(lhdr)]

    lhdr, = fbread(f,"i")
    dim_st_vec=[]
    name_st_vec=[]
    for i in range(lhdr):
        dim_st_vec.append(fbread(f,"i")[0])
        name_st_vec.append(fbread(f,"80s")[0].strip())


    lhdr, = fbread(f,"i")
    name_st_dist = [fbread(f,"80s")[0].strip() for i in range(lhdr)]
    flag_reactor_st_dist = [fbread(f,"I")[0]!=0 for i in range(lhdr)] #!!! logical

    lhdr, = fbread(f,"i")
    N_TR_SCAL = lhdr
    Name_TR_SCAL = [fbread(f,"80s")[0].strip() for i in range(lhdr)]

    lhdr, = fbread(f,"i")
    dim_TR_vec=[]
    name_TR_vec=[]
    N_TR_VEC = lhdr
    for i in range(lhdr):
        dim_TR_vec.append(fbread(f,"i")[0])
        name_TR_vec.append(fbread(f,"80s")[0].strip())

    lhdr, = fbread(f,"i")
    name_tr_dist = [fbread(f,"80s")[0].strip() for i in range(lhdr)]
    flag_reactor_TR_dist = [fbread(f,"I")[0]!=0 for i in range(lhdr)] #!!! logical

    # Read_Model_Description
    Numb_Reactor_Core = faread(f,'i',NP_Reactor_Core)
    N_Coord = faread(f,'i',N_POLY*2).reshape((2,N_POLY))

    with fblock(f):
        npoly = np.fromfile(f,dtype='i',count=NYR*NXR).reshape((NXR,NYR))
        Nxr_B_Reactor = np.fromfile(f,dtype='i',count=NYR)
        Nxr_E_Reactor = np.fromfile(f,dtype='i',count=NYR)
        Nyr_B_Reactor,Nyr_E_Reactor,Nxr_Max_Reactor,Nxr_B_Min_Reactor = bread(f,"iiii")



    with fblock(f):
        Index_Core = np.fromfile(f,dtype='i',count=NYR*NXR).reshape((NXR,NYR))
        Nxr_B_Core = np.fromfile(f,dtype='i',count=NYR)
        Nxr_E_Core = np.fromfile(f,dtype='i',count=NYR)
        Nyr_B_Core, Nyr_E_Core, Nxr_Max_Core, Nxr_B_Min_Core, NZR_Core_Beg, NZR_Core_End  = bread(f,"i"*6)

    with fblock(f):
        hx = np.fromfile(f,dtype='f',count=NXR)
        hy = np.fromfile(f,dtype='f',count=NYR)
        hz = np.fromfile(f,dtype='f',count=NZR)

    with fblock(f):
        npz = np.fromfile(f,dtype='i',count=NZR)
        hzt = np.fromfile(f,dtype='f',count=NZ)

    with fblock(f):
        v_reactor, v_core = bread(f,"ff")
        vol_ass = np.fromfile(f,dtype='f',count=N_POLY*NZR).reshape((NZR,N_POLY))

    with fblock(f):
        Mat_Com_Rod = np.fromfile(f,dtype='f',count=NN_CRod*NN_CRod_El).reshape((NN_CRod_El,NN_CRod))
        nrods = np.fromfile(f,dtype='i',count=NN_CRod*NN_Crod_Bundle).reshape((NN_Crod_Bundle,NN_CRod))
        h_rod_el = np.fromfile(f,dtype='f',count=NN_CRod_El)

    dkt=clear_dict(locals())

    f5=h5py.File(fout,"w")
    g=f5.create_group("dsk")

    for i in dkt:
        g[i]=np.array(dkt[i])

    if name_st_dist:
        g=f5.create_group("st")
        for i in name_st_dist:
            g[i] = faread(f,'f',N_POLY*NZR).reshape((NZR,N_POLY))

    gtim = f5.create_group("dyn")

    for timepoint in xrange(1000000):
        try:
            tname="{:06d}".format(timepoint)
            g=gtim.create_group(tname)
            time, = fbread(f,"f")
            g.attrs["t"]=time
            scal_tr = faread(f,'f',N_TR_SCAL)
            g["scal_tr"]=scal_tr
            gg=g.create_group("v")
            for nm,siz in zip(name_TR_vec,dim_TR_vec):
                gg[nm]=faread(f,'f',siz)

            gg=g.create_group("f")
            for nm in name_tr_dist:
                gg[nm]=faread(f,'f',N_POLY*NZR).reshape((NZR,N_POLY))
        except:
            break

    f5.close()
    f.close()

if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        prog="grf2h5", description="convert grf scetch file to hdf5 format")
    parser.add_argument("-o", "--output", default="{}.h5") # section
    parser.add_argument("file", help=u"input data") #avrinp
    args = parser.parse_args()
    head=os.path.splitext(args.file)[0]
    ofile=args.output.format(head)
    grf2h5(args.file,ofile)
