
from mc2py.re_iter import ReIter,is_float,is_int

def extract_sketch_flux_3d(file):
    itr=ReIter("power.lst")
    itr.search(r"3D Distribution +:")
    itr.search("CHAN")
    index=itr.array()
    itr.endl()
    itr.endl()
    arr=itr.array(skip=r"[ \n:]+")
    n=len(index)
    newarr=arr.reshape((-1,n+1))[:,1:]
    return newarr

def test():
    file="power.lst"
    fi=extract_sketch_flux_3d(file)
    assert((540,6)==newarr.shape)

