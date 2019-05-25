# start up by cleaning the list of suffixes
OPT = -c -fast

.SUFFIXES:
# default rule is given for .f .f90 and .obj
.SUFFIXES: .f .obj 
# default rule to make .obj file

.f.obj:; df $(OPT) $(SRC)$*.f 

LIBSKETCH = ..\Library\Steam_Table.lib
LIBLINKER = link
LIBFLAGS = -lib

LIBOBJ   = SIGMA.obj      BLKDAT.obj   FPROP.obj \
           SATPRS.obj     SATTMP.obj             \
           THERMO.obj     CPLL.obj     HEV.obj   \
           PF1STEAM.obj   SETEOS.obj             \
           STEAM_SUB.obj  CPVV1.obj	  RHOLIQ.obj \
           VISCL.obj                             \
           SATDER.obj    VISCV.obj    THCL.obj   \
           THCV.obj     

$(LIBSKETCH):$(LIBOBJ)
	@echo "Linking  <$(LIBRARY)> ..... "
	@$(LIBLINKER) $(LIBFLAGS) $(LIBOBJ) /out:$(LIBSKETCH)
	@echo "done" 

# Libarry Source Files

SIGMA.obj:     $*.f
BLKDAT.obj:    $*.f  
FPROP.obj:     $*.f
SATPRS.obj:    $*.f
SATTMP.obj:    $*.f
THERMO.obj:    $*.f
CPLL.obj:      $*.f
HEV.obj:       $*.f
PF1STEAM.obj:  $*.f
SETEOS.obj:    $*.f
STEAM_SUB.obj: $*.f
CPVV1.obj:     $*.f
RHOLIQ.obj:    $*.f
VISCL.obj:     $*.f
SATDER.obj:    $*.f
VISCV.obj:     $*.f
THCL.obj:      $*.f
THCV.obj:      $*.f

# DO NOT DELETE
