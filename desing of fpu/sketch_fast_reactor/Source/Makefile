# 
# start up by cleaning the list of suffixes
.SUFFIXES:
# default rule is given for .f .f90 and .obj
.SUFFIXES: .f .f90 .obj 

Depend    = ../Include/sketch.fh    ../Include/parameters.fh ../Include/units.fh
Depend_TH = ../Include/Fuel_Rod.fh  ../Include/TH_Model.fh
Depend_RG = ../Include/cd_file.fh  

OBJ   = precision.obj                 \
        termsort_lib.obj              \
        Homog_XS.obj                  \
        ADJoint.obj                   \
        burnup.obj                    \
        burnup_step_control.obj       \
        CGPrecond.obj                 \
        CGSolvers.obj                 \
        CHeByshev.obj                 \
        CoNTrol.obj                   \
        CR_pos_sub.obj                \
        Control_RoD.obj               \
        Core_Map.obj                  \
        CPU_Timers.obj                \
        EIgenSystem.obj               \
        Fuel_RoD.obj                  \
        GeoMeTry_Bound.obj            \
        GeoMeTry_Faces.obj            \
        GeoMeTry_Triangle.obj         \
        GeoMeTry.obj                  \
        GraPhics.obj                  \
        HOM_Surface_Flux.obj          \
        read_history.obj              \
        INPut.obj                     \
        KINetics.obj                  \
        MAIN.obj                      \
        MATrix_Block.obj              \
        MATrix_Block_Triang.obj       \
        MATrix_FD.obj                 \
        MATrix_Nodal.obj              \
        MATrix_Nodal_ANM.obj          \
        MATrix_Nodal_ANM_NG.obj       \
        MiSCellaneous.obj             \
        MSC_Norms.obj                 \
        MSC_SPARSKIT.obj              \
        OUTput.obj                    \
        PoiNT_Kinetics.obj            \
        POWer.obj                     \
        PreConDitioners.obj           \
        PVM_Mapping.obj               \
        RingalsDiStributions.obj      \
        TH_Model.obj                  \
        Time_Step_Control.obj         \
        TRansverseLeakage.obj         \
        Xe_Sm_Model.obj               \
        XSections.obj                 \
        XS_LIN_TABLE.obj              \
        XSPolynom.obj                 \
	XS_BN800_SUB.obj              \
        XSRingals.obj
OBJ_NO_PVM =   PVM_no_sketch.obj             
OBJ_PVM    = PVM_sketch.obj           
OBJ_POSTPROC = PostProc_MOD.obj PostProc_NT.obj 
 
PROGRAM     = ..\SKETCH.exe
PROGRAM_PVM = ..\SKETCH_PVM.exe
POSTPROC    = ..\POSTPROC.exe
LIBSKETCH   =   ..\Library\Steam_Table.lib  
LIBIMSL     =   imsl.lib imsls_err.lib imslmpistub.lib 
DIRLIBSRC   =   ..\Lib_Source
LDFLAGS = /link /incremental:no 
LINKER  = DF
COMPILATOR = DF

#OPT     = -c /check:all /debug:full  /traceback /warn:unused /warn:argument_checking /warn:declarations /check:nounderflow /assume:byterecl

OPT = -c -fast /assume:byterecl
F90_FIXED = $(COMPILATOR) $(OPT) -fixed

LIB_PVM_FLAGS = -L$(PVM_ROOT)/lib/SUN4SOL2
LIB_PVM   = -lfpvm3 -lpvm3 -lnsl -lsocket

# default rule to make .obj file from .f
.f.obj:; df $(OPT) $(SRC)$*.f 

# default rule to make .obj file from .f90
.f90.obj:; df $(OPT) $(SRC)$*.f90 

$(PROGRAM):$(OBJ) $(OBJ_NO_PVM) $(LIBSKETCH) 
        @echo "Linking  <$(PROGRAM)> ..... "
        @$(LINKER)  $(LDFLAGS)  $(OBJ) $(OBJ_NO_PVM) $(LIBIMSL) $(LIBSKETCH)   /out:$(PROGRAM)
        @echo "done" 
$(POSTPROC):$(OBJ_POSTPROC) 
        @echo "Linking  <$(POSTPROC)> ..... "
        @$(LINKER)  $(LDFLAGS)  $(OBJ_POSTPROC)  /out:$(POSTPROC)
        @echo "done" 
$(PROGRAM_PVM):$(OBJ) $(OBJ_PVM) $(LIBSKETCH) 
        @echo "Linking  <$(PROGRAM_PVM)> ..... "
        @$(LINKER)  $(LDFLAGS)  $(OBJ) $(OBJ_PVM) $(LIBIMSL) $(LIB_PVM_FLAGS) $(LIB_PVM) $(LIBSKETCH) /out:$(PROGRAM_PVM)
        @echo "done" 
$(LIBSKETCH):
        @echo "Making  <$(LIBSKETCH)> ..... "
        @cd $(DIRLIBSRC)
    @cd ..\Lib_Source
    @nmake -f Makefile_LIB_NT
    @cd ..\Source
        @echo "done" 
all:$(PROGRAM) $(POSTPROC)
clean: 
    @echo "Removing ..... "
        @del $(OBJ) $(PROGRAM) $(PROGRAM_PVM) $(POSTPROC)
        @echo "done"

# Source Files for SKETCH:

ADJoint.obj:          $(SRC)$*.f $(Depend)
burnup.obj:           $(SRC)$*.f $(Depend) 
burnup_step_control.obj:  $(SRC)$*.f $(Depend) 
CGPrecond.obj:        $(SRC)$*.f $(Depend)
CGSolvers.obj:        $(SRC)$*.f $(Depend)
CHeByshev.obj:        $(SRC)$*.f $(Depend)
CoNTrol.obj:          $(SRC)$*.f $(Depend)
Control_RoD.obj:      $(SRC)$*.f $(Depend)
Core_Map.obj:   $(SRC)$*.f  $(Depend)
 $(F90_FIXED)  $(SRC)$*.f   
CPU_Timers.obj:       $(SRC)$*.f $(Depend)
CR_pos_sub.obj:       $(SRC)$*.f $(Depend)
EIgenSystem.obj:      $(SRC)$*.f $(Depend)
Fuel_RoD.obj:         $(SRC)$*.f $(Depend) $(Depend_TH)
GeomeTRy_Bound.obj:   $(SRC)$*.f90  $(Depend)
 $(F90_FIXED)  $(SRC)$*.f90   
GeomeTRy_Faces.obj:   $(SRC)$*.f90  $(Depend)
 $(F90_FIXED)  $(SRC)$*.f90   
GeoMeTry_Triangle.obj:   $(SRC)$*.f90  $(Depend)
 $(F90_FIXED)  $(SRC)$*.f90   
GeomeTRy.obj:         $(SRC)$*.f $(Depend)
GRaPhics.obj:         $(SRC)$*.f $(Depend)
HOM_Surface_Flux.obj: $(SRC)$*.f  $(Depend)
 $(F90_FIXED)  $(SRC)$*.f   
Homog_XS.obj: $(SRC)$*.f  $(Depend)
 $(F90_FIXED)  $(SRC)$*.f   
INPut.obj:            $(SRC)$*.f $(Depend)
KINetics.obj:         $(SRC)$*.f $(Depend)
MAIN.obj:             $(SRC)$*.f $(Depend)
MATrix_Block.obj:         $(SRC)$*.f $(Depend)
MATrix_Block_Triang.obj:         $(SRC)$*.f $(Depend)
MATrix_FD.obj:            $(SRC)$*.f $(Depend)
MATrix_Nodal.obj:     $(SRC)$*.f $(Depend)
MATrix_Nodal_ANM.obj: $(SRC)$*.f $(Depend)
MATrix_Nodal_ANM_NG.obj: $(SRC)$*.f $(Depend)
MiSCellaneous.obj:    $(SRC)$*.f $(Depend)
MSC_Norms.obj:        $(SRC)$*.f $(Depend)
MSC_SPARSKIT.obj:     $(SRC)$*.f $(Depend)
OUTput.obj:           $(SRC)$*.f $(Depend)
PoiNT_Kinetics.obj:   $(SRC)$*.f $(Depend)
POWer.obj:            $(SRC)$*.f $(Depend)
PreConDitioners.obj:  $(SRC)$*.f $(Depend)
PVM_Mapping.obj:      $(SRC)$*.f $(Depend)
PVM_no_sketch.obj:    $(SRC)$*.f $(Depend)
read_pos_sub.obj:     $(SRC)$*.f $(Depend)
RingalsDiStributions.obj: $(SRC)$*.f $(Depend)
TH_Model.obj:         $(SRC)$*.f $(Depend) $(Depend_TH)
Time_Step_Control.obj:$(SRC)$*.f $(Depend)
TRansverseLeakage.obj:$(SRC)$*.f $(Depend)
Xe_Sm_Model.obj      :$(SRC)$*.f $(Depend)
XS_LIN_TABLE.obj     :$(SRC)$*.f $(Depend)
XSections.obj:        $(SRC)$*.f $(Depend)
XSPolynom.obj:        $(SRC)$*.f $(Depend)
XS_BN800_SUB.obj:     $(SRC)$*.f $(Depend)
XSRingals.obj:        $(SRC)$*.f $(Depend) $(Depend_RG)
precision.obj:        $(SRC)$*.f90
termsort_lib.obj:     $(SRC)$*.f90

# Source Files for SKETCH_PVM:

PVM_sketch.obj:       $(SRC)$*.f $(Depend)    

# Source Files for POSPROC:

PostProc_MOD.obj:     $(SRC)$*.f90
PostProc_NT.obj:      $(SRC)$*.f90

# DO NOT DELETE





