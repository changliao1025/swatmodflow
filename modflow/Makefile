# Purpose: 
#	Compiles MODFLOW
# 
# Author: Andre Dozier
#   Date: May 10, 2013 
# 
# 

.SUFFIXES: 
.SUFFIXES: .f .f90 .f03 .o .mod

SYS := $(shell gcc -dumpmachine) 
ifneq (, $(findstring mingw, $(SYS)))
OS := win
endif

# if not release, set the debug flag
ifneq ($(rel),true)
deb := -g
endif

ifeq ($(OS),win)

# windows 
RM = del /F
MF.OUT = modflow.exe
MF.DLL = libmodflow.dll
SWAT.DLL = libswat.dll
EVENT.DIR = ..\event\\
EVENT.DLL = libevent.dll
LIB.DIR = ..\..\lib\\
BIN.DIR = ..\..\bin\\
FFLAGS = -fno-underscoring $(deb) -cpp
CP = copy /y

else

# linux 
RM = rm -f
MF.OUT = modflow
MF.DLL = libmodflow.so
SWAT.DLL = libswat.so
EVENT.DIR := ../event/
EVENT.DLL = libevent.so
LIB.DIR = ../../lib/
BIN.DIR = ../../bin/
FFLAGS = -fno-underscoring -fPIC $(deb) -cpp
CP = cp -f

endif

OPT = -O3
FC = gfortran $(FFLAGS)
OBJ = $(patsubst %.f,%.o, $(filter-out modparm.f, $(wildcard *.f))) $(patsubst %.f90,%.o, $(wildcard *.f90)) $(patsubst %.f03,%.o, $(wildcard *.f03))
SWAT.OUT = swat.exe
MFMOD = global.mod gwfnwtmodule.mod gwfstrmodule.mod gwfsfrmodule.mod \
	gwfibsmodule.mod sipmodule.mod gwfsubmodule.mod gwfhufmodule.mod hydsfrmodule.mod \
	gwfuzfmodule.mod gwflpfmodule.mod gwfupwmodule.mod gwfbcfmodule.mod\
	gwflakmodule.mod gwfchdmodule.mod gwfdrnmodule.mod \
	gwfdrtmodule.mod gwfetsmodule.mod gwfevtmodule.mod gwffhbmodule.mod \
	gwfgagmodule.mod gwfghbmodule.mod gwfhfbmodule.mod \
	hydbasmodule.mod hydibsmodule.mod hydsubmodule.mod hydstrmodule.mod \
	de4module.mod pcgmodule.mod gmgmodule.mod gwfmnw1module.mod \
	gwfmnw2module.mod gwfmnw2imodule.mod gwfrchmodule.mod gwfresmodule.mod \
	gwfrivmodule.mod gwfwelmodule.mod \
	machine_constants.mod \
	parammodule.mod gwfbasmodule.mod f_units.mod math_constants.mod \
	mesh.mod bc.mod fdeq.mod i_c.mod parameters.mod control.mod solver.mod \
	solver_gmres.mod source.mod units.mod variables.mod xmdcmn.mod xmdmodule.mod \
	xmdalloc.mod xmdmatrix.mod xmdSymFac.mod xmdSymFacDrop.mod \
	gmresmodule.mod ilupc_mod.mod lmtmodule.mod \
	obsbasmodule.mod obschdmodule.mod obsdrnmodule.mod obsghbmodule.mod \
	obsrivmodule.mod 
OTHERMODS = modevent.mod

#Compile SWAT commands (first modules, then the rest of the code and linkage
all: event modflow modflowdll

event: 
	$(MAKE) -C $(EVENT.DIR) 
	$(CP) $(EVENT.DIR)$(EVENT.DLL) .
	$(CP) $(EVENT.DIR)*.mod .

modflow: $(OTHERMODS) $(MFMOD) $(OBJ)
	$(FC) $(OPT) $(OBJ) $(EVENT.DLL) -o $(MF.OUT)
	$(CP) $(MF.OUT) $(BIN.DIR)
	$(CP) *.mod $(LIB.DIR)
 
modflowdll: $(OTHERMODS) $(MFMOD) $(OBJ)
	$(FC) $(OPT) $(OBJ) $(EVENT.DLL) -shared -o $(MF.DLL)
	$(CP) $(MF.DLL) $(LIB.DIR)
	$(CP) *.mod $(LIB.DIR)

#Clean up for either linux for Windows
clean: 
	$(RM) *.o *.mod *.exe *.dll *.so $(MF.OUT)

#Compile all object files
.f.o:
	$(FC) $(FFLAGS) -c $<
.f90.o: 
	$(FC) $(FFLAGS) -c $<
.f03.o: 
	$(FC) $(FFLAGS) -c $<

#Compile all modules
gwfbcfmodule.mod: mf_gwf2bcf7.f
	$(FC) $(FFLAGS) -c $<
gwfchdmodule.mod: mf_gwf2chd7.f
	$(FC) $(FFLAGS) -c $<
gwfdrnmodule.mod: mf_gwf2drn7_NWT.f
	$(FC) $(FFLAGS) -c $<
gwfdrtmodule.mod: mf_gwf2drt7.f
	$(FC) $(FFLAGS) -c $<
gwfetsmodule.mod: mf_gwf2ets7.f
	$(FC) $(FFLAGS) -c $<
gwfevtmodule.mod: mf_gwf2evt7.f
	$(FC) $(FFLAGS) -c $<
gwffhbmodule.mod: mf_gwf2fhb7.f
	$(FC) $(FFLAGS) -c $<
gwfgagmodule.mod: mf_gwf2gag7.f
	$(FC) $(FFLAGS) -c $<
gwfghbmodule.mod: mf_gwf2ghb7_NWT.f
	$(FC) $(FFLAGS) -c $<
gwfhfbmodule.mod: mf_gwf2hfb7_NWT.f
	$(FC) $(FFLAGS) -c $<
gwfhufmodule.mod: mf_gwf2huf7.f
	$(FC) $(FFLAGS) -c $<
hydbasmodule.mod: mf_gwf2hydmod7.f
	$(FC) $(FFLAGS) -c $<
hydibsmodule.mod: mf_gwf2hydmod7.f
	$(FC) $(FFLAGS) -c $<
hydsubmodule.mod: mf_gwf2hydmod7.f
	$(FC) $(FFLAGS) -c $<
hydstrmodule.mod: mf_gwf2hydmod7.f
	$(FC) $(FFLAGS) -c $<
hydsfrmodule.mod: mf_gwf2hydmod7.f
	$(FC) $(FFLAGS) -c $<
gwfibsmodule.mod: mf_gwf2ibs7.f
	$(FC) $(FFLAGS) -c $<
gwflakmodule.mod: mf_gwf2lak7_NWT.f
	$(FC) $(FFLAGS) -c $<
gwflpfmodule.mod: mf_gwf2lpf7.f 
	$(FC) $(FFLAGS) -c $<
gwfmnw1module.mod: mf_gwf2mnw17_NWT.f
	$(FC) $(FFLAGS) -c $<
gwfmnw2module.mod: mf_gwf2mnw27_NWT.f
	$(FC) $(FFLAGS) -c $<
gwfmnw2imodule.mod: mf_gwf2mnw2i7.f
	$(FC) $(FFLAGS) -c $<
gwfrchmodule.mod: mf_gwf2rch7.f
	$(FC) $(FFLAGS) -c $<
gwfresmodule.mod: mf_gwf2res7.f
	$(FC) $(FFLAGS) -c $<
gwfrivmodule.mod: mf_gwf2riv7_NWT.f
	$(FC) $(FFLAGS) -c $<
gwfstrmodule.mod: mf_gwf2str7.f
	$(FC) $(FFLAGS) -c $<
gwfupwmodule.mod: mf_gwf2upw1.f
	$(FC) $(FFLAGS) -c $<
gwfwelmodule.mod: mf_gwf2wel7_NWT.f
	$(FC) $(FFLAGS) -c $<
gwfsfrmodule.mod: mf_gwfsfrmodule_NWT.f
	$(FC) $(FFLAGS) -c $<
gwfsubmodule.mod: mf_gwf2sub7.f
	$(FC) $(FFLAGS) -c $<
gwfuzfmodule.mod: mf_gwfuzfmodule_NWT.f
	$(FC) $(FFLAGS) -c $<
lmtmodule.mod: mf_lmt7_NWT.f
	$(FC) $(FFLAGS) -c $<
machine_constants.mod: mf_mach_mod.f90
	$(FC) $(FFLAGS) -c $<
de4module.mod: mf_de47_NWT.f
	$(FC) $(FFLAGS) -c $<
parammodule.mod gwfbasmodule.mod global.mod: mf_gwf2bas7_NWT.f
	$(FC) $(FFLAGS) -c $<
f_units.mod math_constants.mod mesh.mod bc.mod fdeq.mod i_c.mod parameters.mod control.mod solver.mod solver_gmres.mod source.mod units.mod variables.mod: mf_modules.f90
	$(FC) $(FFLAGS) -c $<
gmgmodule.mod: mf_nogmg.f
	$(FC) $(FFLAGS) -c $<
gmresmodule.mod: mf_NWT1_gmres.f90
	$(FC) $(FFLAGS) -c $<
ilupc_mod.mod: mf_NWT1_ilupc_mod.f90
	$(FC) $(FFLAGS) -c $<
gwfnwtmodule.mod: mf_NWT1_module.f
	$(FC) $(FFLAGS) -c $<
xmdmodule.mod: mf_NWT1_xmd.f
	$(FC) $(FFLAGS) -c $<
xmdalloc.mod: mf_NWT1_xmdlib.f
	$(FC) $(FFLAGS) -c $<
xmdcmn.mod: mf_NWT1_xmdlib.f
	$(FC) $(FFLAGS) -c $<
xmdmatrix.mod: mf_NWT1_xmdlib.f
	$(FC) $(FFLAGS) -c $<
xmdSymFac.mod: mf_NWT1_xmdlib.f
	$(FC) $(FFLAGS) -c $<
xmdSymFacDrop.mod: mf_NWT1_xmdlib.f
	$(FC) $(FFLAGS) -c $<
obsbasmodule.mod: mf_obs2bas7.f
	$(FC) $(FFLAGS) -c $<
obschdmodule.mod: mf_obs2chd7.f
	$(FC) $(FFLAGS) -c $<
obsdrnmodule.mod: mf_obs2drn7.f
	$(FC) $(FFLAGS) -c $<
obsghbmodule.mod: mf_obs2ghb7.f 
	$(FC) $(FFLAGS) -c $<
obsrivmodule.mod: mf_obs2riv7.f
	$(FC) $(FFLAGS) -c $<
pcgmodule.mod: mf_pcg7_NWT.f
	$(FC) $(FFLAGS) -c $<
sipmodule.mod: mf_sip7_NWT.f
	$(FC) $(FFLAGS) -c $<
