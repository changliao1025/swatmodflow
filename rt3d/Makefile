# Purpose: 
#    Compiles SWAT, MODFLOW, RT3D, and SMRT (linkage files) with gfortran
# 
# Author: Andre Dozier
#   Date: May 10, 2013 
# 
# 

.SUFFIXES: 
.SUFFIXES: .f .f90 .f03 .o .mod

ifeq ($(OS),win)

# windows 
MF.DIR = ..\modflow\\
SWATDIR = ..\swat\\
RM = del /F
RT3D.OUT = rt3d.exe
RT3D.DLL = librt3d.dll
SWAT.DLL = libswat.dll
FFLAGS = -fno-underscoring
EVENT = ..\event\\
# CP = copy /y $(SWATDIR)$(SWAT.DLL) $(SWAT.DLL) 

else

# linux 
MF.DIR = ../modflow/
SWATDIR = ../swat/
RM = rm -f
RT3D.OUT = rt3d
RT3D.DLL = librt3d.so
SWAT.DLL = libswat.so
FFLAGS = -fno-underscoring -fPIC
EVENT := ../event/
# CP = cp -f $(SWATDIR)$(SWAT.DLL) $(SWAT.DLL) 

endif

FC = gfortran
OBJ = $(patsubst %.f,%.o, $(filter-out modparm.f, $(wildcard *.f))) $(patsubst %.f90,%.o, $(wildcard *.f90)) $(patsubst %.f03,%.o, $(wildcard *.f03))
RTMOD = vst_arrays.mod rt_mf_link.mod rt_global.mod

#Compile SWAT commands (first modules, then the rest of the code and linkage
all: rt3d_dll

rt3d: $(RTMOD) $(OBJ)
	$(CP)
	$(FC) $(OBJ) $(FFLAGS) -o $(RT3D.OUT)

rt3d_dll: $(RTMOD) $(OBJ) 
	$(CP)
	$(FC) $(OBJ) $(FFLAGS) -shared -o $(RT3D.DLL)

#Clean up for either linux for Windows
clean: 
	$(RM) *.o *.mod *.exe *.dll *.so $(RT3D.OUT)

#Compile all object files
.f.o:
	$(FC) $(FFLAGS) -c $<
.f90.o: 
	$(FC) $(FFLAGS) -c $<
.f03.o: 
	$(FC) $(FFLAGS) -c $<

#Compile all modules
vst_arrays.mod rt_mf_link.mod rt_global.mod: rt_modparm.f
	$(FC) $(FFLAGS) -c $<