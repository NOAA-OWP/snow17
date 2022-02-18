#========================================================================
# Makefile to compile the Snow-17 and driver code for multi-zone model
#========================================================================

#========================================================================
# PART 0: Define directory paths and compiler

# Define main/top directory (parent of /build/)
# this relative path drops into snow17 parent directory
F_MASTER_DIR =..

# Location of the compiled modules
MOD_PATH = $(F_MASTER_DIR)/build/

# Define the executable and path
EXE = $(F_MASTER_DIR)/bin/snow17.exe

# Define your Fortran compiler
#FC  = pgf90
#FC  = ifort
FC   = gfortran

#========================================================================
# PART 1: Assemble all of the various sub-routines

# Define directories
DRIVER_DIR = $(F_MASTER_DIR)/src/driver/
snow19_dir = $(F_MASTER_DIR)/src/snow19/
share_dir  = $(F_MASTER_DIR)/src/share/

# share utilities
model_UTIL= \
		nrtype.f90 \
		constants.f90 \
		def_namelists.f90 \
		interfaces.f90 \
		read_namelist.f90 \
		ascii_io.f90 \
		day_of_year.f90 \
		sfc_pressure.f90 
UTIL = $(patsubst %, $(share_dir)/%, $(model_UTIL))

# snow-19 code (fortran 90, different options)
model_snow19= \
		zero19.f \
		rout19.f \
		aesc19.f \
		melt19.f \
		SNEW.f \
		SNOWT.f \
		SNOWPACK.f \
		adjc19.f \
		aeco19.f \
		updt19.f \
		SNDEPTH.f \
		PACK19.f \
		exsnow19.f
snow19 = $(patsubst %, $(snow19_dir)/%, $(model_snow19))

# ... and stitch it all together...
utilities   = $(UTIL)
model = $(snow19)

#========================================================================
# PART 2: Define the libraries, driver programs, and executables

# Define the libraries and path to include files
ifeq "$(FC)" "pgf90"
 LOCL_PATH = /usr/local
 LIB = -L$(LOCL_PATH)/lib 
 INC = -I ./
endif

ifeq "$(FC)" "gfortran"
 LOCL_PATH = /usr/local
 LIB = -L$(LOCL_PATH)/lib 
 INC = -I ../
endif

# Define the driver routine
model_DRIVER = multi_driver.f90

DRIVER = $(patsubst %, $(DRIVER_DIR)/%, $(model_DRIVER))

#========================================================================
# PART 3: Compile the puppy

# Define flags
ifeq "$(FC)" "gfortran"
  FLAGS_DEBUG = -static -Wall -g -ffree-line-length-none
  FLAGS = -O3 -fdefault-real-8 -fno-align-commons -ffree-line-length-none
  FLAGS2 = -O3 -fdefault-real-8 -fno-align-commons -ffree-line-length-none
endif

ifeq "$(FC)" "ifort"
  FLAGS_PROF = -static -debug -warn all -check all -FR -O0 -auto -WB -traceback -g -fltconsistency -fpe0
  FLAGS = -O3 -warn all -check all
  FLAGS2 = -O3 
  FLAGS77 = -O3 -f77rtl 
endif

ifeq "$(FC)" "pgf90"
  FLAGS_PROF = -Bstatic -Mbackslash -g -Mchkptr -Mchkstk -Mpgicoff -Minform=inform -Ktrap=divz,inv -Mprof=lines,time
  # FLAGS = -Mfreeform -O3 -Mbackslash -g -Mchkptr -Mchkstk -Mpgicoff -Minform=inform -Ktrap=divz,inv
  FLAGS = -O3 -r8 -Kieee
  FLAGS2 = -O3 -r8
endif

# ----- Compile -----

all: model link clean 
debug: model_debug link 

check:
	echo test
	echo $(FC)

# compile model code
model:
	$(FC) $(FLAGS2) -c $(model) $(INC)
	$(FC) $(FLAGS) -c $(utilities) $(DRIVER) $(INC)

model_debug:
	$(FC) $(FLAGS2) -g -fcheck=all -c $(model) $(INC)
	$(FC) $(FLAGS) -g -fcheck=all -c $(utilities) $(DRIVER) $(INC)

# link routines
link:
#	$(FC) -fPIC -Bstatic_pgi -rpath,/usr/local/netcdf4-pgi/lib *.o -I./ $(LIBNETCDF) -o $(EXE)
	$(FC) -fPIC -I./ $(LIB) -o $(EXE) *.o 

# Remove object files
clean:
	rm -f *.o
	rm -f *.mod
