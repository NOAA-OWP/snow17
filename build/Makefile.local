#========================================================================
# Makefile to compile the Snow-17 and driver code for multi-zone model
#========================================================================

#========================================================================
# PART 0: Define directory paths

# Define main/top directory (parent of /build/)
F_MASTER_DIR = /glade/work/andywood/profdev/snow17/code/snow17/

# Location of the compiled modules
MOD_PATH = $(F_MASTER_DIR)/build/

# Define the executable and path
EXE = $(F_MASTER_DIR)/bin/snow17.exe

#========================================================================
# PART 1: Assemble all of the various sub-routines

# Define directories
DRIVER_DIR = $(F_MASTER_DIR)/src/driver/
snow19_dir = $(F_MASTER_DIR)/src/snow19/
share_dir  = $(F_MASTER_DIR)/src/share/

# share utilities
calib_UTIL= \
		nrtype.f90 \
		constants.f90 \
		def_namelists.f90 \
		interfaces.f90 \
		read_namelist.f90 \
		ascii_io.f90 \
		day_of_year.f90 \
		sfc_pressure.f90 
UTIL = $(patsubst %, $(share_dir)/%, $(calib_UTIL))

# snow-19 code (fortran 90, different options)
calib_snow19= \
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
snow19 = $(patsubst %, $(snow19_dir)/%, $(calib_snow19))

# ... and stitch it all together...
calib_pgf90   = $(UTIL)
calib_pgf90_2 = $(snow19)

#========================================================================
# PART 2: Define the libraries, driver programs, and executables

# Define the Fortran Compilers
#FC  = pgf90
#FC  = ifort
FC   = gfortran

# Define the libraries and path to include files
ifeq "$(FC)" "pgf90"
 LOCL_PATH = /usr/local
 LIB = -L$(LOCL_PATH)/lib 
 INC = -I ./
endif

# Define the driver routine
calib_DRIVER = multi_driver.f90

DRIVER = $(patsubst %, $(DRIVER_DIR)/%, $(calib_DRIVER))

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

# compile calibration code
model:
	$(FC) $(FLAGS2) -c $(calib_pgf90_2) $(INC)
	$(FC) $(FLAGS) -c $(calib_pgf90) $(DRIVER) $(INC)

model_debug:
	$(FC) $(FLAGS2) -g -fcheck=all -c $(calib_pgf90_2) $(INC)
	$(FC) $(FLAGS) -g -fcheck=all -c $(calib_pgf90) $(DRIVER) $(INC)

# link routines
link:
#	$(FC) -fPIC -Bstatic_pgi -rpath,/usr/local/netcdf4-pgi/lib *.o -I./ $(LIBNETCDF) -o $(EXE)
	$(FC) -fPIC -I./ $(LIB) -o $(EXE) *.o 

# Remove object files
clean:
	rm -f *.o
	rm -f *.mod
