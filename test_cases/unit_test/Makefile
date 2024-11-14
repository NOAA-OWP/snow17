# Makefile
#
.SUFFIXES:
.SUFFIXES: .o .f90

F90   = gfortran
F_MASTER_DIR = ../../
DATETIME_FORTRAN_INSTALLATION_DIR=$(F_MASTER_DIR)/../../extern
fdatetime_dir = $(DATETIME_FORTRAN_INSTALLATION_DIR)/datetime-fortran-1.7.0/src/

FREESOURCE     =       -ffree-form  -ffree-line-length-none
#F90FLAGS       =   -O3 -g -fbacktrace -Wall -fcheck=all -fdefault-real-8 -fno-align-commons -ffree-line-length-none -cpp
#F90FLAGS = -O3 -g -fbacktrace -Wall -fdefault-real-8 -fno-align-commons -ffree-line-length-none -cpp $(DATETIME_F_CFLAGS)
F90FLAGS = -O3 -g -fbacktrace -Wall -fno-align-commons -ffree-line-length-none -cpp 
CPP            =       cpp
CPPFLAGS       =       -P -traditional -D_GFORTRAN_

# --- snow-19 code (fortran 90, different options)
snow19_objs = \
		zero19.o \
		rout19.o \
		aesc19.o \
		melt19.o \
		SNEW.o \
		SNOWT.o \
		SNOWPACK.o \
		adjc19.o \
		aeco19.o \
		updt19.o \
		SNDEPTH.o \
		PACK19.o \
		exsnow19.o

# -- share utilities and code used by the driver in running the model
run_util_objs = \
	nrtype.o \
	snowLogger.o \
	constants.o \
	namelistModule.o \
        parametersType.o \
        forcingType.o \
        dateTimeUtilsModule.o \
        runInfoType.o \
        modelVarType.o \
        ioModule.o \
        runSnow17.o 

bmi_functions_objs = bmi.o \
		bmi_snow17.o

fdatetime_obj = datetime_module.o

CMD = snow17_driver_test.exe

#all:	$(CMD)
$(CMD): $(bmi_functions_objs) $(snow19_objs) $(run_util_objs) $(fdatetime_obj) \
      	snow17_driver_test.o
	$(F90) -fPIC -o $(@) -I./ snow17_driver_test.o \
		$(bmi_functions_objs) $(snow19_objs) $(run_util_objs) $(fdatetime_obj)

$(bmi_functions_objs): $(run_util_objs)

$(snow19_objs): $(run_util_objs)

snow17_driver_test.o: snow17_driver_test.f90
	$(F90) -c $(F90FLAGS) $(FREESOURCE) -I./ snow17_driver_test.f90 

$(bmi_functions_objs): %.o: ../../src/bmi/%.f90
	$(F90) -c $(F90FLAGS) $(FREESOURCE) -I./ $< 

$(run_util_objs): %.o: ../../src/share/%.f90 $(fdatetime_obj)
	$(F90) -c $(F90FLAGS) $(FREESOURCE) -I./ $<

$(snow19_objs): %.o: ../../src/snow19/%.f 
	$(F90) -c $(F90FLAGS) -I./ $<

$(fdatetime_obj): $(fdatetime_dir)/datetime_module.f90
	$(F90) -c $(F90FLAGS) -I./ $(fdatetime_dir)/datetime_module.f90

test: $(CMD)
	cd ../../test_cases; tar -zxvf ex1.tgz; \
        cd ex1/run; \
	../../../test_cases/unit_test/snow17_driver_test.exe ./namelist.bmi.HHWM8

# This command cleans up
clean:
	rm -f *.mod *.o $(CMD)

