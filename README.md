# OWP Snow17 Model Version for NextGen

**Description**:  

Snow17 is a snow accumulation and melt model that has been used by the National Weather Service since the late 1970s for operational streamflow 
forecasting.  It is a temperature-index model that was first described in Anderson (1973) before being incorporated into the NWS River Forecasting System (NWSRFS) and more recently the NWS Community Hydrologic Prediction System (CHPS).  The version of the Snow17 model maintained in this directory is formulated from original NWS FORTRAN code to be compatible with 
the [Next Generation Water Resources Modeling Framework](https://github.com/NOAA-OWP/ngen) (NextGen). 

  - **Technology stack**: The model code is written in Fortran with a driver that incorporates [Basic Model Interface](https://csdms.colorado.edu/wiki/BMI) (BMI) commands to enable Snow17 to be run in standalone mode or as a module within the NextGen framework (or other BMI-based systems). It has been compiled using GNU, PGI, and Intel compilers. 
  - **Status**: Beta (runs but has not been extensively tested; validates against the orginal repository version listed below, which has been extensively used)
  - **Links**:  This code was adapted from source code included in https://github.com/NCAR/NWS_hydro_models -- a version used in federally funded streamflow forecasting research at NCAR. The original source code for that effort was obtained from the NWS Office of Hydrology around 2013. The code has since been copied and adapted into other research repositories. 

## Dependencies
- Fortran compiler
- NextGen ISO C Fortran BMI library (optional)

## Running in Standalone
The following describes how to install the run snow17 as a standalone model.

Clone repository if necessary and change to the repo root directory:
```bash
git clone https://github.com/NOAA-OWP/snow17.git
cd snow17
```
Generate a CMake build system and directory (in the example below and those that follow, the directory is assumed to be `cmake_build` within the repo root):
```bash
# You can also include the '-DCMAKE_BUILD_TYPE=Debug' option if you want to build for debugging
cmake -B cmake_build -S .
```

Note that you may need or want to specify the Fortran compiler, done by supplying a value for the `FC` variable when you generate the build directory.  There are many reasons for that:  maybe you have multiple compilers installed, or maybe your compiler isn't installed to the standard system path or using a standard name CMake will recognize.

Regardless, if necessary just append `FC=<path_to_compiler>` to the rest of the command:

```bash
# Here we are manually telling CMake to use '/opt/local/bin/gfortran-mp-14' as the Fortran compiler, rather than 
# whatever compiler it would find on its own.
FC=/opt/local/bin/gfortran-mp-14 cmake -B cmake_build -S .
```

With the build directory generated, build the stand-alone executable:
```bash
cmake --build cmake_build --target snow17
```

You should now see the `cmake_build/snow17` stand-alone executable. 


## Running in Standalone

The following describes how to install the run the Sac-SMA as a standalone model.

Clone repository if necessary and change to the repo root directory:
```bash
git clone https://github.com/NOAA-OWP/sac-sma.git
cd sac-sma
```
Generate a CMake build system and directory (in the example below and those that follow, the directory is assumed to be `cmake_build` within the repo root):
```bash
# You can also include the '-DCMAKE_BUILD_TYPE=Debug' option if you want to build for debugging
cmake -B cmake_build -S .
```

Note that you may need or want to specify the Fortran compiler, done by supplying a value for the `FC` variable when you generate the build directory.  There are many reasons for that:  maybe you have multiple compilers installed, or maybe your compiler isn't installed to the standard system path or using a standard name CMake will recognize.

Regardless, if necessary just append `FC=<path_to_compiler>` to the rest of the command:

```bash
# Here we are manually telling CMake to use '/opt/local/bin/gfortran-mp-14' as the Fortran compiler, rather than 
# whatever compiler it would find on its own.
FC=/opt/local/bin/gfortran-mp-14 cmake -B cmake_build -S .
```

With the build directory generated, build the stand-alone executable:
```bash
cmake --build cmake_build --target snow17
```

There is a test case in the `snow17/test_cases/` directory.  To run it, first cd to the directory (`cd ../test_cases/`) and unpack the example:  `tar -xzvf ex1.tgz`.
`cd ex1/run/` to get to the run directory and then execute the shell script (`./runSnow17.csh`) or type `../../../cmake_build/snow17 namelist.bmi.HHWM8` into your command line
Check the example in the output folder (`cd ../output/`)


## Running in [Ngen](https://github.com/NOAA-OWP/ngen)

The following are instructions for building the Sac-SMA BMI module shared library.  In particular, this is necessary to run the Sac-SMA model in the [Next Generation Water Resources Modeling Framework](https://github.com/NOAA-OWP/ngen).


As described above for stand-alone builds, clone the repo if necessary and change into the repo root directory.
```bash
git clone https://github.com/NOAA-OWP/snow17.git
cd snow17
```

Within the repo root, once again generate a CMake build directory.  However, this time, we (probably) need to specify the location of the NextGen ISO C Fortran BMI library.  This is an intermediate library needed for any Fortran BMI module to ensure NextGen compatibility.  This is done with the `ISO_C_FORTRAN_BMI_PATH` option

> [!WARNING]
> If you created stand-alone-only build directory already, remove it first.  Don't worry:  the NextGen-supporting build directory will also support stand-alone builds.


```bash
# The same advice discussed in the stand-alone section about optionally adding 'FC=<path_to_compiler>' and/or 
# '-DCMAKE_BUILD_TYPE=Debug' applies here
cmake -B cmake_build -DISO_C_FORTRAN_BMI_PATH=/Users/rbartel/Developer/noaa/ngen/extern/iso_c_fortran_bmi -S .
```

> [!TIP]
> The directory to use for ISO_C_FORTRAN_BMI_PATH will generally be `<path_to_your_ngen_repo>/extern/iso_c_fortran_bmi`.  It is assumed that you have already cloned the NextGen repo locally.


With this done when the build directory is created, we will also have access to another build target:  `snow17_bmi`.   This is how we build the NextGen BMI module shared library:

```bash
cmake --build cmake_build --target snow17_bmi
```

> [!NOTE]
> You can still build the stand-alone executable as described above when you generate a build directory for NextGen builds.  You can also omit specify a target when building, in which case, CMake will build all valid build targets.

```bash
cmake --build cmake_build --target snow17 # This will build the stand-alone executable, even when generating for NextGen builds
cmake --build cmake_build                 # This will build the stand-alone executable and the NextGen BMI module shared library
```

Once you build the shared library, you should see the shared library in build directory, named `cmake_build/libsnow17_bmi.<version>.so` on Linux systems (on Mac, you will see `.dylib` rather than `.so`).  There will also be `cmake_build/libsnow17_bmi.so` symlink pointing to the shared library file.

## Getting help

If there are concerns or bug fixes, etc., please  file an issue in this repository's Issue Tracker.

## Getting involved

We encourage community involvement in code development. For more info, please check out our [CONTRIBUTING](CONTRIBUTING.md) document.

----

## Open source licensing info
1. [TERMS](TERMS.md)
2. [LICENSE](LICENSE)

----

## Credits and references

If you wish to use or adapt the code in this repository, please make sure that your new repository credits this one as the original source of the code. 

### References
 - Anderson, E. A., 1973: National Weather Service River Forecast System-Snow Accumulation and Ablation Model. NOAA Tech. Memo. NWS Hydro-17, U.S. National Weather Service. [Avail- able from Office of Hydrologic Development, NOAA/NWS, 1325 East–West Highway, Silver Spring, MD 20910.]
