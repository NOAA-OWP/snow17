# OWP Snow17 Model Version for NextGen

**Description**:  

Snow17 is a snow accumulation and melt model that has been used by the National Weather Service since the late 1970s for operational streamflow 
forecasting.  It is a temperature-index model that was first described in Anderson (1973) [[1]](#ref-anderson-1973) before being incorporated into the NWS River Forecasting System (NWSRFS) and more recently the NWS Community Hydrologic Prediction System (CHPS).  The version of the Snow17 model maintained in this directory is formulated from original NWS FORTRAN code to be compatible with 
the [Next Generation Water Resources Modeling Framework](https://github.com/NOAA-OWP/ngen) (NextGen). 

  - **Technology stack**: The model code is written in Fortran with a driver that incorporates [Basic Model Interface](https://csdms.colorado.edu/wiki/BMI) (BMI) commands to enable Snow17 to be run in standalone mode or as a module within the NextGen framework (or other BMI-based systems). It has been compiled using GNU, PGI, and Intel compilers. 
  - **Status**: Beta (runs but has not been extensively tested; validates against the original repository version listed below, which has been extensively used)
  - **Links**:  This code was adapted from source code included in https://github.com/NCAR/NWS_hydro_models -- a version used in federally funded streamflow forecasting research at NCAR. The original source code for that effort was obtained from the NWS Office of Hydrology around 2013. The code has since been copied and adapted into other research repositories. 

In standalone mode, multiple hydrologic response units (HRUs) can be modeled at once. Multiple HRUs are not currently supported through BMI (e.g., in NextGen): each model instance represents a single HRU. Set `n_hrus = 1` in the namelist and pass parameter values for a single HRU.

## Dependencies
- Fortran compiler
- NextGen ISO C Fortran BMI library (optional)

## Running in Standalone
The following describes how to install and run snow17 as a standalone model.

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

There is a test case in the `snow17/test_cases/` directory.  To run it, first cd to the directory (`cd ../test_cases/`) and unpack the example:  `tar -xzvf ex1.tgz`.
`cd ex1/run/` to get to the run directory and then execute the shell script (`./runSnow17.csh`) or type `../../../cmake_build/snow17 namelist.bmi.HHWM8` into your command line
Check the example in the output folder (`cd ../output/`)


## Running in [Ngen](https://github.com/NOAA-OWP/ngen)

The following are instructions for building the Snow17 BMI module shared library.  In particular, this is necessary to run the Snow17 model in the [Next Generation Water Resources Modeling Framework](https://github.com/NOAA-OWP/ngen).


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


With this done when the build directory is created, we will also have access to another build target:  `snow17bmi`.   This is how we build the NextGen BMI module shared library:

```bash
cmake --build cmake_build --target snow17bmi
```

> [!NOTE]
> You can still build the stand-alone executable as described above when you generate a build directory for NextGen builds.  You can also omit specify a target when building, in which case, CMake will build all valid build targets.

```bash
cmake --build cmake_build --target snow17 # This will build the stand-alone executable, even when generating for NextGen builds
cmake --build cmake_build                 # This will build the stand-alone executable and the NextGen BMI module shared library
```

Once you build the shared library, you should see the shared library in build directory, named `cmake_build/libsnow17bmi.<version>.so` on Linux systems (on Mac, you will see `.dylib` rather than `.so`).  There will also be `cmake_build/libsnow17bmi.so` symlink pointing to the shared library file.

## Namelist

The namelist file configures a run. The standalone executable reads the file named on the command line, or `namelist.input` in the current directory if none is given. In NextGen, it is the `init_config` of the Snow17 formulation in the realization file. It is a Fortran namelist: it starts with `&SNOW17_CONTROL`, ends with `/`, and text after `!` is a comment. Strings should be quoted, including `start_datehr` and `end_datehr` (`start_datehr = "2015120100"`). Unquoted strings work for older compilers but quoted always works. Example namelists are included in the test case (`ex1/run/namelist.bmi.HHWM8` in `test_cases/ex1.tgz`) and in [configs/snow17-init-cat-27.namelist.input](configs/snow17-init-cat-27.namelist.input) for NextGen.

In NextGen, the framework supplies the forcing and handles the outputs, so the forcing and output settings are not used.

Setting | Description | Standalone | NextGen
--------|-------------|------------|--------
main_id | Basin label or gage ID (up to 20 characters) | Names the basin-average output file, `<output_root><main_id>.txt` | Not used
n_hrus | Number of HRUs | Any number | Must be 1 hru per NextGen divide
forcing_root | Path prefix for the forcing files | Reads each HRU's forcing from `<forcing_root><hru_id>.csv`<sup>*</sup> | Not used
output_root | Path prefix for the output files | Used for all output files | Not used
output_hrus | Also write output for each HRU (1 = yes, 0 = no) | Creates `<output_root><hru_id>.txt` for each HRU. Rows are only written when `n_hrus` > 1 | Not used
snow17_param_file | Path to the [parameter file](#parameters) | Used | Used
start_datehr | Date and hour of the first timestep, as `"YYYYMMDDHH"` | Forcing is read from the row with this date | Set it to the realization's `start_time`<sup>§</sup>
end_datehr | Date and hour of the last timestep, as `"YYYYMMDDHH"` | Last forcing row read | If `allow_exceed_end_time` is false in the realization, it must be at least one timestep after the realization's `end_time`
model_timestep | Timestep in seconds. Must be a multiple of 3600 | Must match the forcing interval | Must match the realization timestep
warm_start_run | Start from state files (1 = yes, 0 = no)<sup>†</sup> | Reads `<snow_state_in_root><hru_id>.txt` | Same as standalone
write_states | Write state files at every timestep (1 = yes, 0 = no)<sup>‡</sup> | Writes `<snow_state_out_root><hru_id>.txt` | Same as standalone
snow_state_in_root | Path prefix for the input state files | Used if `warm_start_run` = 1 | Same as standalone
snow_state_out_root | Path prefix for the output state files | Used if `write_states` = 1 | Same as standalone

<sup>*</sup> Standalone forcing files have one header line, then one row per timestep with the columns year, month, day, hour, precipitation (mm s<sup>-1</sup>) and air temperature (°C), separated by commas or spaces. The columns must be in this order; the header names are not read. Rows must run consecutively from `start_datehr` through `end_datehr`, because the run stops if a row's date does not match the model time. If a file has no row for `start_datehr`, the run currently hangs instead of stopping with an error.

<sup>†</sup> A state file has one header line, then one row per timestep: the date, `tprev` and the 19 carryover states `cs1`–`cs19`, as they were at the end of that timestep. A warm start reads the row dated one timestep before `start_datehr`, so a run that continues an earlier one starts one timestep after the earlier run's `end_datehr`. A row dated `FcstDate` matches any start date, for forecast (ESP) initialization.

<sup>‡</sup> `warm_start_run` and `write_states` cannot both be 1. If they are, `write_states` is set to 0.

<sup>§</sup> NextGen supplies forcing by its own clock, so `start_datehr` does not change which forcing the model receives. It sets the model's calendar: the day of year for the seasonal melt factor, the dates written to state files and the date a warm start looks for.

In NextGen, state files are named by the `hru_id` in the parameter file. Catchments that share both a parameter file and a `snow_state_out_root` write to the same state file, so give each catchment its own `hru_id` or its own `snow_state_out_root`.

## Parameters

The tables below list the 26 lines required in the parameter file (the file named by `snow17_param_file` in the [namelist](#namelist)): 4 HRU metadata lines, 11 model parameters and 11 areal depletion curve ordinates. Each line holds a parameter name followed by one value per HRU, separated by spaces (not tabs). Lines can be in any order, but names are case-sensitive. An example parameter file is included in the test case (`ex1/input/params/snow17_params.HHWM8.txt` in `test_cases/ex1.tgz`).

The parameters are split into major parameters, which are usually calibrated, and minor parameters, which are usually set from climate and site conditions and normally not calibrated [[3]](#ref-anderson-2006). In NextGen, the major parameters can also be set with `model_params` in the realization file, which overrides the parameter file. The minor parameters and HRU metadata can only be set in the parameter file. See [[3]](#ref-anderson-2006) for additional details about the conceptual meaning of each parameter.

The minimum and maximum columns give suggested ranges based on [[2]](#ref-anderson-2002) and [[3]](#ref-anderson-2006). The model does not enforce parameter bounds, and the example parameter file includes some values outside these ranges. Initial values are the midpoints of the ranges. They are for reference and should be based on a priori information when available [[2]](#ref-anderson-2002).

### HRU metadata and major parameters

Parameter | Description | Units | Min | Max | Init
----------|-------------|-------|---------|---------|--------------
hru_id | Identification string for each HRU | - | - | - | -
hru_area<sup>*</sup> | Area of each HRU | km<sup>2</sup> | - | - | -
latitude | Centroid latitude of each HRU | decimal degrees | - | - | -
elev | Mean elevation of each HRU | m | - | - | -
scf | Multiplying factor applied to precipitation that falls as snow (mainly gage catch deficiency) | - | 0.7 | 1.6 | 1.15
mfmax | Maximum melt factor during non-rain periods (June 21) | mm °C<sup>-1</sup> 6hr<sup>-1</sup> | 0.5 | 2.2 | 1.35
mfmin | Minimum melt factor during non-rain periods (December 21) | mm °C<sup>-1</sup> 6hr<sup>-1</sup> | 0.1 | 0.6 | 0.35
uadj | Average wind function during rain-on-snow periods | mm mb<sup>-1</sup> 6hr<sup>-1</sup> | 0.05 | 0.2 | 0.125
si | Mean areal water equivalent above which there is always 100% snow cover | mm | 1 | 9999 | 5000
adc1<sup>†</sup> | Areal snow cover at water equivalent / areal index = 0.0 | fraction | - | - | 0.05
adc2 | Areal snow cover at water equivalent / areal index = 0.1 | fraction | 0.11 | 0.30 | 0.205
adc3 | Areal snow cover at water equivalent / areal index = 0.2 | fraction | 0.20 | 0.52 | 0.36
adc4 | Areal snow cover at water equivalent / areal index = 0.3 | fraction | 0.45 | 0.65 | 0.55
adc5 | Areal snow cover at water equivalent / areal index = 0.4 | fraction | 0.50 | 0.77 | 0.635
adc6 | Areal snow cover at water equivalent / areal index = 0.5 | fraction | 0.56 | 0.81 | 0.685
adc7 | Areal snow cover at water equivalent / areal index = 0.6 | fraction | 0.61 | 0.86 | 0.735
adc8 | Areal snow cover at water equivalent / areal index = 0.7 | fraction | 0.65 | 0.89 | 0.77
adc9 | Areal snow cover at water equivalent / areal index = 0.8 | fraction | 0.69 | 0.93 | 0.81
adc10 | Areal snow cover at water equivalent / areal index = 0.9 | fraction | 0.80 | 0.96 | 0.88
adc11<sup>†</sup> | Areal snow cover at water equivalent / areal index = 1.0 | fraction | - | - | 1.0

<sup>*</sup> The area is only used to weight averaged outputs, so other units work as long as they are consistent across HRUs.

<sup>†</sup> `adc1` should always be 0.05 and `adc11` should always be 1.0 [[3]](#ref-anderson-2006). The model does not enforce this.

### Minor parameters

Parameter | Description | Units | Min | Max | Init
----------|-------------|-------|---------|---------|--------------
pxtemp | Temperature that separates rain from snow (snow at or below) | °C | 0.5 | 2.0 | 1.25
nmf | Maximum negative melt factor (June 21) | mm °C<sup>-1</sup> 6hr<sup>-1</sup> | 0.05 | 0.3 | 0.175
tipm | Antecedent temperature index parameter: smaller values give more weight to earlier timesteps | - | 0.05 | 0.2 | 0.125
mbase<sup>‡</sup> | Base temperature for melt during non-rain periods | °C | - | - | 0.0
plwhc | Liquid water holding capacity, as a fraction of the ice portion of the snow (maximum 0.4) | fraction | 0.02 | 0.3 | 0.16
daygm | Constant daily melt at the snow-soil interface (0.0 for frozen soils, 0.3 for intermittent snow or temperate winters) | mm day<sup>-1</sup> | 0.0 | 0.3 | 0.15

<sup>‡</sup> `mbase` has no suggested range. A value of 0 °C is typically used [[3]](#ref-anderson-2006).

The areal depletion curve (`adc1`–`adc11`) gives the fraction of the area covered by snow as a function of the ratio of the mean areal water equivalent to the areal index. The areal index is the smaller of `si` and the maximum water equivalent of the accumulation period [[3]](#ref-anderson-2006). The ordinates should not decrease. The adc ranges are derived from Figure 7-4-4 of [[2]](#ref-anderson-2002) and from [[4]](#ref-newman-2015). They capture the range recommended by Anderson, but wider ranges may be needed in some locations. Set the adc values a priori when data is available [[4]](#ref-newman-2015).

The melt factors `mfmax`, `mfmin` and `nmf` and the wind function `uadj` are always per 6 hours, and `daygm` is always per day. The model scales them, and `tipm`, to the timestep, so no unit conversion is needed when `model_timestep` changes. `model_timestep` must be a multiple of 3600 s. Calibrated values may still not transfer exactly between runs with different timesteps.

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
1. <a id="ref-anderson-1973"></a>Anderson, E. A., 1973: National Weather Service River Forecast System-Snow Accumulation and Ablation Model. NOAA Tech. Memo. NWS Hydro-17, U.S. National Weather Service. [Available from Office of Hydrologic Development, NOAA/NWS, 1325 East–West Highway, Silver Spring, MD 20910.]
2. <a id="ref-anderson-2002"></a>Anderson, E.A. (2002). Calibration of conceptual hydrologic models for use in river forecasting. NOAA Technical Report, NWS 45, Hydrology Laboratory, Silver Spring, MD. https://www.weather.gov/media/owp/oh/hrl/docs/1_Anderson_CalbManual.pdf
3. <a id="ref-anderson-2006"></a>Anderson, E.A. (2006). Snow Accumulation and Ablation Model – SNOW-17. https://www.weather.gov/media/owp/oh/hrl/docs/22snow17.pdf
4. <a id="ref-newman-2015"></a>Newman, A. J., Clark, M. P., Sampson, K., Wood, A., Hay, L. E., Bock, A., Viger, R. J., Blodgett, D., Brekke, L., Arnold, J. R., Hopson, T., and Duan, Q.: Development of a large-sample watershed-scale hydrometeorological data set for the contiguous USA: data set characteristics and assessment of regional variability in hydrologic model performance, Hydrol. Earth Syst. Sci., 19, 209–223, https://doi.org/10.5194/hess-19-209-2015, 2015.
