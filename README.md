# OWP Snow17 Model Version for NextGen

**Description**:  

Snow17 is a snow accumulation and melt model that has been used by the National Weather Service since the late 1970s for operational streamflow 
forecasting.  It is a temperature-index model that was first described in Anderson (1973) before being incorporated into the NWS River Forecasting System (NWSRFS) and more recently the NWS Community Hydrologic Prediction System (CHPS).  The version of the Snow17 model maintained in this directory is formulated from original NWS FORTRAN code to be compatible with 
the [Next Generation Water Resources Modeling Framework](https://github.com/NOAA-OWP/ngen) (NextGen). 

  - **Technology stack**: The model code is written in Fortran with a driver that incorporates [Basic Model Interface](https://csdms.colorado.edu/wiki/BMI) (BMI) commands to enable Snow17 to be run in standalone mode or as a module within the NextGen framework (or other BMI-based systems). It has been compiled using GNU, PGI, and Intel compilers. 
  - **Status**: Beta (runs but has not been extensively tested; validates against the orginal repository version listed below, which has been extensively used)
  - **Links**:  This code was adapted from source code included in https://github.com/NCAR/NWS_hydro_models -- a version used in federally funded streamflow forecasting research at NCAR. The original source code for that effort was obtained from the NWS Office of Hydrology around 2013. The code has since been copied and adapted into other research repositories. 

## Dependencies

There are no current dependencies except for a Fortran-compatible compiler (e.g., GFortran)

## Installation

To build and run Snow-17 with example data, check out the [INSTALL](INSTALL.md) document.

## Configuration

TBA:  If the software is configurable, describe it in detail, either here or in other documentation to which you link.

## How to test the software

Run the included test case example -- see the [INSTALL](INSTALL.md) document.

## Known issues

The restart capability has not been implemented, but will be shortly. 
The software has so far only been tested with the GFortran compiler (on Cheyenne at NCAR)

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