# OWP Snow17 Model Version for NextGen

**Description**:  

Snow17 is a snow accumulation and melt model that has been used by the National Weather Service since the late 1970s for operational streamflow 
forecasting.  It is a temperature-index model that was first described in Anderson (1973) before being incorporated into the NWS River Forecasting System (NWSRFS) and more recently the NWS Community Hydrologic Prediction System (CHPS).  The version of the Snow17 model maintained in this directory is formulated from original NWS FORTRAN code to be compatible with 
the NextGen simulation and forecasting framework. 

  - **Technology stack**: The model code is written in FORTRAN with a driver that incorporates Basic Model Interface (BMI) commands to enable Snow17 to be run as a module within the NextGen framework. 
  is intended as standalone or as a module in a framework or other ecosystem.  It has been compiled using gnu, pgi and intel compilers. 
  - **Status**: Beta (runs but has not been extensively tested; validates against the orginal repository version listed below, which has been extensively used))
  - **Links**:  This code was adapted from source code included in https://github.com/NCAR/NWS_hydro_models -- a version used in federally-funded streamflow forecasting research at NCAR. The original source code for that effort was obtained from the NWS Office of Hydrology around 2013. The code has since been copied and adapted into other research repositories. 

## Dependencies

TBA: Describe any dependencies that must be installed for this software to work.
This includes programming languages, databases or other storage mechanisms, build tools, frameworks, and so forth.
If specific versions of other software are required, or known not to work, call that out.

## Installation

To build and run Snow-17 with example data, check out the [INSTALL](INSTALL.md) document.

## Configuration

TBA:  If the software is configurable, describe it in detail, either here or in other documentation to which you link.

## How to test the software

Run the included test case example -- see INSTALL.md

## Known issues

The restart capability has not been implemented, but will be shortly. 
The software has so far only been tested with the gfortran compiler (on Cheyenne at NCAR)

## Getting help

Currently, questions may be sent to Andy Wood (andywood@ucar.edu). 
If there are concerns or bug fixes, etc., please  file an issue in this repository's Issue Tracker.

## Getting involved

We encourage community involvement in code development. For more info, please check out our CONTRIBUTING document.

----

## Open source licensing info
1. [TERMS](TERMS.md)
2. [LICENSE](LICENSE)

----

## Credits and references

If you wish to use or adapt the code in this repository, please make sure that your new repository credits this one as the original source of the code. 

### References
 - Anderson, E. A., 1973: National Weather Service River Forecast System-Snow Accumulation and Ablation Model. NOAA Tech. Memo. NWS Hydro-17, U.S. National Weather Service. [Avail- able from Office of Hydrologic Development, NOAA/NWS, 1325 East–West Highway, Silver Spring, MD 20910.]

### Related projects

### Books, papers, talks, or other sources that have meaningful impact or influence on this project


----
----

#### OWP Open Source Project Template Instructions

1. Create a new project.
2. [Copy these files into the new project](#installation)
3. Update the README, replacing the contents below as prescribed.
4. Add any libraries, assets, or hard dependencies whose source code will be included
   in the project's repository to the _Exceptions_ section in the [TERMS](TERMS.md).
  - If no exceptions are needed, remove that section from TERMS.
5. If working with an existing code base, answer the questions on the [open source checklist](opensource-checklist.md)
6. Delete these instructions and everything up to the _Project Title_ from the README.
7. Write some great software and tell people about it.

> Keep the README fresh! It's the first thing people see and will make the initial impression.

## Installation

To install all of the template files, run the following script from the root of your project's directory:

```
bash -c "$(curl -s https://raw.githubusercontent.com/NOAA-OWP/owp-open-source-project-template/open_source_template.sh)"
```

----

