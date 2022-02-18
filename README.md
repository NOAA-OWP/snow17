# OWP Snow17 Model Version for NextGen

**Description**:  

Snow17 is a snow accumulation and melt model that has been used by the National Weather Service since the late 1970s for operational streamflow 
forecasting.  It is a temperature-index model that was first described in Anderson (1973) before being incorporated into the NWS River Forecasting System (NWSRFS) and more recently the NWS Community Hydrologic Prediction System (CHPS).  The version of the Snow17 model maintained in this directory is formulated from original NWS FORTRAN code to be compatible with 
the NextGen simulation and forecasting framework. 

  - **Technology stack**: The model code is written in FORTRAN with a driver that incorporates Basic Model Interface (BMI) commands to enable Snow17 to be run as a module within the NextGen framework. 
  is intended as standalone or as a module in a framework or other ecosystem.  It has been compiled using gnu, pgi and intel compilers. 
  - **Status**: pre-Beta (still in initial development)
  - **Links**:  This code was adapted from source code included in https://github.com/NCAR/NWS_hydro_models -- a version used in federally-funded streamflow forecasting research at NCAR. The original source code for that effort was obtained from the NWS Office of Hydrology around 2013. 
  ** - Describe what sets this apart from related-projects. Linking to another doc or page is OK if this can't 
  be expressed in a sentence or two.


## Dependencies

TBA: Describe any dependencies that must be installed for this software to work.
This includes programming languages, databases or other storage mechanisms, build tools, frameworks, and so forth.
If specific versions of other software are required, or known not to work, call that out.

## Installation

To build and run Snow-17 with example data, check out the [INSTALL](INSTALL.md) document.

## Configuration

TBA:  If the software is configurable, describe it in detail, either here or in other documentation to which you link.

## Usage

TBA: Show users how to use the software. Be specific. Use appropriate formatting when showing code snippets.

## How to test the software

TBA:  If the software includes automated tests, detail how to run those tests.

## Known issues

TBA:  Document any known significant shortcomings with the software.

## Getting help

TBA: Instruct users how to get help with this software; this might include links to an issue tracker, wiki, mailing list, etc.

**Example**

If you have questions, concerns, bug reports, etc, please file an issue in this repository's Issue Tracker.

## Getting involved

TBA:  This section should detail why people should get involved and describe key areas you are
currently focusing on; e.g., trying to get feedback on features, fixing certain bugs, building
important pieces, etc.

General instructions on _how_ to contribute should be stated with a link to [CONTRIBUTING](CONTRIBUTING.md).


----

## Open source licensing info
1. [TERMS](TERMS.md)
2. [LICENSE](LICENSE)

----

## Credits and references

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

