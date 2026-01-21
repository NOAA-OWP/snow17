# Installation and example instructions

1. Clone the online GitHub repository either from the main git repo source (https://github.com/NOAA-OWP/snow17.git) or your fork of it -- e.g., `git clone https://github.com/NOAA-OWP/snow17.git`
2. Change directory to the build directory: `cd snow17/build/`
3. Copy `Makefile` to `Makefile.local` (`cp Makefile Makefile.local`) and edit the local version to update the compiler and paths if necessary to match the resources on your system.
4. Run `make -f Makefile.local` to compile the program and generate the executable, which will be found in `snow17/bin/`
5. There is a test case in the `snow17/test_cases/` directory.  To run it, first cd to the directory (`cd ../test_cases/`) and unpack the example:  `tar -xzvf ex1.tgz`.
6. `cd ex1/run/` to get to the run directory and then execute the shell script (`./runSnow17.csh`) or type `../../../bin/snow17.exe namelist.bmi.HHWM8` into your command line
7. Check the example in the output folder (`cd ../output/`)

TL;DR:
```
git clone https://github.com/NOAA-OWP/snow17.git
cd snow17/build/
cp Makefile Makefile.local # edit to match your compiler and system resources
make -f Makefile.local
cd ../test_cases/
tar -xzvf ex1.tgz
cd ex1/run/
./runSnow17.csh           # or ../../../bin/snow17.exe namelist.bmi.HHWM8
cd ../output/             # to view output data 
```
Note that the ex1/ example is part of the repo.  The unpacked directory and test contents will not be included in a push back to the online repo.  To the program for other purposes, create new input files and output directories outside of the `snow17/` repository directory, and link to the executable in `snow17/bin/`. 

## Logger

The Errror Warning and Trapping Systems (EWTS) has been added to this module using a logging schema. All write statements have been converted to `write_log` statements, which saves the ouptut to a log file based on the log level.

When running within the ngen framework, the log file and log level are handled programatically. When running standalone, logging is defaulted to DISABLED. 

**Running Standalone**

In order to generate log messages when running standalone, the `NGEN_EWTS_LOGGING` environment variable must be set to `ENABLED`. This is the only required environment variable . Other optional logger environment variables exist for specifying the log file full pathname and setting the log level. If the user only enables logging, the log level will be set to INFO and the filename will be created based on the user and module names. All logger setup details are written to the console when the module is run. 
```
# Case Sensitive
export NGEN_EWTS_LOGGING=ENABLED
export NGEN_LOG_FILE_PATH=<full pathname for log file>
export SNOW17_LOGLEVEL=<DEBUG, INFO, WARNING, SEVERE, FATAL>
```
**Log Levels**
| Level   | Description                                         | Typical Use                                   |
|---------|-----------------------------------------------------|-----------------------------------------------|
| DEBUG   | Detailed diagnostic info for development/troubleshooting. | Variable values, function entry/exit. |
| FATAL   | Critical failure that aborts or makes app unrecoverable. | Crashes, memory errors, invalid state.        |
| INFO    | General events confirming expected operations.       | Startup/shutdown, configs, task completions.  |
| SEVERE  | Significant problem; app may continue in degraded state. | Failed services, corrupted configs, data loss.|
| WARNING | Potential issue that doesn’t stop execution.         | Deprecated APIs, missing files, repeatable errors. |

Default log level is INFO. The log level is hierarchical. Setting it to INFO, will log INFO, WARNING, SEVERE and FATAL
messages.

