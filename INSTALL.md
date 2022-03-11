# Installation instructions

1. Clone the online github repository either from the main git repo source (https://github.com/NOAA-OWP/snow17.git) or your fork of it. 
   cmd:  `clone [github source]`
2. Change directory to the build directory: `cd snow17/build/`
3. Copy `Makefile` to `Makefile-local` and edit the local version to update the compiler and paths if necessary to match the resources on your system.
4. Run `make -f Makefile.local` to compile the program and generate the executable, which will be found in `snow17/bin/`
5. There is a test case in the `snow17/test_cases/` directory.  To run it, first cd to the directory and unpack the example:  `tar -xzvf ex1.tgz`.
6. cd into `ex1/run/` directory and run the shell script (`./runSnow17.csh`) or type `../../../bin/snow17.exe namelist.HHWM8` into your command line
7. Check the example output in `snow17/test_cases/ex1//output/`

Note that the ex1/ example is part of the repo.  The unpacked directory and test contents will not be included in a push back to the online repo.  To the program for other purposes, create new input files and output directories outside of the `snow17/` repository directory, and link to the executable in `snow17/bin/`. 


