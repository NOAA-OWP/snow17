# Installation and example instructions

1. Clone the online GitHub repository either from the main git repo source (https://github.com/NOAA-OWP/snow17.git) or your fork of it -- e.g., `git clone https://github.com/NOAA-OWP/snow17.git`
2. Change directory to the build directory: `cd snow17/build/`
3. Copy `Makefile` to `Makefile.local` (`cp Makefile Makefile.local`) and edit the local version to update the compiler and paths if necessary to match the resources on your system.
4. Run `make -f MakefileLocal` to compile the program and generate the executable, which will be found in `snow17/bin/`
5. There is a test case in the `snow17/test_cases/` directory.  To run it, first cd to the directory (`cd ../test_cases/`) and unpack the example:  `tar -xzvf ex1.tgz`.
6. `cd ex1/run/` to get to the run directory and then execute the shell script (`./runSnow17.csh`) or type `../../../bin/snow17.exe namelist.bmi.HHWM8` into your command line
7. Check the example in the output folder (`cd ../output/`)

TL;DR:
```
git clone https://github.com/NOAA-OWP/snow17.git
cd snow17/build/
cp Makefile MakefileLocal # edit to match your compiler and system resources
make -f MakefileLocal
cd ../test_cases/
tar -xzvf ex1.tgz
cd ex1/run/
./runSnow17.csh           # or ../../../bin/snow17.exe namelist.bmi.HHWM8
cd ../output/             # to view output data 
```
Note that the ex1/ example is part of the repo.  The unpacked directory and test contents will not be included in a push back to the online repo.  To the program for other purposes, create new input files and output directories outside of the `snow17/` repository directory, and link to the executable in `snow17/bin/`. 


