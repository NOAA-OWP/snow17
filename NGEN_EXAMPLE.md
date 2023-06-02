# Running Snow17 in NextGen with Sample Data

See general [instructions](https://github.com/NOAA-OWP/ngen/wiki/NGen-Tutorial) for building and running models in the NextGen framework. This document outlines a valid workflow for coupling [CFE](https://github.com/NOAA-OWP/cfe), [PET](https://github.com/NOAA-OWP/evapotranspiration), and [Snow17](https://github.com/NOAA-OWP/snow17) in ngen as a single catchment model execution.  Forcing data is provided by the framework.   

### Build
First, clone `ngen` and update it's submodules.
```
git clone https://github.com/noaa-owp/ngen && cd ngen
git submodule update --init --recursive
```
Then, clone, update and build the following modules,  
  - #### CFE
    ```
    git submodule update --remote extern/cfe/cfe 
    cmake -B extern/cfe/cmake_build -S extern/cfe/cfe/ -DNGEN=ON
    make -C extern/cfe/cmake_build
    ```  
  - #### Fortran BMI
    [This](https://github.com/NOAA-OWP/ngen/tree/master/extern/iso_c_fortran_bmi) light-weight library allows for C/Fortran interoperability for BMI functions.

    ```
    cmake -B extern/iso_c_fortran_bmi/cmake_build -S extern/iso_c_fortran_bmi
    make -C extern/iso_c_fortran_bmi/cmake_build
    ```
  - #### Snow17
    ```
    git clone https://github.com/NOAA-OWP/snow17.git ./extern/snow17
    cmake -B extern/snow17/cmake_build -S extern/snow17
    cmake --build extern/snow17/cmake_build --target all
    ``` 
  - #### PET
    ```
    cmake -B extern/evapotranspiration/cmake_build -S extern/evapotranspiration/evapotranspiration/
    make -C extern/evapotranspiration/cmake_build/
    ```
  - #### SLoTH
    [SLoTH](https://github.com/NOAA-OWP/SLoTH) BMI provides dummy values for bmi input variables that are not used in the realization.

    ```
    cd extern/sloth/ && git checkout latest 
    git submodule update --init --recursive
    cd ../..
    cmake -B extern/sloth/cmake_build -S extern/sloth/
    make -C extern/sloth/cmake_build
    ```
  - #### NGEN
    ```
    cmake -B cmake_build -S . -DBMI_C_LIB_ACTIVE=ON -DBMI_FORTRAN_ACTIVE=ON
    make -j4 -C cmake_build
    ```
  
 ### Run 
Pre-process step for configuration and data pathing
 ```
  tar -xzvf extern/snow17/test_cases/ex1.tgz -C extern/snow17/test_cases/
  cp extern/snow17/configs/example_bmi_multi_realization_config_w_snow17.json data/
  cp extern/snow17/configs/snow17-init-cat-27.namelist.input data/bmi/fortran/
  ```
Integrated models (SLoTH+PET+Snow17+CFE) example 
 ```
./cmake_build/ngen data/catchment_data.geojson cat-27 data/nexus_data.geojson nex-26 data/example_bmi_multi_realization_config_w_snow17.json
 ```
See output files `cat-27.csv` and `nex-26_output.csv` for model results. 
