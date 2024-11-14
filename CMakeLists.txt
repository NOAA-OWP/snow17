cmake_minimum_required(VERSION 3.12)
enable_language( Fortran )
add_subdirectory(../iso_c_fortran_bmi ${CMAKE_BINARY_DIR}/iso_c_bmi)

set( CMAKE_VERBOSE_MAKEFILE on )

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/../../cmake/")
# Uncomment this and rebuild artifacts to enable debugging
#set(CMAKE_BUILD_TYPE Debug)

add_compile_options(
  "-Wall"
  "$<$<CONFIG:DEBUG>:-g;-fbacktrace;-fbounds-check>"
)
project(snow17 VERSION 1.0.0 DESCRIPTION "External BMI Models Shared Libraries")

set( CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/fortran)

#### Add variables for individual libraries that are used within the *.pc.in files
# Snow17
set(SNOW17_LIB_NAME_CMAKE snow17bmi)
set(SNOW17_LIB_DESC_CMAKE "External Snow17 Shared Library")

# Make sure these are compiled with this directive
add_compile_definitions(BMI_ACTIVE NGEN_FORCING_ACTIVE NGEN_OUTPUT_ACTIVE NGEN_ACTIVE)

#Use ../../../FindNetCDF.cmake, which looks for config package, pkgconfig, and standard file locations
#in that order
###### 
# Updates to snow17 allow a build of the bmi module that does not require netcdf
# The NGEN_OUTPUT_ACTIVE compiler directive disables all netcdf related code, hence no need
# to link the netcdf library. If at some point this becomes required, the following section
# can be uncommented to provide mechanics to look for and/or define netcdf requirements.
# Also, the target link library line will need uncommented below.
######
###### NETCDF ######
# find_package (NetCDF COMPONENTS F90)
# set(netCDF_INCLUDE_DIR ${NetCDF_INCLUDE_DIRS} CACHE PATH "NetCDF Include Directory" )
# set(netCDF_C_LIB ${NetCDF_LIBRARIES} CACHE PATH 
# "The path to the C shared object library file, libnetcdf.so or the library name if it can be found by the linker, i.e. netcdf" )
# if(NetCDF_FOUND)
#   #This provides a little diagnostic output, but most importantly, gives cache varabiles
#   #for each piece of the netcdf puzzle that a user might need to directly override
#   #if netcdf isn't configured correctly or cannot be found using the known methods.
#   message("Found netCDF package")
#   message("NetCDF inc: " ${NetCDF_INCLUDE_DIR})
#   message("NetCDF lib: " ${NetCDF_LIBRARIES})
# else()
#   message(SEND_ERROR "Could not find NetCDF, install the depdency or\n 1) Set the NetCDF_DIR to a source build\n 2) set the netCDF* cache variables as appropriate")
# endif()
# # Ensure we can find the netcdf.mod file, leave a cache var that can be set if we can't
# find_path(_netCDF_MOD_PATH "netcdf.mod" ${netCDF_INCLUDE_DIR} REQUIRED NO_CACHE)
# set(netCDF_MOD_PATH ${_netCDF_MOD_PATH} CACHE PATH "Directory containing the netcdf.mod fortran module")

# # For some reason the netcdf config.cmake doesn't set the netcdff lib correctly in
# # ${netCDF_Libararies}, so it only links the C lib (netcdf) and not the fortran lib (netcdff)
# # So find that library explicitly and make sure we link with it.
# # also, the FindNetCDF.cmake doesn't seem to do it correctly either.

# find_library(_netCDF_FORTRAN_LIB netcdff REQUIRED  NO_CACHE)
# set(netCDF_FORTRAN_LIB ${_netCDF_FORTRAN_LIB} CACHE PATH 
# "The path to the C shared object library file, libnetcdff.so or the library name if it can be found by the linker, i.e. netcdff")
##### END NETCDF #####

set(MODEL_SOURCE_DIR src/)
set(BMI_SOURCE_DIR src/bmi/)

file(GLOB BMI_SOURCE ${BMI_SOURCE_DIR}*.f90)
#file( GLOB MODEL_SOURCES ${MODEL_SOURCE_DIR}/snow19/*.f ${MODEL_SOURCE_DIR}/driver/*.f90 ${MODEL_SOURCE_DIR}/share/*.f90)
file( GLOB MODEL_SOURCES ${MODEL_SOURCE_DIR}/snow19/*.f  ${MODEL_SOURCE_DIR}/share/*.f90 ${MODEL_SOURCE_DIR}/driver/*.f90 ${CMAKE_CURRENT_LIST_DIR}/../datetime-fortran-1.7.0/src/*.f90)

if(WIN32)
    add_library(snow17bmi ${BMI_SOURCE} )
else()
    add_library(snow17bmi SHARED ${BMI_SOURCE} ${MODEL_SOURCES} )
endif()

#target_include_directories(snow17bmi PUBLIC ${netCDF_INCLUDE_DIR} ${netCDF_MOD_PATH})    # netcdf not needed for this code
#target_include_directories( snow17bmi PUBLIC ${FDATETIME_INCLUDE_DIRS} )    

#We know we are building this for NGEN support, so define the preprocessor directive in the compile options
#Not sure why the global 
target_compile_options(snow17bmi PUBLIC -cpp -ffree-line-length-none)
#target_compile_options(snow17bmi PUBLIC -O3 -fdefault-real-8 -fno-align-commons -ffree-line-length-none -cpp -fcheck=all)

##### NETCDF #####
#target_link_libraries( snow17bmi iso_c_bmi ${netCDF_C_LIB} ${netCDF_FORTRAN_LIB})
##### END NETCDF #####
#target_link_libraries( snow17bmi PUBLIC iso_c_bmi fortran_stdlib::fortran_stdlib ${FDATETIME_LIBRARIES} )
target_link_libraries( snow17bmi PUBLIC iso_c_bmi )

set_target_properties(snow17bmi PROPERTIES VERSION ${PROJECT_VERSION})

include(GNUInstallDirs)

install(TARGETS snow17bmi
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

configure_file(snow17bmi.pc.in snow17bmi.pc @ONLY)

install(FILES ${CMAKE_BINARY_DIR}/snow17bmi.pc DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/pkgconfig)
