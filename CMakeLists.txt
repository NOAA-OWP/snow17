cmake_minimum_required(VERSION 3.12)
project(snow17 VERSION 1.0.0 DESCRIPTION "OWP snow17 Implementation")
enable_language( Fortran )
include(CMakeDependentOption)

set( CMAKE_VERBOSE_MAKEFILE on )

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/../../cmake/")
# Uncomment this and rebuild artifacts to enable debugging
#set(CMAKE_BUILD_TYPE Debug)

add_compile_options(
  "-Wall"
  "$<$<CONFIG:DEBUG>:-g;-fbacktrace;-fbounds-check>"
)
project(snow17 VERSION 1.0.0 DESCRIPTION "External BMI Models Shared Libraries")


# Do this so we can default to ../iso_c_fortran_bmi for bindings if that directory exists
if (IS_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}/../iso_c_fortran_bmi)
    set(DEFAULT_ISO_C_BMI_DIR_VAL ${CMAKE_CURRENT_LIST_DIR}/../iso_c_fortran_bmi)
else ()
    set(DEFAULT_ISO_C_BMI_DIR_VAL OFF)
endif ()

option(ISO_C_FORTRAN_BMI_PATH  "Path to NextGen middleware for ISO C Fortran BMI bindings" ${DEFAULT_ISO_C_BMI_DIR_VAL})

# Syntax: cmake_dependent_option(<option> "<help_text>" <value> <depends> <force>)
cmake_dependent_option(BMI_SHARED_LIB "Support for building BMI module shared library" ON "ISO_C_FORTRAN_BMI_PATH" OFF)

if (${BMI_SHARED_LIB} AND NOT IS_DIRECTORY ${ISO_C_FORTRAN_BMI_PATH})
    message(FATAL_ERROR "Provided CMake option 'ISO_C_FORTRAN_BMI_PATH' is not a valid directory!")
endif ()

# TODO: (later) need to figure out the proper analogous compiler settings for -Wall in Intel and nv compilers
if ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel|IntelLLVM")
    add_compile_options(-cpp -fp-model=strict)
elseif ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU")
    add_compile_options(-cpp -Wall -fcheck=bounds -ffree-line-length-none -frounding-math -fno-fast-math)
elseif ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "NVIDIA" OR "${CMAKE_Fortran_COMPILER_ID}" MATCHES "NVHPC")
	add_compile_options(${SAC_LIB_NAME_CMAKE} -Kieee -Mbackslash -Mpreprocess)
endif ()

#### Add variables for individual libraries that are used within the *.pc.in files
# snow17
set(SNOW_LIB_NAME_CMAKE snow17bmi)
set(SNOW_STANDALONE_EXE_NAME snow17)
set(SNOW_LIB_DESC_CMAKE "External Snow17 Shared Library")
set(PARENT_SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR}/src/)
set(BMI_SOURCES_DIR ${PARENT_SOURCE_DIR}/bmi/)
set(DRIVER_SOURCE_DIR ${PARENT_SOURCE_DIR}/driver/)
set(MODEL_SOURCE_DIR ${PARENT_SOURCE_DIR}/snow19/)
set(SHARE_SOURCE_DIR ${PARENT_SOURCE_DIR}/share/)

# Create variables with the source files needed in different contexts
#file(GLOB MODEL_SOURCES ${PARENT_SOURCE_DIR}/sac/*.f90 ${PARENT_SOURCE_DIR}/driver/*.f90 ${PARENT_SOURCE_DIR}/share/*.f90)
#file(GLOB BMI_SOURCES ${BMI_SOURCES_DIR}*.f90)
#file(GLOB MODEL_SOURCES ${MODEL_SOURCE_DIR}/*.f)
#file(GLOB DRIVER_SOURCES ${DRIVER_SOURCE_DIR}/*.f90)
#file(GLOB SHARE_SOURCES ${SHARE_SOURCE_DIR}/*.f90)
set(BMI_SOURCES "${BMI_SOURCES_DIR}/bmi.f90" "${BMI_SOURCES_DIR}/bmi_snow17.f90" )
set(MODEL_SOURCES "${MODEL_SOURCE_DIR}/PACK19.f" "${MODEL_SOURCE_DIR}/SNDEPTH.f" "${MODEL_SOURCE_DIR}/SNEW.f"
   "${MODEL_SOURCE_DIR}/SNOWPACK.f" "${MODEL_SOURCE_DIR}/SNOWT.f" "${MODEL_SOURCE_DIR}/adjc19.f"
   "${MODEL_SOURCE_DIR}/aeco19.f" "${MODEL_SOURCE_DIR}/aesc19.f" "${MODEL_SOURCE_DIR}/exsnow19.f"
   "${MODEL_SOURCE_DIR}/melt19.f" "${MODEL_SOURCE_DIR}/rout19.f" "${MODEL_SOURCE_DIR}/updt19.f"
   "${MODEL_SOURCE_DIR}/zero19.f"
)
set(DRIVER_SOURCES ${DRIVER_SOURCE_DIR}/driver_bmi.f90)
set(SHARE_SOURCES "src/share/constants.f90" "src/share/dateTimeUtilsModule.f90"
        "src/share/forcingType.f90" "src/share/ioModule.f90" "src/share/modelVarType.f90"
        "src/share/namelistModule.f90" "src/share/nrtype.f90" "src/share/parametersType.f90"
        "src/share/runInfoType.f90" "src/share/runSnow17.f90"
)

add_executable(${SNOW_STANDALONE_EXE_NAME} ${MODEL_SOURCES} ${BMI_SOURCES} ${DRIVER_SOURCES} ${SHARE_SOURCES})

if (BMI_SHARED_LIB AND WIN32)
    message(FATAL_ERROR "Currently shared library target is not supported on Windows platforms.")
elseif (BMI_SHARED_LIB)
    add_subdirectory(${ISO_C_FORTRAN_BMI_PATH} ${CMAKE_BINARY_DIR}/iso_c_bmi)
    add_library(${SNOW_LIB_NAME_CMAKE} SHARED ${BMI_SOURCES} ${MODEL_SOURCES} ${DRIVER_SOURCES} ${SHARE_SOURCES})
    target_compile_definitions(${SNOW_LIB_NAME_CMAKE} PUBLIC BMI_ACTIVE NGEN_FORCING_ACTIVE NGEN_OUTPUT_ACTIVE NGEN_ACTIVE)
    target_compile_options(${SNOW_LIB_NAME_CMAKE} PUBLIC -fPIC)
    target_link_libraries(${SNOW_LIB_NAME_CMAKE} PUBLIC iso_c_bmi)
    set_target_properties(${SNOW_LIB_NAME_CMAKE} PROPERTIES VERSION ${PROJECT_VERSION})

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
