cmake_minimum_required(VERSION 3.12)
project(snow17 VERSION 1.0.0 DESCRIPTION "OWP snow17 Implementation")
enable_language( Fortran )
include(CMakeDependentOption)

# Perhaps in the future set/append to CMAKE_MODULE_PATH and create a cmake directory with helper cmake stuff
# list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake/")


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
set(SNOW_LIB_NAME_CMAKE snow17_bmi)
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

#    include(GNUInstallDirs)
#
#    install(TARGETS ${SNOW_LIB_NAME_CMAKE}
#            LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
#            PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})
endif ()
