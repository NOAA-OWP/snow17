MODULE parametersModule
!  Parameter table read routines:
!  Adapted from the original module_sf_noahmplsm.F module with several modifications
!  1. made _TABLE variables public.
!  2. added directory argment in subroutines to enable to read TBLs from a desired location
!  3. replace error handle routine ("wrf_error_fatal") with "handle_err"

    implicit none

    save

    integer, private, parameter :: MVT   = 27
    integer, private, parameter :: MBAND = 2
 

    real, public :: C3PSN_TABLE(MVT)       !photosynthetic pathway: 0. = c4, 1. = c3
    real, public :: KC25_TABLE(MVT)        !co2 michaelis-menten constant at 25c (pa)
    real, public :: AKC_TABLE(MVT)         !q10 for kc25
    real, public :: KO25_TABLE(MVT)        !o2 michaelis-menten constant at 25c (pa)
    real, public :: AKO_TABLE(MVT)         !q10 for ko25
    real, public :: VCMX25_TABLE(MVT)      !maximum rate of carboxylation at 25c (umol co2/m**2/s)
    real, public :: AVCMX_TABLE(MVT)       !q10 for vcmx25
    real, public :: BP_TABLE(MVT)          !minimum leaf conductance (umol/m**2/s)
    real, public :: MP_TABLE(MVT)          !slope of conductance-to-photosynthesis relationship
    real, public :: QE25_TABLE(MVT)        !quantum efficiency at 25c (umol co2 / umol photon)
    real, public :: AQE_TABLE(MVT)         !q10 for qe25
    real, public :: RMF25_TABLE(MVT)       !leaf maintenance respiration at 25c (umol co2/m**2/s)

    ! Public subroutines/functions
    public :: read_veg_parameters

CONTAINS

  SUBROUTINE read_veg_parameters(param_dir, noahowp_table, DATASET_IDENTIFIER)
    implicit none
    character(len=*), intent(in) :: param_dir
    character(len=*), intent(in) :: noahowp_table
    character(len=*), intent(in) :: DATASET_IDENTIFIER
    integer :: ierr
    integer :: IK,IM
    logical :: file_named

    integer :: NVEG
    character(len=256) :: VEG_DATASET_DESCRIPTION

    integer :: ISURBAN
    integer :: ISWATER
    integer :: ISBARREN
    integer :: ISICE
    integer :: ISCROP
    integer :: EBLFOREST
    integer :: NATURAL
    integer :: LCZ_1
    integer :: LCZ_2
    integer :: LCZ_3
    integer :: LCZ_4
    integer :: LCZ_5
    integer :: LCZ_6
    integer :: LCZ_7
    integer :: LCZ_8
    integer :: LCZ_9
    integer :: LCZ_10
    integer :: LCZ_11

    real, dimension(MVT) :: SAI_JAN,SAI_FEB,SAI_MAR,SAI_APR,SAI_MAY,SAI_JUN, &
                                     SAI_JUL,SAI_AUG,SAI_SEP,SAI_OCT,SAI_NOV,SAI_DEC
    real, dimension(MVT) :: LAI_JAN,LAI_FEB,LAI_MAR,LAI_APR,LAI_MAY,LAI_JUN, &
                                     LAI_JUL,LAI_AUG,LAI_SEP,LAI_OCT,LAI_NOV,LAI_DEC
    real, dimension(MVT) :: RHOL_VIS, RHOL_NIR, RHOS_VIS, RHOS_NIR, &
                                     TAUL_VIS, TAUL_NIR, TAUS_VIS, TAUS_NIR
    real, dimension(MVT) :: CH2OP, DLEAF, Z0MVT, HVT, HVB, DEN, RC, MFSNO, SCFFAC, XL, CWPVT, C3PSN, KC25, AKC, KO25, AKO, &
                            AVCMX, AQE, LTOVRC,  DILEFC,  DILEFW,  RMF25 ,  SLA   ,  FRAGR ,  TMIN  ,  VCMX25,  TDLEF ,  &
                            BP, MP, QE25, RMS25, RMR25, ARM, FOLNMX, WDPOOL, WRRAT, MRP, SHDFAC, NROOT, RGL, RS, HS, TOPT, RSMAX, &
                            SLAREA, EPS1, EPS2, EPS3, EPS4, EPS5

    namelist / usgs_veg_categories / VEG_DATASET_DESCRIPTION, NVEG

    namelist / usgs_veg_parameters / ISURBAN, ISWATER, ISBARREN, ISICE, ISCROP, EBLFOREST, NATURAL, &
         LCZ_1,LCZ_2,LCZ_3,LCZ_4,LCZ_5,LCZ_6,LCZ_7,LCZ_8,LCZ_9,LCZ_10,LCZ_11,&
         CH2OP, DLEAF, Z0MVT, HVT, HVB, DEN, RC, MFSNO, SCFFAC, XL, CWPVT, C3PSN, KC25, AKC, KO25, AKO, AVCMX, AQE, &
         LTOVRC,  DILEFC,  DILEFW,  RMF25 ,  SLA   ,  FRAGR ,  TMIN  ,  VCMX25,  TDLEF ,  BP, MP, QE25, RMS25, RMR25, ARM, &
         FOLNMX, WDPOOL, WRRAT, MRP, SHDFAC, NROOT, RGL, RS, HS, TOPT, RSMAX, &
         SAI_JAN, SAI_FEB, SAI_MAR, SAI_APR, SAI_MAY, SAI_JUN,SAI_JUL,SAI_AUG,SAI_SEP,SAI_OCT,SAI_NOV,SAI_DEC, &
         LAI_JAN, LAI_FEB, LAI_MAR, LAI_APR, LAI_MAY, LAI_JUN,LAI_JUL,LAI_AUG,LAI_SEP,LAI_OCT,LAI_NOV,LAI_DEC, &
         RHOL_VIS, RHOL_NIR, RHOS_VIS, RHOS_NIR, TAUL_VIS, TAUL_NIR, TAUS_VIS, TAUS_NIR, SLAREA, EPS1, EPS2, EPS3, EPS4, EPS5



    inquire( file=trim(param_dir)//'/'//trim(noahowp_table), exist=file_named )
    if ( file_named ) then
      open(15, file=trim(param_dir)//'/'//trim(noahowp_table), status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if

    if (ierr /= 0) then
       call handle_err(ierr, "ParametersRead.f90: read_veg_parameters: Cannot find file MPTABLE.TBL")
    endif

    if ( trim(DATASET_IDENTIFIER) == "USGS" ) then
       read(15,usgs_veg_categories)
       read(15,usgs_veg_parameters)
    else if ( trim(DATASET_IDENTIFIER) == "MODIFIED_IGBP_MODIS_NOAH" ) then
       read(15,modis_veg_categories)
       read(15,modis_veg_parameters)
    else
       write(*,'("WARNING: DATASET_IDENTIFIER = ''", A, "''")') trim(DATASET_IDENTIFIER)
       call handle_err(ierr, 'ParametersRead.f90: read_veg_parameters: Unrecognized DATASET_IDENTIFIER in subroutine read_VEG_PARAMETERS')
    endif
    close(15)

       ISURBAN_TABLE   = ISURBAN
       ISWATER_TABLE   = ISWATER
      ISBARREN_TABLE   = ISBARREN
         ISICE_TABLE   = ISICE
        ISCROP_TABLE   = ISCROP
     EBLFOREST_TABLE   = EBLFOREST
       NATURAL_TABLE   = NATURAL
         LCZ_1_TABLE   = LCZ_1
         LCZ_2_TABLE   = LCZ_2
         LCZ_3_TABLE   = LCZ_3
         LCZ_4_TABLE   = LCZ_4
         LCZ_5_TABLE   = LCZ_5
         LCZ_6_TABLE   = LCZ_6
         LCZ_7_TABLE   = LCZ_7
         LCZ_8_TABLE   = LCZ_8
         LCZ_9_TABLE   = LCZ_9
         LCZ_10_TABLE  = LCZ_10
         LCZ_11_TABLE  = LCZ_11

      HS_TABLE(1:NVEG)  = HS(1:NVEG)
      TOPT_TABLE(1:NVEG)  = TOPT(1:NVEG)
     RSMAX_TABLE(1:NVEG)  = RSMAX(1:NVEG)

    ! Put LAI and SAI into 2d array from monthly lines in table; same for canopy radiation properties

    SAIM_TABLE(1:NVEG, 1) = SAI_JAN(1:NVEG)
    SAIM_TABLE(1:NVEG, 2) = SAI_FEB(1:NVEG)
    SAIM_TABLE(1:NVEG, 3) = SAI_MAR(1:NVEG)
    SAIM_TABLE(1:NVEG, 4) = SAI_APR(1:NVEG)


  END SUBROUTINE read_veg_parameters




  subroutine read_global_parameters(param_dir, noahowp_table)
    implicit none
    character(len=*), intent(in) :: param_dir
    character(len=*), intent(in) :: noahowp_table
    integer                      :: ierr
    logical                      :: file_named

    real :: CO2,O2,TIMEAN,FSATMX,Z0SNO,SSI,SNOW_RET_FAC,SNOW_EMIS,&
            SWEMX,TAU0,GRAIN_GROWTH,EXTRA_GROWTH,DIRT_SOOT,&
            BATS_COSZ,BATS_VIS_NEW,BATS_NIR_NEW,BATS_VIS_AGE,BATS_NIR_AGE,BATS_VIS_DIR,BATS_NIR_DIR,&
            RSURF_SNOW,RSURF_EXP

    namelist / global_parameters / CO2,O2,TIMEAN,FSATMX,Z0SNO,SSI,SNOW_RET_FAC,SNOW_EMIS,&
            SWEMX,TAU0,GRAIN_GROWTH,EXTRA_GROWTH,DIRT_SOOT,&
            BATS_COSZ,BATS_VIS_NEW,BATS_NIR_NEW,BATS_VIS_AGE,BATS_NIR_AGE,BATS_VIS_DIR,BATS_NIR_DIR,&
            RSURF_SNOW,RSURF_EXP


    ! Initialize our variables to bad values, so that if the namelist read fails, we come to a screeching halt as soon as we try to use anything.
           CO2_TABLE     = -1.E36
            O2_TABLE     = -1.E36
        TIMEAN_TABLE     = -1.E36
        FSATMX_TABLE     = -1.E36
         Z0SNO_TABLE     = -1.E36
           SSI_TABLE     = -1.E36
    SNOW_RET_FAC_TABLE   = -1.E36
       SNOW_EMIS_TABLE   = -1.E36
           SWEMX_TABLE   = -1.E36
            TAU0_TABLE   = -1.E36
    GRAIN_GROWTH_TABLE   = -1.E36
    EXTRA_GROWTH_TABLE   = -1.E36
       DIRT_SOOT_TABLE   = -1.E36
       BATS_COSZ_TABLE   = -1.E36
    BATS_VIS_NEW_TABLE   = -1.E36
    BATS_NIR_NEW_TABLE   = -1.E36
    BATS_VIS_AGE_TABLE   = -1.E36
    BATS_NIR_AGE_TABLE   = -1.E36
    BATS_VIS_DIR_TABLE   = -1.E36
    BATS_NIR_DIR_TABLE   = -1.E36
    RSURF_SNOW_TABLE     = -1.E36
     RSURF_EXP_TABLE     = -1.E36

    inquire( file=trim(param_dir)//'/'//trim(noahowp_table), exist=file_named )
    if ( file_named ) then
      open(15, file=trim(param_dir)//'/'//trim(noahowp_table), status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if

    if (ierr /= 0) then
       call handle_err(ierr, 'ParametersRead.f90: read_global_parameters: Cannot find file MPTABLE.TBL')
    endif

    read(15,global_parameters)
    close(15)

           CO2_TABLE     = CO2
            O2_TABLE     = O2
        TIMEAN_TABLE     = TIMEAN
        FSATMX_TABLE     = FSATMX
         Z0SNO_TABLE     = Z0SNO
           SSI_TABLE     = SSI
    SNOW_RET_FAC_TABLE   = SNOW_RET_FAC
       SNOW_EMIS_TABLE   = SNOW_EMIS
         SWEMX_TABLE     = SWEMX
            TAU0_TABLE   = TAU0
    GRAIN_GROWTH_TABLE   = GRAIN_GROWTH
    EXTRA_GROWTH_TABLE   = EXTRA_GROWTH
       DIRT_SOOT_TABLE   = DIRT_SOOT
       BATS_COSZ_TABLE   = BATS_COSZ
    BATS_VIS_NEW_TABLE   = BATS_VIS_NEW
    BATS_NIR_NEW_TABLE   = BATS_NIR_NEW
    BATS_VIS_AGE_TABLE   = BATS_VIS_AGE
    BATS_NIR_AGE_TABLE   = BATS_NIR_AGE
    BATS_VIS_DIR_TABLE   = BATS_VIS_DIR
    BATS_NIR_DIR_TABLE   = BATS_NIR_DIR
    RSURF_SNOW_TABLE     = RSURF_SNOW
     RSURF_EXP_TABLE     = RSURF_EXP

  END SUBROUTINE read_global_parameters



  SUBROUTINE handle_err(err,message)
    implicit none
    integer,     intent(in) :: err             ! error code
    character(*),intent(in) :: message         ! error message
    if(err/=0)then
      write(*,*) 'FATAL ERROR: '//trim(message)
      call flush(6)
      stop
    endif
  END SUBROUTINE handle_err

END MODULE parametersModule
