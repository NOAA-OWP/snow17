! ====================================================================================
! Driver for the Snow17 model for multiple HRUs within a basin and writes zone and areal mean output
!
! Adapted by A. Wood (2022) from the NCAR Snow17SacUH codebase (https://github.com/NCAR/NWS_hydro_models) 
!   that was formulated from Office of Hydrology FORTRAN source files obtained circa 2012-2013
! 
! Feb 2022:  Reformulated to use BMI at driver level with compiler options for running in NextGen framework
!
! ====================================================================================

program multi_driver
  
  !---------------------------------------------------------------------
  !  Modules
  !  Only the BMI modules need to be exposed
  !---------------------------------------------------------------------
  use bmi_snow17
  use bmif_2_0

  implicit none

  !---------------------------------------------------------------------
  !  Types (only the bmi_snow17 type needed)
  !---------------------------------------------------------------------
  type (bmi_snow17)  :: m

  !---------------------------------------------------------------------
  !  Local variable(s) 
  !---------------------------------------------------------------------
  character (len = 400) :: namelist_file    ! command line argument for namelist file
  double precision      :: current_time     ! current run time of model in s from beginning
  double precision      :: end_time         ! end of model simulation time in s
  integer               :: status           ! returns status values (function return codes)

  !---------------------------------------------------------------------
  !  Initialize
  !  Call the initialize_from_file() subroutine in src/RunSnow17.f90
  !---------------------------------------------------------------------
  print*, "Initializing..."
  call get_command_argument(1, namelist_file)
  if( .not. ( present(namelist_file) ) ) then
    namelist_file = "namelist.input"
    print*, 'No namelist filename supplied -- attempting to read namelist.input (default)'
  endif  
  status = m%initialize(namelist_file)

  !---------------------------------------------------------------------
  ! Run the model
  ! Update calls advance_in_time() subroutine in ../src/RunSnow17.f90
  !---------------------------------------------------------------------
  status = m%get_current_time(current_time)
  status = m%get_end_time(end_time)
  print*, end_time
  
  ! loop through while current time <= end time
  print*, "Running..."
  do while (current_time < end_time)
    status = m%update()                       ! run the model one time step
    status = m%get_current_time(current_time) ! update current_time
  end do

  !---------------------------------------------------------------------
  ! Finalize the model run
  ! All model finalization code in ../src/RunSnow17.f90
  !---------------------------------------------------------------------
  print*, "Finalizing..."
  status = m%finalize()
  print*, "DONE"

end program
