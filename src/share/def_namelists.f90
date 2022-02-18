! A.Wood-2016:  large edits for distributed SAC
module def_namelists
  use nrtype
  implicit none

  ! initialization variables
  ! these are in the namelist bloc &INIT_CONTROL
  character(len = 20)   :: main_id              ! ID string used for main/combined output
  integer(I4B)		    :: n_hrus		        ! number of HRU areas in parameter files
  character(len = 1024) :: forcing_root	    	! base name of forcing data file root
  character(len = 1024) :: output_root		    ! base name for output files
  character(len = 1024) :: snow17_param_file	! name for snow17 parameters
  character(len = 1024)	:: snow_state_out_root  ! name for snow17 state output root
  character(len = 1024)	:: sac_state_out_root	! name for sac state output root
  character(len = 1024)	:: snow_state_in_root	! name for snow17 state input root
  
  integer(I4B)		:: output_hrus 		! output HRU results? (1=yes; 0=no)
  integer(I4B)		:: start_month		! starting month 
  integer(I4B)		:: start_day		! starting day
  integer(I4B)		:: start_year		! starting year
  integer(I4B)		:: end_month		! ending month 
  integer(I4B)		:: end_day		! ending day
  integer(I4B)		:: end_year		! ending year
  integer(I4B)		:: warm_start_run	! warm restart run flag
  integer(I4B)		:: write_states	        ! flag to write states for a warm start run

  real(sp)		    :: init_swe             ! initial states if for a cold start run
                                            ! tprev ??

  ! Snow17_model params in the snow param file
  character(len = 20), dimension(:), allocatable :: hru_id   ! local hru id
  real(dp), dimension(:), allocatable :: hru_area   ! sq-km, needed for combination & routing conv.
  real(dp), dimension(:), allocatable :: latitude   ! decimal degrees
  real(dp), dimension(:), allocatable :: elev       ! m
  real(sp), dimension(:), allocatable :: scf,mfmax,mfmin,uadj,si,pxtemp
  real(sp), dimension(:), allocatable :: nmf,tipm,mbase,plwhc,daygm
  real(sp), dimension(11)             :: adc  ! AW can we keep this the same for all HUCs, for now?

  ! namelist elements to be shared
  namelist / INIT_CONTROL / forcing_root, output_root, main_id, n_hrus, output_hrus, &
                          start_day, start_month, start_year, end_year, end_month, &
                          end_day, init_swe, snow17_param_file, warm_start_run, write_states, &
			              snow_state_out_root,snow_state_in_root, elev, latitude, hru_id, hru_area
  save
end module
