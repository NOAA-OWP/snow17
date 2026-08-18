module defNamelist

implicit none

  ! variable definitions
  character(len = 20)     :: main_id              ! ID string used for main/combined output
  integer 		          :: n_hrus		          ! number of HRU areas in parameter files
  character(len = 1024)   :: forcing_root	      ! base name of forcing data file root
  character(len = 1024)   :: output_root		  ! base name for output files
  character(len = 1024)   :: snow17_param_file	  ! name for snow17 parameters
  character(len = 1024)	  :: snow_state_out_root  ! name for snow17 state output root
  character(len = 1024)	  :: snow_state_in_root	  ! name for snow17 state input root
  integer                 :: model_timestep       ! model timestep in seconds
  integer 		          :: output_hrus 		  ! output HRU results? (1=yes; 0=no)
  character(len = 10)     :: start_datehr	      ! YYYYMMDDHH
  character(len = 10)     :: end_datehr	          ! YYYYMMDDHH
  integer 		          :: warm_start_run	      ! warm restart run flag
  integer 		          :: write_states	      ! flag to write states for a warm start run
  
  ! namelist elements to be shared
  namelist / SNOW17_CONTROL / forcing_root, output_root, main_id, n_hrus, output_hrus, &
                start_datehr, end_datehr, &
                snow17_param_file, warm_start_run, write_states, model_timestep, &
                snow_state_in_root, snow_state_out_root
  save

end module defNamelist

module namelistModule
use snow_log_module
implicit none

type, public :: namelist_type

  ! namelist variables
  character(len = 20)   :: main_id              ! ID string used for main/combined output
  integer		        :: n_hrus		        ! number of HRU areas in parameter files
  character(len = 1024) :: forcing_root	    	! base name of forcing data file root
  character(len = 1024) :: output_root		    ! base name for output files
  character(len = 1024) :: snow17_param_file	! name for snow17 parameters
  character(len = 1024)	:: snow_state_in_root	! name for snow17 state input root
  character(len = 1024)	:: snow_state_out_root  ! name for snow17 state output root
  integer               :: model_timestep       ! model timestep in seconds
  integer		        :: output_hrus 		    ! output HRU results? (1=yes; 0=no)
  character(len = 10)   :: start_datehr	        ! YYYYMMDDHH
  character(len = 10)   :: end_datehr	        ! YYYYMMDDHH
  integer		        :: warm_start_run	    ! warm restart run flag
  integer		        :: write_states	        ! flag to write states for a warm start run
  
  contains

    procedure, public  :: readNamelist

end type namelist_type

contains

  subroutine readNamelist(this, namelist_file)
    use defNamelist
    implicit none
   
    class(namelist_type) :: this
    ! Optional namelist_file path/filename to read
    ! if not given, the program looks for 'namelist.input' in run directory as a default
    character(len=*), intent (in), optional :: namelist_file
    integer :: ios
    ios = 0

    call write_log("readNameList : reading namelist", LOG_LEVEL_INFO)

    ! -- open and read namelist file
    open(33, file=namelist_file, form="formatted", IOSTAT=ios)
    if (ios /= 0) then
      call write_log('Error opening namelist file ' // namelist_file, LOG_LEVEL_FATAL)
    end if

    read(33, SNOW17_CONTROL)
    close(33)
    
    call write_log('readNameList -- simulating basin ' // main_id // ' with ' // itoa(n_hrus) // ' snowbands', LOG_LEVEL_INFO)
    ! -- transfer to namelist datatype
    this%main_id             = main_id
    this%n_hrus              = n_hrus
    this%forcing_root        = forcing_root
    this%output_root         = output_root
    this%output_hrus         = output_hrus
    this%start_datehr        = start_datehr
    this%end_datehr          = end_datehr
    this%snow17_param_file   = snow17_param_file
    this%warm_start_run      = warm_start_run
    this%write_states        = write_states
    this%model_timestep      = model_timestep
    this%snow_state_out_root = snow_state_out_root
    this%snow_state_in_root  = snow_state_in_root
    
    ! -- namelist entry checks --
    if (this%warm_start_run .eq. 1 .and. this%write_states .eq. 1) then
      this%write_states = 0
      call write_log("readNameList - cannot read and write state files at the same time.", LOG_LEVEL_WARNING)
      call write_log("Setting write_states option to 0 and continuing", LOG_LEVEL_WARNING)
    endif

  end subroutine readNamelist

end module namelistModule
