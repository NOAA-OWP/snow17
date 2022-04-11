module namelistModule

implicit none
save
private

type, public :: namelist_type

  ! namelist variables
  character(len = 20)   :: main_id              ! ID string used for main/combined output
  integer(I4B)		    :: n_hrus		        ! number of HRU areas in parameter files
  character(len = 1024) :: forcing_root	    	! base name of forcing data file root
  character(len = 1024) :: output_root		    ! base name for output files
  character(len = 1024) :: snow17_param_file	! name for snow17 parameters
  character(len = 1024)	:: snow_state_out_root  ! name for snow17 state output root
  character(len = 1024)	:: sac_state_out_root	! name for sac state output root
  character(len = 1024)	:: snow_state_in_root	! name for snow17 state input root
  integer(I4B)          :: model_timestep       ! model timestep in seconds
  integer(I4B)		    :: output_hrus 		    ! output HRU results? (1=yes; 0=no)
  integer(I4B)		    :: startdatehr	        ! YYYYMMDDHH
  integer(I4B)		    :: enddatehr	        ! YYYYMMDDHH
  integer(I4B)		    :: warm_start_run	    ! warm restart run flag
  integer(I4B)		    :: write_states	        ! flag to write states for a warm start run
  
  contains

    procedure, public  :: readNamelist

end type namelist_type

contains

  subroutine readNamelist(this, namelist_file)

    class(namelist_type) :: this
    ! Optional namelist_file path/filename to read
    ! if not given, the program looks for 'namelist.input' in run directory as a default
    character(len=*), intent (in), optional :: namelist_file
    
    ! variable definitions
    character(len = 20)     :: main_id              ! ID string used for main/combined output
    integer(I4B)		    :: n_hrus		        ! number of HRU areas in parameter files
    character(len = 1024)   :: forcing_root	    	! base name of forcing data file root
    character(len = 1024)   :: output_root		    ! base name for output files
    character(len = 1024)   :: snow17_param_file	! name for snow17 parameters
    character(len = 1024)	:: snow_state_out_root  ! name for snow17 state output root
    character(len = 1024)	:: snow_state_in_root	! name for snow17 state input root
    integer(I4B)            :: model_timestep       ! model timestep in seconds
  
    integer(I4B)		:: output_hrus 		! output HRU results? (1=yes; 0=no)
    integer(I4B)		:: startdatehr	    ! YYYYMMDDHH
    integer(I4B)		:: enddatehr	    ! YYYYMMDDHH
    integer(I4B)		:: warm_start_run	! warm restart run flag
    integer(I4B)		:: write_states	    ! flag to write states for a warm start run
  
    ! namelist elements to be shared
    namelist / SNOW17_CONTROL / forcing_root, output_root, main_id, n_hrus, output_hrus, &
                  startdatehr, enddatehr, &
                  snow17_param_file, warm_start_run, write_states, model_timestep, &
			      snow_state_out_root,snow_state_in_root

    ! -- read namelist file
    if( .not. ( present(namelist_file) ) ) then
      namelist_file = "namelist.input"
      print*, 'No namelist filename supplied -- attempting to read namelist.input (default)'
    endif

    open(333, file=namelist_file_, form="formatted")
      read(333, SNOW17_CONTROL)
    close(333)

    ! -- transfer to namelist datatype
    this%main_id             = main_id
    this%n_hrus              = n_hrus
    this%forcing_root        = forcing_root
    this%output_root         = output_root
    this%output_hrus         = output_hrus
    this%startdatehr         = startdatehr
    this%enddatehr           = enddatehr
    this%snow17_param_file   = snow17_param_file
    this%warm_start_run      = warm_start_run
    this%write_states        = write_states
    this%model_timestep      = model_timestep
    this%snow_state_out_root = snow_state_out_root
    this%snow_state_in_root  = snow_state_in_root

  end subroutine readNamelist

end module namelistModule