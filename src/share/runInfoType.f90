module runInfoType

use namelistModule, only: namelist_type
use dateTimeUtilsModule

implicit none
save
!private

type, public :: runinfo_type

  ! time-space information about the model run
  integer                             :: n_hrus		       ! number of HRU areas in parameter files
  character(len=10)                   :: start_datehr      ! Start date of the model run ( YYYYMMDDHH ) 
  character(len=10)                   :: end_datehr        ! End date of the model run ( YYYYMMDDHH ) 
  character(len=10)                   :: curr_datehr       ! Current date of the model run ( YYYYMMDDHH ) 
  
  double precision                    :: start_datetime    ! unix start datetime (s since 1970-01-01 00:00:00) ?UTC? 
  double precision                    :: end_datetime      ! unix end datetime (s since 1970-01-01 00:00:00) ?UTC? 
  double precision                    :: curr_datetime     ! unix current datetime (s since 1970-01-01 00:00:00) ?UTC? 
  double precision                    :: time_dbl          ! current time of model run in seconds from beginning
  double precision                    :: dt                ! run timestep (s)
  integer                             :: itime             ! current integer time step of model run
  integer                             :: ntime             ! total number of integer time steps in model run
  ! note: previously used 'real*8' instead of double precision
  
  integer                             :: curr_yr, curr_mo, curr_dy, curr_hr, curr_min, curr_sec  
  integer                             :: start_year, start_month, start_day, start_hour
  integer                             :: end_year, end_month, end_day, end_hour
  integer, dimension(:), allocatable  :: forcing_fileunits  ! fileunit numbers for multi-hru forcings
  integer, dimension(:), allocatable  :: output_fileunits   ! fileunit numbers for multi-hru outputs
  integer, dimension(:), allocatable  :: state_fileunits    ! fileunit numbers for multi-hru state files

  contains

    procedure, public  :: initInfo

end type runinfo_type

contains   

  subroutine initInfo(this, namelist)
  
    use defNamelist
    implicit none

    class(runinfo_type),intent(inout) :: this
    type(namelist_type),intent(in)    :: namelist
    
    ! local vars
    integer                           :: nh

    ! variable allocations
    allocate(this%forcing_fileunits(namelist%n_hrus))
    allocate(this%state_fileunits(namelist%n_hrus))
    allocate(this%output_fileunits(namelist%n_hrus+1))   ! extra file-unit for combined basin outputs
   
    ! namelist-based assignments
    this%dt           = namelist%model_timestep         ! in seconds
    this%start_datehr = namelist%start_datehr
    this%end_datehr   = namelist%end_datehr
    
    ! calculated / derived variables
    !this%start_year   = this%start_datehr(1:4)
    !this%start_month  = this%start_datehr(5:6)
    !this%start_day    = this%start_datehr(7:8)
    !this%start_hour   = this%start_datehr(9:10)
    write(this%start_year, '(I4)') this%start_datehr(1:4)
    write(this%start_month, '(I4)') this%start_datehr(5:6)
    write(this%start_day, '(I4)') this%start_datehr(7:8)
    write(this%start_hour, '(I4)') this%start_datehr(9:10)
    !this%end_year     = this%end_datehr(1:4)
    !this%end_month    = this%end_datehr(5:6)
    !this%end_day      = this%end_datehr(7:8)
    !this%curr_datehr  = this%start_datehr    
    write(this%end_year, '(I4)') this%end_datehr(1:4)
    write(this%end_month, '(I4)') this%end_datehr(5:6)
    write(this%end_day, '(I4)') this%end_datehr(7:8)
    write(this%end_hour, '(I4)') this%end_datehr(9:10)
    this%curr_yr      = this%start_year
    this%curr_mo      = this%start_month
    this%curr_dy      = this%start_day
    this%curr_hr      = this%start_hour
    this%curr_min     = 0
    this%curr_sec     = 0
    
    ! unix times
    this%start_datetime = date_to_unix(namelist%start_datehr)  ! returns seconds-since-1970-01-01 00
    this%end_datetime   = date_to_unix(namelist%end_datehr)    
    this%curr_datetime  = this%start_datetime
    this%ntime          = int( (this%start_datetime - this%end_datetime)/this%dt )

    ! other default assignments
    this%itime          = 1           ! initialize the time loop counter at 1
    this%time_dbl       = 0.d0        ! start model run at t = 0.0  
    
    ! assign input forcing and output fileunit numbers
    do nh=1,n_hrus
      this%forcing_fileunits(nh) = 250 + nh     
      this%output_fileunits(nh)  = 500 + nh       ! allows for 250 non-overlapping fileunits
      this%state_fileunits(nh)   = 750 + nh       ! allows for 250 non-overlapping fileunits
    end do
    this%output_fileunits(nh)    = 500 + nh + 1   ! add one more unit for combined ouptuts

  end subroutine initInfo

end module runInfoType
