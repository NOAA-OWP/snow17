module runInfoType

use namelistModule, only: namelist_type
use dateTimeUtilsModule

implicit none
save
!private

type, public :: runinfo_type

  ! time-space information about the model run

  !integer             :: nx                ! i indices (x dim) in grid  
  !integer             :: nj                ! j indices (y dim) in grid
  !integer             :: iloc              ! current i index in grid  
  !integer             :: jloc              ! current j index in grid (set to 1 in vector/HRU case)
  real                :: dt                ! run timestep (s)
  integer(I4B)		  :: n_hrus		        ! number of HRU areas in parameter files
  !character(len=8)    :: startdate         ! Start date of the model run ( YYYYMMDDHH ) 
  !character(len=8)    :: enddate           ! End date of the model run ( YYYYMMDDHH ) 
  !character(len=8)    :: nowdate           ! Current date of the model run ( YYYYMMDDHH ) 
  character(len=10)   :: startdatehr       ! Start date of the model run ( YYYYMMDDHH ) 
  character(len=10)   :: enddatehr         ! End date of the model run ( YYYYMMDDHH ) 
  character(len=10)   :: nowdatehr         ! Current date of the model run ( YYYYMMDDHH ) 
  real*8              :: start_datetime    ! unix start datetime (s since 1970-01-01 00:00:00) ?UTC? 
  real*8              :: end_datetime      ! unix end datetime (s since 1970-01-01 00:00:00) ?UTC? 
  real*8              :: curr_datetime     ! unix current datetime (s since 1970-01-01 00:00:00) ?UTC? 
  real*8, allocatable :: sim_datetimes(:)  ! vector of unix sim times given start/end dates and dt (try 'ki8' type)
  integer             :: itime             ! current integer time step of model run
  integer             :: ntime             ! total number of integer time steps in model run
  double precision    :: time_dbl          ! current time of model run in seconds from beginning
  
  integer             :: start_year, start_month, start_day, start_hour
  integer             :: end_year, end_month, end_day, end_hour
  integer, dimension(:), allocatable  :: forcing_fileunits  ! fileunit numbers for multi-hru forcings
  integer, dimension(:), allocatable  :: output_fileunits   ! fileunit numbers for multi-hru forcings

  contains

    procedure, public  :: Init         

end type runinfo_type

contains   

  subroutine Init(this, namelist)

    class(runinfo_type)            :: this
    type(namelist_type),intent(in) :: namelist

    ! variable allocations
    !-- sim_datetimes()?
    ! example:
    !allocate(this%zsoil (namelist%nsoil))  ; this%zsoil  (:)   = huge(1.0)
    allocate(this%forcing_fileunits(namelist%n_hrus)
    allocate(this%output_fileunits(namelist%n_hrus)
   
    ! namelist assignments
    this%dt          = namelist%model_timestep   ! in seconds
    this%startdatehr = namelist%startdatehr
    this%enddatehr   = namelist%enddatehr
    
    ! default assignments
    !this%nx        = namelist%n_hrus
    !this%nj        = 1           ! this snow17 version reads a vector model
    !this%iloc      = huge(1)
    !this%jloc      = 1           ! this snow17 version reads a vector model
    !this%nowdate   = 'EMPTYDATE999'
    this%curr_datetime  = huge(1)
    this%itime     = 1           ! initialize the time loop counter at 1
    this%time_dbl  = 0.d0        ! start model run at t = 0.0  
    
    
    ! calculated / derived variables
    !this%startdate   = namelist%startdatehr(1:8)
    !this%enddate     = namelist%enddatehr(1:8)
    this%start_year  = namelist%startdatehr(1:4)
    this%start_month = namelist%startdatehr(5:6)
    this%start_day   = namelist%startdatehr(7:8)
    this%end_year    = namelist%enddatehr(1:4)
    this%end_month   = namelist%enddatehr(5:6)
    this%end_day     = namelist%enddatehr(7:8)
    
    this%start_datetime = date_to_unix(namelist%startdatehr)  ! returns seconds-since-1970-01-01 00
    this%end_datetime   = date_to_unix(namelist%enddatehr)    
    this%ntime          = int( (this%start_datetime - this%end_datetime)/this%dt ) 
    
    do nh=1,n_hrus
      this%forcing_fileunits(nh) = 250 + nh     ! assign input and output fileunit numbers
      this%output_fileunits(nh)  = 350 + nh
    end do
    
    !forcing_timestep  = runinfo%dt        ! integer timestep for some subroutine calls

    ! time variables
    !runinfo%nowdate   = runinfo%startdate  ! start the model with nowdate = startdate
      
    !---------------------------------------------------------------------
    !--- set a time vector for simulation ---
    !---------------------------------------------------------------------
    ! --- AWW:  calculate start and end utimes & records for requested data read period ---
    !           makes unix-time list for desired records (end-of-timestep)
    !call get_utime_list (runinfo%start_datetime, runinfo%end_datetime, runinfo%dt, runinfo%sim_datetimes)  
    !runinfo%ntime = size (runinfo%sim_datetimes)   
    !print *, "---------"; 
    !print *, 'Simulation startdate = ', runinfo%startdate, ' enddate = ', runinfo%enddate, ' dt(sec) = ',&
      !runinfo%dt, ' ntimes = ', runinfo%ntime  ! YYYYMMDD dates
    !print *, "---------"

  end subroutine Init

end module runInfoType
