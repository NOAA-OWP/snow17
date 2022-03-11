module AsciiReadModule
  
  use UtilitiesModule
  
  implicit none
  
contains

  ! ==== Open forcings files and read past header
  SUBROUTINE open_init_forcing_files(namelist, runinfo, parameters)
    implicit none
    type (namelist_type),    intent(in)   :: namelist
    type (runinfo_type),     intent(in)   :: runinfo
    type (parameters_type),  intent(in)   :: parameters
    
    ! local variables
    character*256  :: filename
    logical        :: lexist ! logical for whether the file specified by filename exists
    integer        :: ierr   ! error code returned by open(iostat = ierr)
    integer        :: nh     ! loop counter

    ! --- code ------------------------------------------------------------------
    do nh=1, runinfo%n_hrus

      ! make filename to read
      filename = trim(namelist%forcing_root) // trim(parameters%hru_id(nh))	

      !  Check if the specified file exists
      inquire(file = trim(filename), exist = lexist)
      if (.not. lexist) then
         write(*,'(/," ***** Problem *****")')
         write(*,'(" ***** File ''", A, "'' does not exist.")') trim(filename)
         write(*,'(" ***** Check the forcing file specified as a command-line argument",/)')
         stop ":  ERROR EXIT"
      endif
    
      ! Open the forcing file 
      open(runinfo%forcing_fileunits(nh), file = trim(filename), form = 'formatted', action = 'read', iostat = ierr)
      if (ierr /= 0) then
         write(*,'("Problem opening file ''", A, "''")') trim(filename)
         stop ":  ERROR EXIT"
      endif
      
      ! Skip 1-line header
      read(runinfo%forcing_fileunits(nh), *)

    end do   ! end loop over sub-units
    
  END SUBROUTINE open_init_forcing_files

  ! ==== Open output files and write header
  SUBROUTINE open_init_output_files(namelist, runinfo, parameters)
    implicit none
    type (namelist_type),    intent(in)   :: namelist
    type (runinfo_type),     intent(in)   :: runinfo
    type (parameters_type),  intent(in)   :: parameters
    
    ! local variables
    character*256  :: filename
    logical        :: lexist ! logical for whether the file specified by filename exists
    integer        :: ierr   ! error code returned by open(iostat = ierr)
    integer        :: nh     ! loop counter

    ! --- code ------------------------------------------------------------------
    do nh=1, runinfo%n_hrus

      ! make filename to read
      filename = trim(namelist%output_root) // trim(parameters%hru_id(nh))	

      !  Check if the specified file exists
      !inquire(file = trim(filename), exist = lexist)
      !if (.not. lexist) then
      !   write(*,'(/," ***** Problem *****")')
      !   write(*,'(" ***** File ''", A, "'' does not exist.")') trim(filename)
      !   write(*,'(" ***** Check the forcing file specified as a command-line argument",/)')
      !   stop ":  ERROR EXIT"
      !endif
    
      ! Open the forcing file 
      open(runinfo%output_fileunits(nh), file = trim(filename), form = 'formatted', action = 'write', status='replace', iostat = ierr)
      if (ierr /= 0) then
         write(*,'("Problem opening file ''", A, "''")') trim(filename)
         stop ":  ERROR EXIT"
      endif
      
      ! Write 1-line header
      write(runinfo%output_fileunits(nh),'(A)') 'year mo dy hr tair precip precip*scf sneqv raim '

    end do   ! end loop over sub-units
    
  END SUBROUTINE  open_init_output_files


! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% !
subroutine read_snow17_state(state_date_str, cs,tprev,curr_hru_id)
  use nrtype
  use def_namelists, only: snow_state_in_root
  implicit none

  ! input variables
  character(len=10),intent(in)	:: state_date_str  ! AWW string to match date in input states
  character(len = 20), intent(in) 	:: curr_hru_id	! HRU extension for snow state fname

  ! output variables
  real(sp), intent(out) 		:: tprev	! carry over variable
  real(sp), dimension(:), intent(out)	:: cs		! carry over array

  !local variables
  integer(I4B)	       :: ios=0
  character(len = 480) :: state_infile
  character(len = 10)  :: file_state_date_str

  ! make state filename
  state_infile = trim(snow_state_in_root) // trim(curr_hru_id)
  open(unit=95,FILE=trim(state_infile),FORM='formatted',status='old')
  print*, 'Reading snow state file: ', trim(state_infile)

  ! format for input is an unknown number of rows with 20 data columns (1 tprev, 19 for cs)
  !   the first column is the datestring
  do while(ios .ge. 0)

    ! read each row and check to see if the date matches the initial state date
    read(95,*,IOSTAT=ios) file_state_date_str, tprev, cs(:)

    ! checks either for real date or special word identifying the state to use
    !   this functionality facilitates ESP forecast initialization
    if(file_state_date_str==state_date_str .or. file_state_date_str=='PseudoDate') then
      print *, '  -- found initial snow state on ', state_date_str
      close(unit=95)
      return
    end if

  end do
  close(unit=95)

  ! if you reach here without returning, quit -- the initial state date was not found
  print*, 'ERROR:  snow init state not found in snow initial state file.  Looking for: ',state_date_str
  print*, '  -- last state read was: ', file_state_date_str
  print*, 'Stopping.  Check inputs!'
  stop

end subroutine read_snow17_state

! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% !
subroutine write_snow17_state(year, month, day, hour, cs, tprev, sim_length, curr_hru_id)
  use nrtype
  use def_namelists, only: snow_state_out_root
  implicit none

  !input variables
  character(len = 20), intent(in) 	:: curr_hru_id	! HRU extension for state fname
  integer(I4B),dimension(:),intent(in)	:: year
  integer(I4B),dimension(:),intent(in)	:: month
  integer(I4B),dimension(:),intent(in)	:: day
  integer(I4B),dimension(:),intent(in)	:: hour
  real(sp),dimension(:,:),intent(in)	:: cs	    ! carry over array
  real(sp),dimension(:),intent(in)	    :: tprev    ! carry over variable
  integer(I4B),intent(in)               :: sim_length   ! length of simulation

  !local variables
  integer(I4B)	:: i
  character(len = 480) :: state_outfile

  ! make state input filename
  state_outfile = trim(snow_state_out_root) // trim(curr_hru_id)

  open(unit=95,FILE=trim(state_outfile),FORM='formatted',status='replace')
  print*, 'Writing snow state file: ', trim(state_outfile)

  41 FORMAT(I0.4, 3(I0.2), 20(F20.12))  ! big enough to separate fields
  do i = 1,sim_length
    ! print*, 'tprev = ',tprev(i)  AWW debugging

    write(95,41) year(i),month(i),day(i),hour(i),tprev(i), cs(:,i)
  enddo

  close(unit=95)

  return
end subroutine write_snow17_state


! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% !
! AWW modified to read PET instead of dayl, vpd and swdown
! AWW modified to return basin area in sq km
subroutine read_areal_forcing(year,month,day,hour,tmin,tmax,precip,pet,curr_hru_id)
  use nrtype
  use def_namelists, only: forcing_root, start_year,start_day,start_month, &
                        end_year,end_month,end_day

  implicit none

  ! input variables
  character(len = 20), intent(in) 	:: curr_hru_id	! HRU extension for sac state fname

  ! output variables
  integer(I4B),dimension(:),intent(out)	:: year
  integer(I4B),dimension(:),intent(out)	:: month
  integer(I4B),dimension(:),intent(out)	:: day
  integer(I4B),dimension(:),intent(out)	:: hour
  real(dp),dimension(:),intent(out)	:: tmax   ! deg C
  real(dp),dimension(:),intent(out)	:: tmin    ! deg C
  real(dp),dimension(:),intent(out)	:: precip  ! mm/day
  real(dp),dimension(:),intent(out)	:: pet   ! mm/day


  ! local variables
  integer(I4B)				:: i,ios=0
  integer(I4B)				:: yr,mnth,dy,hr
  integer(I4B)				:: read_flag
  character(len = 1024)			:: dum_str
  real(DP)				:: pcp,tma,tmn,pm_pet

  character(len = 420) :: filename

  ! make filename to read
  filename = trim(forcing_root) // trim(curr_hru_id)	

  ! =========  code below  =============
  i = 1
  read_flag = 0

  ! read met file
  open (UNIT=50,file=trim(filename),form='formatted',status='old')

  ! skip header info
  read (UNIT=50,FMT='(80A)') dum_str   ! column labels

  ! read the data, keeping only forcings in simulation period
  do while(ios .ge. 0 .and. read_flag < 2)
    ! forcing could have any format, nice!
    read (UNIT=50,FMT=*,IOSTAT=ios) yr,mnth,dy,hr,pcp,tma,tmn,pm_pet

    if(yr .eq. start_year .and. mnth .eq. start_month .and. dy .eq. start_day) then
      read_flag = 1
    end if

    ! read and store data for simulation period
    if(read_flag .eq. 1) then
      year(i)	= yr
      month(i)	= mnth
      day(i)	= dy
      hour(i)	= hr
      precip(i)	= pcp
      tmax(i)	= tma
      tmin(i)	= tmn
      pet(i)	= pm_pet
      i = i + 1
    end if

    if(yr .eq. end_year .and. mnth .eq. end_month .and. dy .eq. end_day) then
      read_flag = 2
    end if

  end do

  close(unit=50)
  return
end subroutine read_areal_forcing

! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% !

  
  
  
  
  subroutine read_forcing_text(iunit, nowdate, forcing_timestep, &
    u, v, sfctmp, spechumd, sfcprs, swrad, lwrad, pcprate, ierr)
    
    implicit none

    ! Input
    integer,           intent(in)  :: iunit
    character(len=12), intent(in)  :: nowdate
    integer,           intent(in)  :: forcing_timestep

    ! Output
    real,              intent(out) :: sfctmp
    real,              intent(out) :: spechumd
    real,              intent(out) :: sfcprs
    real,              intent(out) :: swrad
    real,              intent(out) :: lwrad
    real,              intent(out) :: pcprate
    integer,           intent(out) :: ierr
    real,              intent(out) :: u
    real,              intent(out) :: v

    ! Local
    real              :: wspd 
    integer           :: year
    integer           :: month
    integer           :: day
    integer           :: hour
    integer           :: minute
    character(len=12) :: readdate
    real              :: read_windspeed
    real              :: read_winddir
    real              :: read_temperature
    real              :: read_pressure
    real              :: read_humidity
    real              :: read_swrad
    real              :: read_lwrad
    real              :: read_rain
    real              :: wdir

    type fdata
       character(len=12) :: readdate
       real              :: windspeed
       real              :: winddir
       real              :: temperature
       real              :: humidity
       real              :: pressure
       real              :: swrad
       real              :: lwrad
       real              :: rain
    end type fdata

    type(fdata) :: before = fdata("000000000000", -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36 ) 
    type(fdata) :: after  = fdata("000000000000", -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36 ) 
      
    integer :: idts
    integer :: idts2
    real    :: fraction

    real    :: svp ! Saturation Vapor pressure, computed herein as a function of Temperature
    real    :: e   ! Water Vapor Pressure, computed herein as a function of Temperature, Pressure, and Relative Humidity
    real    :: rhf ! Relative humidity expressed as a fraction [ 0.0 to 1.0 ]
    real    :: qs  ! Saturation specific humidity [ kg kg{-1} ]

    ! Parameters used to compute Saturation Vapor Pressure as a function of Temperature
    real, parameter :: svp1  = 611.2
    real, parameter :: svp2  = 17.67
    real, parameter :: svp3  = 29.65
    real, parameter :: svpt0 = 273.15

    ! Parameter used to compute Specific Humidity from Pressure and Saturation Vapor Pressure.
    real, parameter :: eps   = 0.622

    character(len=1024) :: string

    ! Flag to tell us whether this is the first time this subroutine is called, in which case
    ! we need to seek forward to the data.
    logical :: FirstTime = .TRUE.

    ! The format string for reading the forcing data:
    character(len=64), parameter :: read_format = "(I4.4, 4(1x,I2.2),8(F17.10))"

    real, parameter :: pi = 3.14159265

    ! First time in, skip forward, positioning ourself at the beginning of the data.
    if ( FirstTime ) then
       FirstTime = .FALSE.
       do
          read(iunit, '(A1024)') string
          string = upcase(adjustl(string))
          if (string(1:9) == "<FORCING>") exit
       enddo
    endif

    ! Wind Speed in this file is m s{-1}
    ! Wind direction in this file is degrees from north.
    ! Temperature in this file is in Degrees C.
    ! Humidity in this file is Relative Humidity, in % (i.e., between 0 and 100+).
    ! Pressure in this file is in mb.
    ! Incoming Short-wave Radiation in this file is in W m{-2}
    ! Incoming Long-wave Radiation in this file is in W m{-2}
    ! Precipitation rate in this file is in Inches per forcing timestep

    READLOOP : do

       ! If our dates in storage are already bracketing NOWDATE, we don't have to
       ! read anything; we can just exit.
       if (before%readdate <= nowdate .and. nowdate <= after%readdate) exit READLOOP

       ! But if we do have to read data, let's read some data!
       read(UNIT=iunit, FMT=read_format, IOSTAT=ierr) &
            year, month, day, hour, minute, &
            read_windspeed,   &
            read_winddir,     &
            read_temperature, &
            read_humidity,    &
            read_pressure,    &
            read_swrad,       &
            read_lwrad,       &
            read_rain
       if (ierr < 0) then
          !KWM write(*,'("Hit the end of file.")')
          ierr = 1

          before = fdata("000000000000", -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36 ) 
          after  = fdata("000000000000", -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36 ) 
          FirstTime = .TRUE.

          return
       endif
       if (ierr /= 0) then
          write(*,'("Error reading from data file.")')
          ierr = 2
          return
       endif
       write(readdate,'(I4.4,4I2.2)') year, month, day, hour, minute

       if ( readdate > nowdate ) then
          ! After becomes before, and then we have a new before
          if (after%readdate > "000000000000" ) before = after
          after = fdata ( readdate, read_windspeed, read_winddir, read_temperature, read_humidity, read_pressure, read_swrad, read_lwrad, read_rain )
          exit READLOOP
       else if (readdate == nowdate) then
          before = fdata ( readdate, read_windspeed, read_winddir, read_temperature, read_humidity, read_pressure, read_swrad, read_lwrad, read_rain )
          exit READLOOP
       else if (readdate < nowdate) then
          before = fdata ( readdate, read_windspeed, read_winddir, read_temperature, read_humidity, read_pressure, read_swrad, read_lwrad, read_rain )
          cycle READLOOP
       else
          stop "Logic problem"
       endif
    enddo READLOOP

    if (before%readdate == nowdate) then

       pcprate = before%rain                              ! No conversion necessary
       sfctmp  = before%temperature                       ! No conversion necessary
       sfcprs  = before%pressure*1.E2                     ! Convert pressure from mb to Pa
       wspd    = before%windspeed                         ! No conversion necessary
       wdir    = before%winddir                           ! No conversion necessary
       swrad   = before%swrad                             ! No conversion necessary
       lwrad   = before%lwrad                             ! No conversion necessary
       rhf     = before%humidity * 1.E-2                  ! Convert Relative Humidity from percent to fraction

    else if (after%readdate == nowdate) then

       pcprate = after%rain                              ! No conversion necessary
       sfctmp  = after%temperature                       ! No conversion necessary
       sfcprs  = after%pressure*1.E2                     ! Convert pressure from mb to Pa
       wspd    = after%windspeed                         ! No conversion necessary
       wdir    = after%winddir                           ! No conversion necessary
       swrad   = after%swrad                             ! No conversion necessary
       lwrad   = after%lwrad                             ! No conversion necessary
       rhf     = after%humidity * 1.E-2                  ! Convert Relative Humidity from percent to fraction
     
       before = after
       after  = fdata("000000000000", -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36 )        

    else if (before%readdate < nowdate .and. nowdate < after%readdate) then

       call geth_idts(nowdate, before%readdate, idts)
       call geth_idts(after%readdate, before%readdate, idts2)

       if (idts2*60 /= forcing_timestep) then
          print*, 'forcing_timestep = ', forcing_timestep
          print*,' nowdate = ', nowdate
          print*, 'before%readdate = ', before%readdate
          print*, 'idts = ', idts
          print*,' after%readdate = ', after%readdate
          print*, 'idts2 = ', idts2
          stop "IDTS PROBLEM"
       endif

       fraction = real(idts2-idts)/real(idts2)

       pcprate = before%rain  ! Precip rate is not interpolated, but carried forward.

       sfctmp = ( before%temperature * fraction )  + ( after%temperature * ( 1.0 - fraction ) )

       sfcprs = ( before%pressure * fraction ) + ( after%pressure * ( 1.0 - fraction ) )
       sfcprs = sfcprs * 1.E2

       wspd = ( before%windspeed * fraction ) + ( after%windspeed * ( 1.0 - fraction ) )

       wdir = ( before%winddir * fraction ) + ( after%winddir * ( 1.0 - fraction ) )

       swrad = ( before%swrad * fraction ) + ( after%swrad * ( 1.0 - fraction ) )

       lwrad = ( before%lwrad * fraction ) + ( after%lwrad * ( 1.0 - fraction ) )

       rhf = ( before%humidity * fraction ) + ( after%humidity * ( 1.0 - fraction ) )
       rhf = rhf * 1.E-2

    else
       print*, 'nowdate = "'//nowdate//'"'
       stop "Problem in the logic of read_forcing_text."
    endif

    
  end subroutine read_forcing_text
  
  
  
  character(len=256) function upcase(h) result(return_string)
    implicit none
    character(len=*), intent(in) :: h
    integer :: i
    
    return_string = ""

    do i = 1, len_trim(h)

       if ((ichar(h(i:i)).ge.96) .and. (ichar(h(i:i)).le.123)) then
          return_string(i:i) = char(ichar(h(i:i))-32)
       else
          return_string(i:i) = h(i:i)
       endif
    enddo

  end function upcase
  
end module AsciiReadModule
