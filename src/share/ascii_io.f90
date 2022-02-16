! A. Wood, Aug 2016:  These are all reconfigured to work with multi-HRU model setup
! AWW 2022:  updated to move hru_id and area read into snow17 param files

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
subroutine read_snow17_params(param_file_name, n_hrus)
  use nrtype
  use def_namelists, only: scf,mfmax,mfmin,uadj,si,pxtemp,nmf,&
                        tipm,mbase,plwhc,daygm,adc,latitude, elev,&
                        hru_id, hru_area
  implicit none
 
  !input variables
  character(len=1024),intent(in) :: param_file_name
  integer(I4B),intent(in) :: n_hrus

  !local variables
  character(len=400)            :: readline
  character(len=50)		:: param
  integer(I4B)			:: ios=0
  integer :: pos
  integer                        :: n_params_read  ! count number read

  ! open parameter file
  open(unit=51,file=trim(param_file_name),status='old')

  ! allocate parameter variables
  allocate(hru_id(n_hrus))
  allocate(hru_area(n_hrus))
  allocate(latitude(n_hrus))
  allocate(elev(n_hrus))
  allocate(scf(n_hrus))
  allocate(mfmax(n_hrus))
  allocate(mfmin(n_hrus))
  allocate(uadj(n_hrus))
  allocate(si(n_hrus))
  allocate(pxtemp(n_hrus))
  allocate(nmf(n_hrus))
  allocate(tipm(n_hrus))
  allocate(mbase(n_hrus))
  allocate(plwhc(n_hrus))
  allocate(daygm(n_hrus))

  print*, 'Reading Snow17 parameters'

  ! --- now loop through parameter file and assign parameters 
  n_params_read = 0
  do while(ios .eq. 0)
    read(unit=51,FMT='(A)',IOSTAT=ios) readline

    if(ios == 0) then   ! means 'readline' was from the file
      !print*, '  ',trim(readline)

      ! Find the first instance of whitespace in line read. Split label vs data.
      pos = scan(readline, '    ')
      param = trim(readline(1:pos))
      readline = readline(pos+1:)  ! shorten readline to include only data

      ! assign line to correct parameter array & type
      ! (following http://jblevins.org/log/control-file)
      select case (param)
       case ('hru_id')
          read(readline, *, iostat=ios) hru_id
          n_params_read = n_params_read + 1
        case ('hru_area')
          read(readline, *, iostat=ios) hru_area
          n_params_read = n_params_read + 1
        case ('latitude')
          read(readline, *, iostat=ios) latitude
          n_params_read = n_params_read + 1
        case ('elev')
          read(readline, *, iostat=ios) elev
          n_params_read = n_params_read + 1
        case ('mfmax')
          read(readline, *, iostat=ios) mfmax
          n_params_read = n_params_read + 1
        case ('mfmin')
          read(readline, *, iostat=ios) mfmin
          n_params_read = n_params_read + 1
        case ('scf')
          read(readline, *, iostat=ios) scf
          n_params_read = n_params_read + 1
        case ('uadj')
          read(readline, *, iostat=ios) uadj
          n_params_read = n_params_read + 1
        case ('si')
          read(readline, *, iostat=ios) si
          n_params_read = n_params_read + 1
        case ('pxtemp')
          read(readline, *, iostat=ios) pxtemp
          n_params_read = n_params_read + 1
        case ('nmf')
          read(readline, *, iostat=ios) nmf
          n_params_read = n_params_read + 1
        case ('tipm')
          read(readline, *, iostat=ios) tipm
          n_params_read = n_params_read + 1
        case ('mbase')
          read(readline, *, iostat=ios) mbase
          n_params_read = n_params_read + 1
        case ('plwhc')
          read(readline, *, iostat=ios) plwhc
          n_params_read = n_params_read + 1
        case ('daygm')
          read(readline, *, iostat=ios) daygm
          n_params_read = n_params_read + 1
        case ('adc1')
          read(readline, *, iostat=ios) adc(1)
          n_params_read = n_params_read + 1
        case ('adc2')
          read(readline, *, iostat=ios) adc(2)
          n_params_read = n_params_read + 1
        case ('adc3')
          read(readline, *, iostat=ios) adc(3)
          n_params_read = n_params_read + 1
        case ('adc4')
          read(readline, *, iostat=ios) adc(4)
          n_params_read = n_params_read + 1
        case ('adc5')
          read(readline, *, iostat=ios) adc(5)
          n_params_read = n_params_read + 1
        case ('adc6')
          read(readline, *, iostat=ios) adc(6)
          n_params_read = n_params_read + 1
        case ('adc7')
          read(readline, *, iostat=ios) adc(7)
          n_params_read = n_params_read + 1
        case ('adc8')
          read(readline, *, iostat=ios) adc(8)
          n_params_read = n_params_read + 1
        case ('adc9')
          read(readline, *, iostat=ios) adc(9)
          n_params_read = n_params_read + 1
        case ('adc10')
          read(readline, *, iostat=ios) adc(10)
          n_params_read = n_params_read + 1
        case ('adc11')
          read(readline, *, iostat=ios) adc(11)
          n_params_read = n_params_read + 1
        case default
          print *, 'Parameter ',param,' not recognized in snow file'
      end select

    end if

  end do
  close(unit=51)

  ! quick check on completeness
  if(n_params_read /= 26) then
    print *, 'Read ', n_params_read , ' SNOW17 params, but need 26.  Quitting...'
    stop
  end if
  !print*, '  -------------------'

  return
end subroutine read_snow17_params

