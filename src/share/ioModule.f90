module ioModule
  
  use UtilitiesModule
  use parametersModule
  
  implicit none
  
contains

  ! ==== Open forcings files and read to start of first record
  SUBROUTINE open_and_init_forcing_files(namelist, runinfo, parameters)
    implicit none
    type (namelist_type),    intent(in)   :: namelist
    type (runinfo_type),     intent(in)   :: runinfo
    type (parameters_type),  intent(in)   :: parameters
    
    ! local variables
    character*480  :: filename
    logical        :: lexist ! logical for whether the file specified by filename exists
    integer        :: ierr=0   ! error code returned by open(iostat = ierr)
    integer        :: nh     ! loop counter
    
    ! local variables
    integer				:: yr, mnth, dy, hr
    real			    :: pcp, tma, tmn 

    ! --- code ------------------------------------------------------------------
    found_start = 0
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
      
      ! advance to first record needed in simulation 
      do while(ios .ge. 0)
        ! forcing could have any format (not fixed width)
        read (UNIT=runinfo%forcing_fileunits(nh), FMT=*, IOSTAT=ierr) yr, mnth, dy, hr, pcp, tma, tmn

        if(yr .eq. runinfo%start_year .and. mnth .eq. runinfo%start_month .and. dy .eq. runinfo%start_day .and. hr .eq. runinfo%start_hour) then
          found_start = found_start + 1
          exit    ! break out of the loop
        end if
        
      end do
      
      ! backspace the file to the previous record
      backspace runinfo%forcing_fileunits(nh)

    end do  ! end loop over snow bands
    
    ! error out if start of all forcings files not found
    if (found_start /= runinfo%n_hrus) then
      print*, 'ERROR: found the starting date in only', found_start, ' out of', runinfo%n_hrus, ' forcing files.  Quitting.'
      stop
    endif

  END SUBROUTINE open_and_init_forcing_files
  
  ! ==== read multiple snowband forcings one timestep at a time
  !      assumes files are already opened and advanced to timestep of interest
  subroutine read_areal_forcing_vec(namelist, parameters, runinfo, forcing)
    !use nrtype
    use namelistModule 
    implicit none

    ! input and inout variables
    type (namelist_type),    intent(in)      :: namelist
    type (parameters_type),  intent(in)      :: parameters
    type (runinfo_type),     intent(inout)   :: runinfo
    type (forcing_type),     intent(inout)   :: forcing
  
    ! local variables
    integer				                     :: nh, ierr=0
    integer				                     :: yr, mnth, dy, hr
    real   				                     :: tmx, tmn
    character(len=10)                        :: forcing_datehr
  
    do nh=1, runinfo%n_hrus

      ! read one record from already open files and check success
      read (UNIT=runinfo%forcing_fileunits(nh), FMT=*, IOSTAT=ierr) yr, mnth, dy, hr, forcing%precip(nh), tmx, tmn
      if(ierr /= 0) then
        print*, 'ERROR:  failed to read forcing from file', trim(namelist%forcing_root) // trim(parameters%hru_id(nh))
        STOP
      end

      ! check forcing date against run date (in readable format)
      write(forcing_datehr ,'(I0.4,I0.2,I0.2,I0.2)') yr, mnth, dy, hr)
      if(forcing_datehr /= runinfo%curr_datehr) then
        print*, 'ERROR: forcing datehr',forcing_datehr, ' does not match curr_datehr of run', runinfo%curr_datehr
        STOP
      end if 

      ! update other forcing fields (derived)
      forcing%tair(nh)	   = (tmx + tmn)/2
      forcing%precip_scf(nh) = forcing%precip(nh) * parameters%scf(nh)
    
    end do  ! end loop across snowbands (hrus)

    return
  END subroutine read_areal_forcing_vec  

  ! ==== Open output files and write header
  SUBROUTINE open_and_init_output_files(namelist, runinfo, parameters)
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

    ! open the main basin-average output file and write header

    ! make filename to read
    filename = trim(namelist%output_root) // trim(parameters%main_id)	
        
    ! Open the output file
    open(runinfo%output_fileunits(1), file = trim(filename), form = 'formatted', action = 'write', status='replace', iostat = ierr)
    if (ierr /= 0) then
      write(*,'("Problem opening file ''", A, "''")') trim(filename)
      stop ":  ERROR EXIT"
    endif
      
    ! Write 1-line header
    write(runinfo%output_fileunits(1),'(A)') 'year mo dy hr tair precip precip*scf sneqv snowh snow raim '

    ! if user setting is to write out information for each snowband, open the individual files
    if (runinfo%output_hrus == 1) then
      do nh=1, runinfo%n_hrus

        ! make filename to read
        filename = trim(namelist%output_root) // trim(parameters%hru_id(nh))	

        ! Open the output files
        open(runinfo%output_fileunits(nh+1), file = trim(filename), form = 'formatted', action = 'write', status='replace', iostat = ierr)
        if (ierr /= 0) then
          write(*,'("Problem opening file ''", A, "''")') trim(filename)
          stop ":  ERROR EXIT"
        endif
      
        ! Write 1-line header
        write(runinfo%output_fileunits(nh+1),'(A)') 'year mo dy hr tair precip precip*scf sneqv snowh snow raim '
      end if
      
    end do   ! end loop over sub-units
    
  END SUBROUTINE  open_and_init_output_files
  
  

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% !   NEEDS UPDATING
  subroutine read_snow17_state(modelvar, namelist, parameters)
    !use nrtype
    !use def_namelists, only: snow_state_in_root
    implicit none
  
    ! input variables
    class(parameters_type), intent(in)        :: parameters
    class(namelist_type), intent(in)          :: namelist
  
    ! [in]/output variables  (Note:  types before were real(sp)
    class(modelvar_type), intent(inout)          :: modelvar
   
    ! local variables
    integer               :: hru
    integer	              :: ios=0
    character(len=480)    :: state_infile
    character(len=10)     :: file_state_date_str
    character(len=10)	  :: state_date_str  ! AWW string to match date in input states
    integer               :: state_year, state_month, state_day
    
    
    ! starting statefiles match format of statefile outputs (date then vars)
    !   state read routines look for state from one day before the start date of run
    call day_before_date(namelist%start_year,namelist%start_month,namelist%start_day,state_year,state_month,state_day)
    ! create string that will be matched in state file
    write(state_date_str,'(I0.4,I0.2,I0.2,I0.2)') state_year, state_month, state_day, hour(1)
    
    ! loop over hrus and read and store initial state values
    do hru=1,n_hrus
      print*, 'Reading initial model states for area', hru, 'out of', namelist%n_hrus, 'for watershed ', namelist%main_id
  
      ! make state filename
      state_infile = trim(namelist%snow_state_in_root) // trim(parameters%hru_id[hru])
      open(unit=95,FILE=trim(state_infile),FORM='formatted',status='old')
      print*, 'Reading snow state file: ', trim(state_infile)
  
      ! format for input is an unknown number of rows with 20 data columns (1 tprev, 19 for cs)
      !   the first column is the datestring
      do while(ios .ge. 0)
  
        ! read each row and check to see if the date matches the initial state date
        read(95,*,IOSTAT=ios) file_state_date_str, modelvar%tprev(:), modelvar%cs(hru,:)
  
        ! checks either for real date or special word identifying the state to use
        !   this functionality facilitates ESP forecast initialization
        if(file_state_date_str==state_date_str .or. file_state_date_str=='PseudoDate') then
          print *, '  -- found initial snow state on ', state_date_str
          close(unit=95)
          return
        end if
        
      end do  ! end loop through state file
      close(unit=95)
  
      ! if you reach here without returning, quit -- the initial state date was not found
      print*, 'ERROR:  snow init state not found in snow initial state file.  Looking for: ', state_date_str
      print*, '  -- last state read was: ', file_state_date_str
      print*, 'Stopping.  Check inputs!'
      stop
      
    end do   ! end loop over one or more hrus (eg, snowbands)
  
  end subroutine read_snow17_state
  
  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% !   NEEDS UPDATING !!
  subroutine write_snow17_state(year, month, day, hour, cs, tprev, sim_length, curr_hru_id)
    use nrtype
    use def_namelists, only: snow_state_out_root
    implicit none
  
    !input variables
    character(len = 20), intent(in) 	    :: curr_hru_id  ! HRU extension for state fname
    integer(I4B),dimension(:),intent(in)	:: year
    integer(I4B),dimension(:),intent(in)	:: month
    integer(I4B),dimension(:),intent(in)	:: day
    integer(I4B),dimension(:),intent(in)	:: hour
    real(sp),dimension(:,:),intent(in)	    :: cs	         ! carry over array
    real(sp),dimension(:),intent(in)	    :: tprev         ! carry over variable
    integer(I4B),intent(in)                 :: sim_length    ! length of simulation
  
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

  ! === write output for one timestep ===
  ! assumes that files have been opened and header already written
  SUBROUTINE write_output_vec(runinfo, parameters, forcing, modelvar)
    implicit none
    type (namelist_type),    intent(in)   :: namelist
    type (runinfo_type),     intent(in)   :: runinfo
    type (parameters_type),  intent(in)   :: parameters
    type (forcing_type),  intent(in)      :: forcing
    type (modelvar_type),  intent(in)     :: modelvar
    
    ! output 
    
    ! local variables
    character*256  :: filename
    integer        :: ierr   ! error code returned by open(iostat = ierr)
    integer        :: nh     ! loop counter
  
    ! ==== WRITE output for current area simulation ====
    ! Note:  write order should match header written by open_and_init_output_files()
    
    32 FORMAT(I4.4, 3(1x,I2.2),7(F10.3)

    ! if user setting is to write out information for each snowband, open the individual files
    if (runinfo%output_hrus == 1) then
      do nh=1, runinfo%n_hrus
    
        write(runinfo%output_fileunits(nh+1), 32) runinfo%curr_yr, runinfo%curr_mo, runinfo%curr_dy, runinfo%curr_hr, &
              forcing%tair(nh), forcing%precip(nh), forcing%precip(nh)*scf(nh), &
              modelvar%sneqv(nh)*1000., modelvar%snowh(nh), modelvar%snow(nh), modelvar%raim(nh)
      end do
    end if  ! IF case for writing HRU-specific output to file (not including states)

    ! ==== sum across snowbands with weighting for snowband area ====
    do nh=1, runinfo%n_hrus
      tair_comb              = tair_comb + tair(nh) * hru_area(nh)
      precip_comb            = precip_comb + precip(nh) * hru_area(nh)
      precip_scf_comb        = precip_scf_comb + precip(nh) * scf(nh) * hru_area(nh)
      modelvar%sneqv_comb    = modelvar%sneqv_comb + modelvar%sneqv(nh) * hru_area(nh) 
      modelvar%snowh_comb    = modelvar%snowh_comb + modelvar%snowh(nh) * hru_area(nh) 
      modelvar%snow_comb     = modelvar%snow_comb + modelvar%snowh(nh) * hru_area(nh) 
      modelvar%raim_comb     = modelvar%raim_comb + modelvar%raim(nh) * hru_area(nh)
    end if

    ! take average of weighted sum of HRU areas
    forcing%%tair_comb       = forcing%tair_comb / parameters%total_area
    forcing%precip_comb      = forcing%precip_comb / parameters%total_area
    forcing%precip_scf_comb  = forcing%precip_scf_comb / parameters%total_area
    modelvar%sneqv_comb      = modelvar%sneqv_comb / parameters%total_area
    modelvar%snowh_comb      = modelvar%snowh_comb / parameters%total_area
    modelvar%snow_comb       = modelvar%snow_comb / parameters%total_area
    modelvar%raim_comb       = modelvar%raim_comb / parameters%total_area

    ! -- write out combined file that is similar to each area file, but add flow variable in CFS units

    write(runinfo%output_fileunits(1), 32) runinfo%curr_yr, runinfo%curr_mo, runinfo%curr_dy, runinfo%curr_hr, &
          forcing%tair_comb, forcing%precip_comb, forcing%precip_scf_comb, &
          modelvar%sneqv_comb*1000., modelvar%snowh_comb, modelvar%snow_comb, modelvar%raim_comb

    return
  END SUBROUTINE write_output_vec

  ! === miscellaneous ===

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


  
end module ioModule
