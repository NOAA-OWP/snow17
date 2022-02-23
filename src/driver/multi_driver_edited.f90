! ====================================================================================
! Driver for the Snow17 model for multiple HRUs within a basin and writes zone and areal mean output
!
! Adapted by A. Wood (2022) from the NCAR Snow17SacUH codebase (https://github.com/NCAR/NWS_hydro_models) 
!   that was formulated from Office of Hydrology FORTRAN source files obtained circa 2012-2013
! ====================================================================================

program multi_driver

  ! specify modules needed
  use nrtype
  use def_namelists
  use constants, only:   sec_hour,sec_day
  use interfaces, only:  read_namelist, sfc_pressure, read_areal_forcing, &
			             julian_day,write_snow17_state, read_snow17_state, &
			             date_diff_ndays, day_before_date
  implicit none

  ! local variables
  character(len=2000)	:: arg  !command line arg for namelist file
  character(len=2000)	:: namelist_name  !command line arg for namelist file
  character(len=2000)	:: combined_output_filename  ! AWW constructed on the fly
  character(len=2000)	:: hru_output_filename  ! AWW constructed on the fly
  character(len=10)	    :: state_date_str  ! AWW string to match date in input states

  integer(I4B) :: dt           ! model timestep (seconds)
                               ! moved here from namelist until model can be validated at 
                               ! other than a daily timestep (86400 s)
  integer      :: nh           ! AWW index for looping through areas
  real(dp)     :: total_area   ! (sqkm) AWW needed for combining outputs
  integer(I4B) :: i, k, m      ! tau
  integer(I4B) :: sim_length   ! length of simulation (days)
  real(sp)     :: tair_sp
  real(sp)	   :: precip_sp
  real(dp)     :: pa       ! snow17 surface pressure

  ! snow17 carry over variables
  real(sp)                :: tprev    ! carry over variable
  real(sp),dimension(19)  :: cs       ! carry over variable array
  real(sp),dimension(:),allocatable    :: tprev_states    ! for state file write
  real(sp),dimension(:,:),allocatable  :: cs_states       ! for state file write

  ! ==== ALLOCATABLE VARIABLES ====

  ! snow-17 output variables  single precision
  real(sp), dimension(:),allocatable   :: snowh, sneqv, snow !output variables for snow-17

  ! date variables
  integer, dimension(:),allocatable :: year, month, day, hour
  integer(I4B)                      :: state_year, state_month, state_day, state_hr

  ! precip/snowmelt inputs to Sac from Snow17
  real(sp), dimension(:),allocatable :: raim

  ! atmospheric forcing variables
  real(dp), dimension(:),allocatable :: tair, precip

  ! derived forcing variables
  real(dp), dimension(:),allocatable :: tair

  ! various other combined variables (aggregating multiple basins zones)
  real(sp), dimension(:),allocatable :: sneqv_comb ! AWW ditto, combined vars
  real(sp), dimension(:),allocatable :: raim_comb  ! AWW combined vars
  real(dp), dimension(:),allocatable :: precip_comb, precip_scf_comb ! AWW combined vars
  real(dp), dimension(:),allocatable :: tair_comb  ! AWW combined vars

  ! =======  CODE starts below =====================================================================

  ! get control file filename as argument
  i = 0
  do
    call get_command_argument(i, arg)
    if(i .eq. 1) namelist_name=arg   ! first argument has i=1
    if(LEN_TRIM(arg) == 0) EXIT
    i = i + 1
  end do

  ! set model timestep (DO NOT CHANGE in this daily modeling code version )
  dt = 86400 ! (s) model timestep (86400 seconds = 1 day)

  ! read namelist file to get info on the current simulation areas
  call read_namelist(namelist_name)

  ! read parameter files (contain params for all huc areas)
  call read_snow17_params(snow17_param_file, n_hrus)

  ! ========================= HRU AREA LOOP ========================================================
  !   loop through the simulation areas, running the lump model code and averaging the output

  print*, '--------------------------------------------------------------'

  do nh=1,n_hrus
    print*, 'Running area',nh,'out of',n_hrus,'for watershed ', main_id

    ! --- allocate variables (before simulating first area) ---
    if(nh .eq. 1) then
      ! calculate sim length to use in allocating variables (AWW new routine)
      call date_diff_ndays(start_year,start_month,start_day,end_year,end_month,end_day,sim_length)

      !forcing variables
      allocate(year(sim_length))
      allocate(month(sim_length))
      allocate(day(sim_length))
      allocate(hour(sim_length))
      allocate(precip(sim_length))
      allocate(tair(sim_length))
  
      ! snow17 output variables
      allocate(snowh(sim_length))
      allocate(sneqv(sim_length))
      allocate(snow(sim_length))
      allocate(raim(sim_length))
      
      ! snow17 state variables
      allocate(cs_states(19,sim_length)) ! new for state write
      allocate(tprev_states(sim_length)) ! ditto

    end if  ! end of IF case for allocating only when running the first simulation area

    ! read forcing data
    call read_areal_forcing(year, month, day, hour, tair, precip, hru_id(nh)) ! hour not used

    ! ================== RUN models for huc_area! ==========================================
  
    ! get sfc_pressure (pa is estimate by subroutine, needed by snow17 call)
    call sfc_pressure(elev(nh), pa)
  
    ! set single precision sac state variables to initial values
    if(warm_start_run .eq. 0) then
      ! we are not warm starting from a state file
      cs(1:19) = 0          ! init. SWE (model 'WE') cs(1) and rest of model states to zero
      tprev    = 0          ! temperature at end of previous timestep / start of run

    else if(warm_start_run .gt. 0) then
      ! we *ARE* warm starting from a state file
      ! read in external state files and overwrites namelist state variables 

      ! starting state files must match format of state file outputs (date then vars)
      !   state read routines look for state from one day before the start date of run
      call day_before_date(start_year,start_month,start_day,state_year,state_month,state_day)
      ! create string that will be matched in state file
      write(state_date_str,'(I0.4,I0.2,I0.2,I0.2)') state_year,state_month,state_day,hour(1)
      call read_snow17_state(state_date_str, cs, tprev, hru_id(nh))

    endif
  
    ! =============== START SIMULATION TIME LOOP =====================================
    do i = 1,sim_length,1
  
      !set single precision inputs
      tair_sp   = real(tair(i),kind(sp))
      precip_sp = real(precip(i),kind(sp))
      pet_sp    = real(pet(i),kind(sp))
   
      call exsnow19(int(dt),int(dt/sec_hour),day(i),month(i),year(i),&
    	!SNOW17 INPUT AND OUTPUT VARIABLES
  	  precip_sp,tair_sp,raim(i),sneqv(i),snow(i),snowh(i),&
    	  !SNOW17 PARAMETERS
          !ALAT,SCF,MFMAX,MFMIN,UADJ,SI,NMF,TIPM,MBASE,PXTEMP,PLWHC,DAYGM,ELEV,PA,ADC
  	  real(latitude(nh),kind(sp)),scf(nh),mfmax(nh),mfmin(nh),uadj(nh),si(nh),nmf(nh),&
              tipm(nh),mbase(nh),pxtemp(nh),plwhc(nh),daygm(nh),&
              real(elev(nh),kind(sp)),real(pa,kind(sp)),adc,&
              !SNOW17 CARRYOVER VARIABLES
  			  cs,tprev) 
  
      ! store other single timestep run outputs for possible state write
      if(write_states > 0) then
        cs_states(:,i)  = cs
        tprev_states(i) = tprev
        ! print*,'tprev: ',i,day(i),month(i),year(i),tprev,cs(1)  AWW debugging
      end if  

    end do  
    ! ============ end simulation time loop ====================
  
    ! ==== WRITE output for current area simulation ====

    if (output_hrus == 1) then

      hru_output_filename = trim(output_root) // trim(hru_id(nh))

      open(unit=45,FILE=trim(hru_output_filename),FORM='formatted',status='replace')  ! output file open
  
      ! print header first
      write(45,'(A)') 'year mo dy hr tair pcp pcp*scf swe raim '
  
      31 FORMAT(I4.4, 3(1x,I2.2),5(F8.2))  ! AWW: goes with update

      do i = 1,sim_length
        write(45,31) year(i),month(i),day(i),hour(i),tair(i),precip(i),precip(i)*scf(nh),&
                     sneqv(i)*1000.,raim(i)
      end do
      close(unit=45)
    end if  ! IF case for writing HRU-specific output to file (not including states)

    ! === write out STATE FILES for snow17 ===
    if(write_states > 0) then
      call write_snow17_state(year,month,day,hour,cs_states,tprev_states,sim_length,hru_id(nh))
    end if

    ! ==== store/add single simulation area timeseries ====
    if(nh == 1) then

      ! first area:  allocate combination output variables before first use (if any)
      allocate(tair_comb(sim_length))
      allocate(precip_comb(sim_length))
      allocate(precip_scf_comb(sim_length))
      allocate(sneqv_comb(sim_length))
      allocate(raim_comb(sim_length))
      allocate(pet_comb(sim_length))

      ! store current area variable multiplied by area (in sqkm)
      tair_comb       = tair * hru_area(nh)   
      precip_comb     = precip * hru_area(nh)
      precip_scf_comb = precip * scf(nh) * hru_area(nh)
      sneqv_comb      = sneqv * hru_area(nh) 
      raim_comb       = raim * hru_area(nh)
      pet_comb        = pet * hru_area(nh)
      total_area      = hru_area(nh)  ! initialize on first area

    else       ! not first area, so add to summary variables already started

      !    AWW 'hru_area' from 'use def_namelists' statement (in sqkm)
      tair_comb       = tair_comb + tair * hru_area(nh)
      precip_comb     = precip_comb + precip * hru_area(nh)
      precip_scf_comb = precip_scf_comb + precip * scf(nh) * hru_area(nh)
      sneqv_comb      = sneqv_comb + sneqv * hru_area(nh) 
      raim_comb       = raim_comb + raim * hru_area(nh)
      pet_comb        = pet_comb + pet * hru_area(nh)
      total_area      = total_area + hru_area(nh)
    end if

  end do   ! ========== END of simulation areas loop ====================

  ! ====== print combined simulation output ============
  print*, ' '
  print*, '            Start: ', year(1), month(1), day(1)
  print*, '              End: ', year(sim_length), month(sim_length), day(sim_length) 
  print*, 'Sim_length (days): ', sim_length
  print*, '--------------------------------------------------------------'

  combined_output_filename = trim(output_root) // trim(main_id)
  print*, 'Combining outputs and writing ', trim(combined_output_filename)

  ! take weighted average of sum of HRU areas
  tair_comb       = tair_comb / total_area
  precip_comb     = precip_comb / total_area
  precip_scf_comb = precip_scf_comb / total_area
  sneqv_comb      = sneqv_comb / total_area
  raim_comb       = raim_comb / total_area
  pet_comb        = pet_comb / total_area

  ! -- write out combined file that is similar to each area file, but add flow variable in CFS units

  ! combined output file open
  open(unit=125, FILE=trim(combined_output_filename),FORM='formatted',status='replace')
  
  ! print header & data (diffence if there's routing or not)
  33 FORMAT(I4.4, 3(1x,I2.2),3(F8.2),F9.2,F8.2,2(F8.2),6(F9.2),2(F10.3),F12.2)  
  
  write(125,'(A)') 'year mo dy hr tair pcp pcp*scf pet swe raim'
  do i = 1,sim_length
    write(125, 33) year(i), month(i), day(i), hour(i), tair_comb(i), precip_comb(i), precip_scf_comb(i),&
                     pet_comb(i), sneqv_comb(i)*1000., raim_comb(i) 
  end do
  close(unit=125)

  print*, 'DONE!'; print*, '--------------------------------------------------------------'

end program
