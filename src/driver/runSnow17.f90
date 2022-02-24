! module for executing Snow17 model

module RunModule
  
  use namelistModule
  use ioModule
  use utilitiesModule
  use forcingModule
  use statesModule
  use dateTimeUtilsModule
  
  !use interfaces, only: sfc_pressure
  !use constants, only: sfc_pres_a,sfc_pres_b,sfc_pres_c,sfc_pres_d,sfc_pres_e
  
  implicit none

  type :: snow17_type
    type(namelist_type)   :: namelist
    type(runinfo_type)    :: runinfo
    type(parameters_type) :: parameters
    type(forcing_type)    :: forcing
    type(states_type)     :: states
  end type snow17_type

contains

  !== Initialize the model ================================================================================

  SUBROUTINE initialize_from_file (model, config_file)
    implicit none
    
    type(snow17_type), target, intent(out) :: model
    character(len=*), intent (in) :: config_file ! namelist file from command line argument
    
    ! local vars
    integer             :: forcing_timestep  ! integer time step (set to dt) for some subroutine calls
    
    associate(namelist   => model%namelist,   &
              runinfo    => model%runinfo,    &
              parameters => model%parameters, &
              forcing    => model%forcing,    &
              states     => model%states)
              
      !---------------------------------------------------------------------
      !  initialize
      !---------------------------------------------------------------------
      call namelist%readNamelist(config_file)

      call runinfo%Init(namelist)           ! initialize run space-time info
      call parameters%Init(namelist)        ! read and/or initialize parameters
      call forcing%Init(namelist)           ! initialize forcing data type/structure
      call states%Init(namelist)            ! initialize model states (incl. restarts)
      !call output%Init(namelist)            ! initialize output data type/structure
         
      !---------------------------------------------------------------------
      ! Open the forcing file
      ! Comp. dir. NGEN_FORCING_ACTIVE indicates Nextgen forcing is used
      !---------------------------------------------------------------------
#ifndef NGEN_FORCING_ACTIVE
      !call open_forcing_files(namelist%input_filename)
      call open_init_forcing_files(namelist, runinfo, parameters)
#endif
      
      !---------------------------------------------------------------------
      ! Create output file and add initial values
      ! Compiler directive NGEN_OUTPUT_ACTIVE indicates Nextgen controls outputs
      !---------------------------------------------------------------------
#ifndef NGEN_OUTPUT_ACTIVE
      !call initialize_output(namelist%output_filename, runinfo%ntime, levels%nsoil, levels%nsnow)
      call open_init_output_files(namelist, runinfo, parameters)
#endif
      
    end associate ! terminate the associate block

  END SUBROUTINE initialize_from_file                
              
             
             
  ! == Move the model ahead one time step ================================================================

  SUBROUTINE advance_in_time(model)
    type (snow17_type), intent (inout) :: model

    call solve_snow17(model)

    model%runinfo%itime    = model%runinfo%itime + 1 ! increment the integer time by 1
    model%runinfo%time_dbl = dble(model%runinfo%time_dbl + model%runinfo%dt) ! increment model time in seconds by DT
  END SUBROUTINE advance_in_time
  


  ! == Run the model for one timestep and all spatial sub-units ==============================================
  SUBROUTINE solve_snow17(model)
    type (noahowp_type), intent (inout) :: model
    integer, parameter :: iunit        = 10 ! Fortran unit number to attach to the opened file
    integer            :: forcing_timestep  ! integer time step (set to dt) for some subroutine calls
    integer            :: ierr              ! error code for reading forcing data
    integer            :: curr_yr, curr_mo, curr_dy, curr_hr, curr_min, curr_sec  ! current UNIX timestep details

    associate(namelist   => model%namelist,   &
              runinfo    => model%runinfo,    &
              parameters => model%parameters, &
              forcing    => model%forcing,    &
              states     => model%states)
    
      ! Compute the current UNIX datetime
      runinfo%curr_datetime = runinfo%sim_datetimes(runinfo%itime)     ! use end-of-timestep datetimes

      call unix_to_date (runinfo%curr_datetime, curr_yr, curr_mo, curr_dy, curr_hr, curr_min, curr_sec)
      ! print '(2x,I4,1x,I2,1x,I2,1x,I2,1x,I2)', curr_yr, curr_mo, curr_dy, curr_hr, curr_min ! time check for debugging
    
      forcing_timestep = runinfo%dt
  
      ! --- loop over spatial sub-units ---
      do nh=1, runinfo%n_hrus
        print*, 'Running area',nh,'out of',n_hrus,'for watershed ', main_id

        !---------------------------------------------------------------------
        ! Read in the forcing data if NGEN_FORCING_ACTIVE is not defined
        !---------------------------------------------------------------------
#ifndef NGEN_FORCING_ACTIVE
        call read_forcing_ascii(iunit(nh), runinfo%nowdate, forcing_timestep, forcing%precip, forcing%tair, ierr)
#endif

        !---------------------------------------------------------------------
        ! call the main snow17 state update routine
        !---------------------------------------------------------------------
        !call snow_states_update (runinfo, parameters, forcing, states)
        !set single precision inputs
        tair_sp   = real(forcing%tair,  kind(sp))
        precip_sp = real(forcing%precip,kind(sp))
        
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

        !---------------------------------------------------------------------
        ! add results to output file if NGEN_OUTPUT_ACTIVE is undefined
        !---------------------------------------------------------------------
#ifndef NGEN_OUTPUT_ACTIVE
        call add_to_output(runinfo, parameters, forcing, states, runinfo%itime)
#endif
    
      end do  ! end of spatial sub-unit loop

    end associate ! terminate associate block
    
  END SUBROUTINE solve_snow17
  
  
  !== Finalize the model ================================================================================
  SUBROUTINE cleanup(model)
    implicit none
    type(snow17_type), intent(inout) :: model
      
      !---------------------------------------------------------------------
      ! Compiler directive NGEN_OUTPUT_ACTIVE to be defined if 
      ! Nextgen is writing model output (https://github.com/NOAA-OWP/ngen)
      !---------------------------------------------------------------------
#ifndef NGEN_OUTPUT_ACTIVE
      call finalize_output()
#endif
  
  END SUBROUTINE cleanup


  

end module RunModule              
