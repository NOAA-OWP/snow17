! module for executing Snow17 model

module RunModule
  
  use namelistModule
  use ioModule
  use utilitiesModule
  use dateTimeUtilsModule
  use parametersModule
  
  !use interfaces, only: sfc_pressure
  !use constants, only: sfc_pres_a,sfc_pres_b,sfc_pres_c,sfc_pres_d,sfc_pres_e
  
  implicit none

  type :: snow17_type
    type(namelist_type)   :: namelist
    type(runinfo_type)    :: runinfo
    type(parameters_type) :: parameters
    type(forcing_type)    :: forcing
    type(modelvar_type)   :: modelvar
  end type snow17_type

contains

  !== Initialize the model ================================================================================

  SUBROUTINE initialize_from_file (model, config_file)
    implicit none
    
    type(snow17_type), target, intent(out) :: model
    character(len=*), intent (in) :: config_file ! namelist file from command line argument
    
    associate(namelist   => model%namelist,   &
              runinfo    => model%runinfo,    &
              parameters => model%parameters, &
              forcing    => model%forcing,    &
              modelvar   => model%modelvar)
              
      !-----------------------------------------------------------------------------------------
      !  read namelist, initialize data structures
      !-----------------------------------------------------------------------------------------
      call namelist%readNamelist(config_file)

      call runinfo%Init(namelist)           ! initialize run space-time info
      call parameters%Init(namelist)        ! read and/or initialize parameters
      call forcing%Init(namelist)           ! initialize forcing data type/structure
      call modelvar%Init(namelist)          ! initialize model states (incl. restarts)
      call output%Init(namelist)            ! initialize output data type/structure
      
      ! read parameters from input file
      call parameters%read_snow17_parameters(this, namelist%snow17_param_file)

      ! initialize model states (and read state files if any)
      call modelvar%assignStates(this, namelist%snow17_param_file)
         
      !---------------------------------------------------------------------
      ! Open the forcing file
      ! Comp. dir. NGEN_FORCING_ACTIVE indicates Nextgen forcing is used
      !---------------------------------------------------------------------
#ifndef NGEN_FORCING_ACTIVE
      !call open_forcing_files(namelist%input_filename)
      call open_and_init_forcing_files(namelist, runinfo, parameters)
#endif
      
      !---------------------------------------------------------------------
      ! Create output file and add initial values
      ! Compiler directive NGEN_OUTPUT_ACTIVE indicates Nextgen controls outputs
      !---------------------------------------------------------------------
#ifndef NGEN_OUTPUT_ACTIVE
      call open_and_init_output_files(namelist, runinfo, parameters)
#endif
      
    end associate ! terminate the associate block

  END SUBROUTINE initialize_from_file                
              
             
             
  ! == Move the model ahead one time step ================================================================

  SUBROUTINE advance_in_time(model)
    type (snow17_type), intent (inout) :: model

    ! -- run snow17 for one time step
    call solve_snow17(model)

    ! -- advance run time info
    model%runinfo%itime    = model%runinfo%itime + 1 ! increment the integer time by 1
    !model%runinfo%time_dbl = dble(model%runinfo%time_dbl + model%runinfo%dt)        ! increment relative model run time in seconds by DT
    model%runinfo%curr_datetime = model%runinfo%curr_datetime + model%runinfo%dt     ! increment unix model run time in seconds by DT
    call unix_to_datehr (runinfo%curr_datetime, runinfo%curr_datehr)       ! update datehr field as well
    call unix_to_date_elem (runinfo%curr_datetime, runinfo%curr_yr, runinfo%curr_mo, runinfo%curr_dy, &
                            runinfo%curr_hr, runinfo%curr_min, runinfo%curr_sec)
    
  END SUBROUTINE advance_in_time
  

  ! == Run the model for one timestep and all spatial sub-units ==============================================
  SUBROUTINE solve_snow17(model)
    type (snow17_type), intent (inout) :: model
    
    ! local parameters
    integer            :: nh             ! counter for snowbands

    associate(namelist   => model%namelist,   &
              runinfo    => model%runinfo,    &
              parameters => model%parameters, &
              forcing    => model%forcing,    &
              modelvar   => model%modelvar)
    
      !---------------------------------------------------------------------
      ! Read in the forcing data if NGEN_FORCING_ACTIVE is not defined
      !   will read current timestep forcing for all snowbands
      !---------------------------------------------------------------------
#ifndef NGEN_FORCING_ACTIVE
      call read_areal_forcing_vec(namelist, parameters, runinfo, forcing)
#endif

      !---------------------------------------------------------------------
      ! call the main snow17 state update routine in loop over spatial sub-units
      !---------------------------------------------------------------------
      do nh=1, runinfo%n_hrus

        !call snow_states_update (runinfo, parameters, forcing, states)
        !set single precision inputs
        !tair_sp   = real(forcing%tair(nh),   kind(sp))
        !precip_sp = real(forcing%precip(nh), kind(sp))
        
        !call exsnow19(int(dt), int(dt/sec_hour), day(i), month(i), year(i),&
    	  !SNOW17 INPUT AND OUTPUT VARIABLES
  	    !  precip_sp,tair_sp,raim(i),sneqv(i),snow(i),snowh(i),&
    	  !SNOW17 PARAMETERS
          !ALAT,SCF,MFMAX,MFMIN,UADJ,SI,NMF,TIPM,MBASE,PXTEMP,PLWHC,DAYGM,ELEV,PA,ADC
  	    !  real(latitude(nh),kind(sp)),scf(nh),mfmax(nh),mfmin(nh),uadj(nh),si(nh),nmf(nh),&
        !  tipm(nh),mbase(nh),pxtemp(nh),plwhc(nh),daygm(nh),&
        !  real(elev(nh),kind(sp)),real(pa,kind(sp)),adc,&
          !SNOW17 CARRYOVER VARIABLES
  		!  cs, tprev)
        call exsnow19(int(dt), int(dt/3600), runinfo%curr_dy, runinfo%curr_mo, runinfo%curr_yr, &
    	  ! SNOW17 INPUT AND OUTPUT VARIABLES
  	      forcing%precip(nh), forcing%tair(nh), modelvar%raim(nh), modelvar%sneqv(nh), modelvar%snow(nh), modelvar%snowh(nh), &
    	  ! SNOW17 PARAMETERS
          !ALAT,SCF,MFMAX,MFMIN,UADJ,SI,NMF,TIPM,MBASE,PXTEMP,PLWHC,DAYGM,ELEV,PA,ADC
  	      parameters%latitude(nh), parameters%scf(nh), parameters%mfmax(nh), parameters%mfmin(nh), &
          parameters%uadj(nh), parameters%si(nh), parameters%nmf(nh), parameters%tipm(nh), parameters%mbase(nh), &
          parameters%pxtemp(nh), parameters%plwhc(nh), parameters%daygm(nh), parameters%elev(nh), parameters%pa(nh), &
          parameters%adc(nh), &
          ! SNOW17 CARRYOVER VARIABLES
  		  modelvar%cs(nh), modelvar%tprev(nh) )             

        !---------------------------------------------------------------------
        ! add results to output file if NGEN_OUTPUT_ACTIVE is undefined
        !---------------------------------------------------------------------
#ifndef NGEN_OUTPUT_ACTIVE
        call write_output_vec(runinfo, parameters, forcing, modelvar)
        
        ! === write out STATE FILES for snow17 if needed ===
        if(namelist%write_states > 0) then
          call write_snow17_state(runinfo, namelist, modelvar)          ! SUBR. STILL NEEDS REFACTORING
        end if
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
      !call finalize_output()      ! short enough that another sub not needed
      
      ! -- close all files
      do nh=1, runinfo%n_hrus
        close(runinfo%forcing_fileunits(nh)
        close(runinfo%output_fileunits(nh) 
      end do
      close(runinfo%output_fileunits(nh+1)        

      ! -- print final stdout messages for user incl. location of output & period of run
      print*, 'Done'
      !TBA (see orig driver code)
#endif
  
  END SUBROUTINE cleanup

end module RunModule              
