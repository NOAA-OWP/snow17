! module for executing Snow17 model
module runModule
  
  use namelistModule
  use ioModule
  use dateTimeUtilsModule
  use parametersType
  use runInfoType
  use forcingType
  use modelVarType
  use snow_log_module
  use messagepack
  use iso_fortran_env

  implicit none

  type, public :: snow17_type
    type(namelist_type)   :: namelist
    type(runinfo_type)    :: runinfo
    type(parameters_type) :: parameters
    type(forcing_type)    :: forcing
    type(modelvar_type)   :: modelvar
    byte, dimension(:), allocatable :: serialization_buffer
  end type snow17_type

contains

  !== Initialize the model ================================================================================

  SUBROUTINE initialize_from_file (model, config_file)
    implicit none
    
    type(snow17_type), target, intent(out) :: model
    character(len=*), intent (in)          :: config_file ! namelist file from command line argument
    
    associate(namelist   => model%namelist,   &
              runinfo    => model%runinfo,    &
              parameters => model%parameters, &
              forcing    => model%forcing,    &
              modelvar   => model%modelvar)
              
      !-----------------------------------------------------------------------------------------
      !  read namelist, initialize data structures and read parameters
      !-----------------------------------------------------------------------------------------
      call namelist%readNamelist(config_file)

      call runinfo%initInfo(namelist)          ! initialize run space-time info
      call forcing%initForcing(namelist)       ! initialize forcing data type/structure
      call modelvar%initModelVar(namelist)     ! initialize model states (incl. restarts)
      call parameters%initParams(namelist)     ! read and/or initialize parameters
      
      ! read parameters from input file
      call read_snow17_parameters(parameters, namelist%snow17_param_file, runinfo)
        
      !---------------------------------------------------------------------
      ! Open the forcing file
      ! Comp. dir. NGEN_FORCING_ACTIVE indicates Nextgen forcing is used
      !---------------------------------------------------------------------
#ifndef NGEN_FORCING_ACTIVE
      call init_forcing_files(namelist, runinfo, parameters)
#endif
      
      !---------------------------------------------------------------------
      ! If warm start is specified, read an initial state from a restart file
      ! Comp. dir. NGEN_READ_RESTART_ACTIVE indicates Nextgen sets the states
      !---------------------------------------------------------------------
#ifndef NGEN_READ_RESTART_ACTIVE
      ! we *ARE* warm-starting from a state file
      ! read in external state files and overwrites namelist state variables
      if(namelist%warm_start_run .eq. 1) then
        call read_snow17_statefiles (modelvar, namelist, parameters, runinfo) 
      endif
#endif

      !---------------------------------------------------------------------
      ! Create output file and write header
      ! Compiler directive NGEN_OUTPUT_ACTIVE indicates Nextgen controls outputs
      !---------------------------------------------------------------------
#ifndef NGEN_OUTPUT_ACTIVE
      call init_output_files(namelist, runinfo, parameters)
#endif

      !---------------------------------------------------------------------
      ! If a run to write initial states is specified, open/init state files
      ! Comp. dir. NGEN_WRITE_RESTART_ACTIVE indicates Nextgen will do it
      !---------------------------------------------------------------------
#ifndef NGEN_WRITE_RESTART_ACTIVE
      ! -- If namelist write_states == 1, open state files and write header
      if(namelist%write_states .eq. 1) then
        call init_new_state_files(namelist, runinfo, parameters)
      endif
#endif

    end associate ! terminate the associate block

  END SUBROUTINE initialize_from_file                
              
             
  ! == Move the model ahead one time step ================================================================
  SUBROUTINE advance_in_time(model)
    type (snow17_type), intent (inout) :: model
    
    !print*, 'current time: ', model%runinfo%curr_datehr

    ! -- run snow17 for one time step
    call solve_snow17(model)

    ! -- advance run time info
    model%runinfo%itime         = model%runinfo%itime + 1                            ! increment the integer time by 1
    !model%runinfo%time_dbl     = dble(model%runinfo%time_dbl + model%runinfo%dt)    ! increment relative model run time in seconds by DT
    model%runinfo%curr_datetime = model%runinfo%curr_datetime + model%runinfo%dt     ! increment unix model run time in seconds by DT
    call unix_to_datehr (model%runinfo%curr_datetime, model%runinfo%curr_datehr)     ! update datehr field as well
    call unix_to_date_elem (model%runinfo%curr_datetime, model%runinfo%curr_yr, model%runinfo%curr_mo, model%runinfo%curr_dy, &
                            model%runinfo%curr_hr, model%runinfo%curr_min, model%runinfo%curr_sec)
    
  END SUBROUTINE advance_in_time
  

  ! == Routing to run the model for one timestep and all spatial sub-units ================================
  SUBROUTINE solve_snow17(model)
    type (snow17_type), intent (inout) :: model
    
    ! local parameters
    integer            :: nh             ! counter for snowbands
    real               :: prcp_mm        ! precip as a depth (for input to snow17) (mm)

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
      call read_areal_forcing(namelist, parameters, runinfo, forcing)
#endif

      !---------------------------------------------------------------------
      ! initialize basin-average variables
      !---------------------------------------------------------------------
      forcing%tair_comb       = 0.0
      forcing%precip_comb     = 0.0
      forcing%precip_scf_comb = 0.0
      modelvar%sneqv_comb     = 0.0
      modelvar%snowh_comb     = 0.0
      modelvar%raim_comb      = 0.0

      !---------------------------------------------------------------------
      ! call the main snow17 state update routine in loop over spatial sub-units
      !---------------------------------------------------------------------
      do nh=1, runinfo%n_hrus

        prcp_mm = forcing%precip(nh)*runinfo%dt   ! convert prcp input to a depth per timestep (mm)

        call sfc_pressure(parameters%elev(nh), forcing%pa(nh))  ! fill in the surface pressure field (in mb)
                                                                ! uses elev. based constant PA (Anderson, 2006)
                                                                ! (alternative: input PA as a dynamic forcing)

        call exsnow19(int(runinfo%dt), int(runinfo%dt/3600), runinfo%curr_dy, runinfo%curr_mo, runinfo%curr_yr, &
    	  ! SNOW17 INPUT AND OUTPUT VARIABLES
          prcp_mm, forcing%tair(nh), modelvar%raim(nh), modelvar%sneqv(nh), modelvar%snow(nh), modelvar%snowh(nh), &
    	  ! SNOW17 PARAMETERS
          !ALAT,SCF,MFMAX,MFMIN,UADJ,SI,NMF,TIPM,MBASE,PXTEMP,PLWHC,DAYGM,ELEV,PA,ADC
  	      parameters%latitude(nh), parameters%scf(nh), parameters%mfmax(nh), parameters%mfmin(nh), &
          parameters%uadj(nh), parameters%si(nh), parameters%nmf(nh), parameters%tipm(nh), parameters%mbase(nh), &
          parameters%pxtemp(nh), parameters%plwhc(nh), parameters%daygm(nh), parameters%elev(nh), forcing%pa(nh), &
          parameters%adc(:,nh), &
          ! SNOW17 CARRYOVER VARIABLES
  		  modelvar%cs(:,nh), modelvar%tprev(nh) )

        ! convert raim output to a rate (mm/s)
        modelvar%raim(nh) = modelvar%raim(nh) / runinfo%dt

        ! scale precip by correction factor (nb: for output; model input precip is scaled in pack19()
        forcing%precip_scf(nh) = forcing%precip(nh) * parameters%scf(nh) 
        
        ! update basin-averaged variables
        forcing%tair_comb        = forcing%tair_comb + forcing%tair(nh) * parameters%hru_area(nh)
        forcing%precip_comb      = forcing%precip_comb + forcing%precip(nh) * parameters%hru_area(nh)
        forcing%precip_scf_comb  = forcing%precip_scf_comb + forcing%precip_scf(nh) * parameters%hru_area(nh)
        modelvar%sneqv_comb      = modelvar%sneqv_comb + modelvar%sneqv(nh) * parameters%hru_area(nh) 
        modelvar%snowh_comb      = modelvar%snowh_comb + modelvar%snowh(nh) * parameters%hru_area(nh) 
        modelvar%raim_comb       = modelvar%raim_comb + modelvar%raim(nh) * parameters%hru_area(nh)

        ! ==== if all snowbands have been run, sum across snowbands with weighting for snowband area ====
        if (nh .eq. runinfo%n_hrus) then 
          forcing%tair_comb        = forcing%tair_comb / parameters%total_area
          forcing%precip_comb      = forcing%precip_comb / parameters%total_area
          forcing%precip_scf_comb  = forcing%precip_scf_comb / parameters%total_area
          modelvar%sneqv_comb      = modelvar%sneqv_comb / parameters%total_area
          modelvar%snowh_comb      = modelvar%snowh_comb / parameters%total_area
          modelvar%raim_comb       = modelvar%raim_comb / parameters%total_area
        end if

        !---------------------------------------------------------------------
        ! add results to output file if NGEN_OUTPUT_ACTIVE is undefined
        !---------------------------------------------------------------------
#ifndef NGEN_OUTPUT_ACTIVE
        call write_snow17_output(namelist, runinfo, parameters, forcing, modelvar, nh)
#endif
        
        ! === write out end-of-timestep values to STATE FILES for snow17 if requested in namelist ===
#ifndef NGEN_WRITE_RESTART_ACTIVE
        if(namelist%write_states .eq. 1) then
          call write_snow17_statefile(runinfo, namelist, modelvar, nh)  
        end if
#endif        
    
      end do  ! end of spatial sub-unit (snowband) loop

    end associate ! terminate associate block
    
  END SUBROUTINE solve_snow17
  
  
  !== Finalize the model ================================================================================
  SUBROUTINE cleanup(model)
    implicit none
    type(snow17_type), intent(inout) :: model
    
    ! local variables
    integer         :: nh
      
    !---------------------------------------------------------------------
    ! Compiler directive NGEN_OUTPUT_ACTIVE to be defined if 
    ! Nextgen is writing model output (https://github.com/NOAA-OWP/ngen)
    !---------------------------------------------------------------------
#ifndef NGEN_OUTPUT_ACTIVE
    !call finalize_output()      ! short enough that another sub not needed unless to print some end of run info
      
    ! -- close all forcing and output files
    do nh=1, model%runinfo%n_hrus
      close(model%runinfo%forcing_fileunits(nh))
      close(model%runinfo%output_fileunits(nh))
    end do
    !
    !
    ! Now 'nh' value is (runinfo%n_hrus + 1)
    close(model%runinfo%output_fileunits(nh)) ! combined output file (basin avg)

#endif

#ifndef NGEN_WRITE_RESTART_ACTIVE
    ! -- close state files
    do nh=1, model%runinfo%n_hrus
      close(model%runinfo%state_fileunits(nh))
    end do
#endif
    !Free up serialization buffer memory
    if(allocated(model%serialization_buffer)) then
      deallocate(model%serialization_buffer)
    end if

  end subroutine cleanup

  SUBROUTINE new_serialization_request (model, exec_status)
    type(snow17_type), intent(inout) :: model
    integer(kind=int64) :: nh !counter for HRUs
    real, dimension(:), allocatable :: cs_per_hru
    class(msgpack), allocatable :: mp
    class(mp_arr_type), allocatable :: mp_sub_arr
    class(mp_arr_type), allocatable :: mp_state_arr
    class(mp_arr_type), allocatable :: mp_cs_arr
    byte, dimension(:), allocatable :: serialization_buffer
    integer(kind=int64), intent(out) :: exec_status

    mp = msgpack()
    mp_cs_arr = mp_arr_type(model%runinfo%n_hrus)
    do nh=1, model%runinfo%n_hrus
        cs_per_hru = model%modelvar%cs(:,nh)
        mp_sub_arr = mp_arr_type(19)
        mp_sub_arr = transfer_values_to_mp(cs_per_hru)
        mp_cs_arr%values(nh)%obj = mp_sub_arr
    end do

    !Add the time information and the state variables by HRU to the main mp array.
    mp_state_arr = mp_arr_type(6)
    mp_state_arr%values(1)%obj = mp_int_type(model%runinfo%curr_yr) !curr_yr
    mp_state_arr%values(2)%obj = mp_int_type(model%runinfo%curr_mo) !curr_mo
    mp_state_arr%values(3)%obj = mp_int_type(model%runinfo%curr_dy) !curr_dy
    mp_state_arr%values(4)%obj = mp_int_type(model%runinfo%curr_hr) !curr_hr
    mp_state_arr%values(5)%obj = transfer_values_to_mp(model%modelvar%tprev)
    mp_state_arr%values(6)%obj = mp_cs_arr
            
    ! pack the data
    call mp%pack_alloc(mp_state_arr, serialization_buffer)
    if (mp%failed()) then
        call write_log("Serialization using messagepack failed!. Error:" // mp%error_message, LOG_LEVEL_FATAL)
        exec_status = 1
    else
        exec_status = 0
        model%serialization_buffer = serialization_buffer
        call write_log("Serialization using messagepack successful!", LOG_LEVEL_DEBUG)
    end if
  END SUBROUTINE new_serialization_request

  SUBROUTINE deserialize_mp_buffer (model, serialized_data,exec_status)
    type(snow17_type), intent(inout) :: model
    integer , intent(in) :: serialized_data(:)
    integer(kind=int64), intent(out) :: exec_status
    byte, allocatable :: serialized_data_1b(:)
    class(msgpack), allocatable :: mp
    class(mp_value_type), allocatable :: mpv
    class(mp_arr_type), allocatable :: arr
    class(mp_arr_type), allocatable :: arr_tprev_hrus
    class(mp_arr_type), allocatable :: arr_cs_hrus
    class(mp_arr_type), allocatable :: arr_state
    integer(kind=int64) :: nh, yr, mo, dd, hr
    logical :: status, allow_logging
    
    exec_status = 0
    mp = msgpack()
    !convert integer(4) to integer(1) for messagepack
    allocate(serialized_data_1b(size(serialized_data, 1, int64)*4_int64))
    serialized_data_1b = transfer(serialized_data, serialized_data_1b) 
    call mp%unpack(serialized_data_1b, mpv)
    if (is_arr(mpv)) then
      call get_arr_ref(mpv, arr_state, status) 
      if (status) then
        !Update the start and current time for the runInfo.
        call get_int(arr_state%values(1)%obj, yr, status)
        model%runinfo%curr_yr = yr
        call get_int(arr_state%values(2)%obj, mo, status)
        model%runinfo%curr_mo = mo
        call get_int(arr_state%values(3)%obj, dd, status)
        model%runinfo%curr_dy = dd
        call get_int(arr_state%values(4)%obj, hr, status)
        model%runinfo%curr_hr = hr
        
        call get_arr_ref(arr_state%values(5)%obj,arr_tprev_hrus,status)
        if(status) then
          !The number of elements in the serialized HRU data array for tprev is expected to match the 
          !number of HRUs. Check here and log if they are not equal.
          if (arr_tprev_hrus%numelements() .NE. model%runinfo%n_hrus) then
            call write_log("The serialized data for model variable 'tprev' does not contain state information for all HRUs. Please check inputs", LOG_LEVEL_FATAL)
            exec_status = 1
          else
            model%modelvar%tprev = transfer_values_from_mp(arr_tprev_hrus)            
          end if
        else
          call write_log("Deserializing data for model variable 'tprev' failed!. Error:" // mp%error_message, LOG_LEVEL_FATAL)
          exec_status = 1
        end if
        
        call get_arr_ref(arr_state%values(6)%obj,arr_cs_hrus,status)
        if(status) then
          !The number of elements in the serialized HRU data array for cs is expected to match the 
          !number of HRUs. Check here and log if they are not equal.
          if (arr_cs_hrus%numelements() .NE. model%runinfo%n_hrus) then
            call write_log("The serialized data model variable 'cs' does not contain state information for all HRUs. Please check inputs", LOG_LEVEL_FATAL)
            exec_status = 1
          else
            allow_logging = .TRUE. ! using this variable to supress repeated error logs in HRU loop.
            do nh=1, model%runinfo%n_hrus
              call get_arr_ref(arr_cs_hrus%values(nh)%obj,arr,status)
              if (status) then
                model%modelvar%cs(:,nh) = transfer_values_from_mp(arr)
              else
                if(allow_logging) then
                  call write_log("Serialization using messagepack (HRU internal array) for variable 'cs' failed!. Error:" // mp%error_message, LOG_LEVEL_FATAL)
                  exec_status = 1
                  allow_logging = .FALSE.
                end if
              end if
            end do
          end if  
        else
          call write_log("Deserializing data for model variable 'cs' failed!. Error:" // mp%error_message, LOG_LEVEL_FATAL)
          exec_status = 1
        end if
      else
        call write_log("Getting an array reference to deserialized data failed! Error: " // mp%error_message, LOG_LEVEL_FATAL)  
        exec_status = 1
      end if
    else
      call write_log("Deserialized data structure is not a messagepack array. Error: " // mp%error_message, LOG_LEVEL_FATAL)  
      exec_status = 1
    end if
    deallocate (mpv)
    deallocate (serialized_data_1b)
  
  END SUBROUTINE deserialize_mp_buffer

  FUNCTION transfer_values_to_mp (src) RESULT (dest)

    real, allocatable, dimension(:), intent(in) :: src
    class(mp_arr_type), allocatable :: dest
    integer(kind=int64) :: index

        do index=LBOUND(src,1), UBOUND(src,1)
            dest%values(index)%obj = mp_float_type(src(index))
        end do

  END FUNCTION transfer_values_to_mp

  FUNCTION transfer_values_from_mp (src) RESULT (dest)

    class(mp_arr_type), allocatable, intent(in) :: src
    real, allocatable, dimension(:) :: dest
    real(kind=real64) :: deserialized_val
    integer(kind=int64) :: index
    logical :: status
        
        do index=1, src%numelements()
            call get_real(src%values(index)%obj, deserialized_val, status)
            dest(index) = deserialized_val
        end do

  END FUNCTION transfer_values_from_mp

end module runModule
