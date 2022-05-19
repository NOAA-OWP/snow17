! AWW:  this code file just contains interfaces for functions & subs 

module interfaces



  interface 

    subroutine julian_day(year,month,day,jday)
      use nrtype
      !input variables
      integer(I4B),dimension(:),intent(in) :: year
      integer(I4B),dimension(:),intent(in) :: month
      integer(I4B),dimension(:),intent(in) :: day
      !output variables
      integer(I4B),dimension(:),intent(out) :: jday
    end subroutine julian_day

    subroutine julianday_scalar(iyear,imonth,iday,jday_scalar)
      use nrtype
      integer(I4B),intent(in) :: iyear
      integer(I4B),intent(in) :: imonth
      integer(I4B),intent(in) :: iday
      integer(I4B),intent(out) :: jday_scalar
    end subroutine julianday_scalar

    ! AWW (to replace get_simlength subroutine)
    subroutine date_diff_ndays ( yr1, mo1, dy1, yr2, mo2, dy2, nday_diff )
      use nrtype
      ! input variables
      integer(I4B),intent(in)  :: yr1, mo1, dy1, yr2, mo2, dy2  
      ! output variables
      integer(I4B),intent(out) :: nday_diff
    end subroutine date_diff_ndays

    subroutine day_before_date(year, month, day, newyear, newmonth, newday)
      use nrtype
      ! input variables
      integer(I4B),intent(in)  :: year       ! starting date
      integer(I4B),intent(in)  :: month
      integer(I4B),intent(in)  :: day
      ! output variables
      integer(I4B),intent(out)  :: newyear   ! ending date
      integer(I4B),intent(out)  :: newmonth
      integer(I4B),intent(out)  :: newday
    end subroutine day_before_date   

    subroutine read_namelist(namelist_name)
      use nrtype
      !input variable
      character(len=2000),intent(in)	:: namelist_name
    end subroutine read_namelist

    subroutine sfc_pressure(elevation, pres)
      use nrtype
      use constants, only: sfc_pres_a,sfc_pres_b,sfc_pres_c,&
                       sfc_pres_d,sfc_pres_e
      real(dp), intent(in)		:: elevation
      real(dp), intent(out)		:: pres
    end subroutine sfc_pressure

    subroutine write_snow17_state(year,month,day,hour,cs,tprev,sim_length,curr_hru_id)
      use nrtype
      use defNamelist, only: snow_state_out_root
      !input variables
      character(len = 20), intent(in) 	:: curr_hru_id	! HRU extension for state fname
      integer(I4B),dimension(:),intent(in)	:: year
      integer(I4B),dimension(:),intent(in)	:: month
      integer(I4B),dimension(:),intent(in)	:: day
      integer(I4B),dimension(:),intent(in)	:: hour
      real(sp),dimension(:,:),intent(in)  :: cs	    ! carry over array
      real(sp),dimension(:),intent(in)	  :: tprev    ! carry over variable
      integer(I4B),intent(in)             :: sim_length   ! length of simulation
    end subroutine write_snow17_state

    subroutine read_snow17_state(state_date_str, cs,tprev,curr_hru_id)
      use nrtype
      use defNamelist, only: snow_state_in_root
      ! input variables
      character(len = 10), intent(in) :: state_date_str  ! AWW string to match date in input states
      character(len = 20), intent(in) :: curr_hru_id	! HRU extension for snow state filename
      ! output variables
      real(sp), intent(out) 			:: tprev	! carry over variable
      real(sp), dimension(:), intent(out)	:: cs		! carry over snow var array
    end subroutine read_snow17_state

    ! subroutine read_areal_forcing(year,month,day,hour,tmin,tmax,vpd,dayl,swdown,precip)
    ! AWW mod to just read PET
    subroutine read_areal_forcing(year,month,day,hour,tmin,tmax,precip,pet,curr_hru_id)
      use nrtype
      use defNamelist, only: forcing_root, start_datehr, end_datehr
      !output variables
      character(len = 20), intent(in) 	:: curr_hru_id	! HRU extension for state fname
      integer(I4B),dimension(:),intent(out)	:: year
      integer(I4B),dimension(:),intent(out)	:: month
      integer(I4B),dimension(:),intent(out)	:: day
      integer(I4B),dimension(:),intent(out)	:: hour
      real(dp),dimension(:),intent(out)	:: tmin
      real(dp),dimension(:),intent(out)	:: tmax
      real(dp),dimension(:),intent(out)	:: pet
      real(dp),dimension(:),intent(out)	:: precip    
    end subroutine read_areal_forcing

    subroutine read_snow17_params(param_name, n_hrus)
      use nrtype
      use parametersType, only: parameters_type
      !input variables
      character(len=1024),intent(in)	:: param_name
      integer(I4B),intent(in) :: n_hrus
    end subroutine read_snow17_params
    
    SUBROUTINE write_output_vec(namelist, runinfo, parameters, forcing, modelvar)
      implicit none
      class (namelist_type),    intent(in)   :: namelist
      class (runinfo_type),     intent(in)   :: runinfo
      class (parameters_type),  intent(in)   :: parameters
      class (forcing_type),     intent(in)   :: forcing
      class (modelvar_type),    intent(in)   :: modelvar
    end subroutine write_output_vec

  end interface
end module interfaces

