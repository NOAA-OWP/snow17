module forcingType

use namelistModule, only: namelist_type

implicit none
save
!private

type, public :: forcing_type

  ! atmospheric inputs & outputs (surface meteorology)
  real, dimension(:), allocatable   :: tair       ! surface air temperature [K]
  real, dimension(:), allocatable   :: precip     ! total input precipitation [mm/s]
  real, dimension(:), allocatable   :: precip_scf ! total input precipitation with SCF applied [mm/s]
  real, dimension(:), allocatable   :: pa         ! snow17 surface pressure (Pa) 
  real                              :: precip_comb, precip_scf_comb, tair_comb    ! areally averaged forcings across HRUs

  contains

    procedure, public   :: Init         
    procedure, private  :: sfc_pressure

end type forcing_type

contains   

  subroutine Init(this, namelist)

    class(forcing_type), intent(out) :: this
    type(namelist_type), intent(in)  :: namelist

    ! local variables
    integer             :: n_hrus

    ! code
    n_hrus = namelist%n_hrus    ! could use associate here
    
    ! -- variable allocations (time dim not needed since forcings are one-rec scalars)
    !allocate(this%tair       (1:namelist%n_hrus))
    !allocate(this%precip     (1:namelist%n_hrus))
    !allocate(this%precip_scf (1:namelist%n_hrus))
    !allocate(this%pa         (1:namelist%n_hrus))
    allocate(this%tair       (n_hrus))
    allocate(this%precip     (n_hrus))
    allocate(this%precip_scf (n_hrus))
    allocate(this%pa         (n_hrus))
    
    ! -- default assignments
    this%precip(:)         = huge(1.0)
    this%precip_scf(:)     = huge(1.0)
    this%tair(:)           = huge(1.0)
    this%pa(:)             = huge(1.0)
    this%precip_comb       = huge(1.0)
    this%tair_comb         = huge(1.0)
    this%precip_scf_comb   = huge(1.0)
    
    ! -- estimate derived variables

    ! calc sfc_pressure 
    do nh=1,n_hrus
      call sfc_pressure(namelist%elev(nh), this%pa(nh))
    end do

  end subroutine Init
  
  
  subroutine sfc_pressure(elevation, sfc_pres)
    use nrtype
    use constants, only: sfc_pres_a,sfc_pres_b,sfc_pres_c,sfc_pres_d,sfc_pres_e

    implicit none

    real(DP), intent(in)   :: elevation
    real(DP), intent(out)  :: sfc_pres
  
    sfc_pres = sfc_pres_a * (sfc_pres_b - (sfc_pres_c * (elevation/100.0_dp)) &
               + (sfc_pres_d*((elevation/100.0_dp)**sfc_pres_e)))   !sfc pres in hPa

    return
  
  end subroutine sfc_pressure
  
  ! -- could add forcing file open & read routines below
  

end module forcingType
