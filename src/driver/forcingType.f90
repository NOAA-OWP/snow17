module forcingType

use namelistModule, only: namelist_type

implicit none
save
private

type, public :: forcing_type

  ! atmospheric inputs (meteorology)
  real       :: tair       ! surface air temperature [K]
  real       :: precip     ! total input precipitation[mm/s]
  real(dp), dimension(:), allocatable   :: pa   ! snow17 surface pressure (pa) for n_hrus

  contains

    procedure, public  :: Init         

end type forcing_type

contains   

  subroutine Init(this, namelist)

    class(forcing_type) :: this
    type(namelist_type) :: namelist
    
    ! -- variable allocations (time dim not needed since forcings are one-rec scalars)
    allocate(this%pa (1:namelist%n_hrus))
    
    ! -- default assignments
    !this%precip(:) = huge(1.0)   ! if adding back the time or space (hru) dimension
    this%precip  = huge(1.0) 
    this%tair    = huge(1.0) 
    this%pa(:)   = huge(1.0) 
    
    ! -- derived variables

    ! calc sfc_pressure 
    do nh=1,n_hrus
      call sfc_pressure(elev(nh), pa(nh))
    end do

  end subroutine Init

end module forcingType
