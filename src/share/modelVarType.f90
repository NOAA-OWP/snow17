module modelVarType

use namelistModule, only: namelist_type
use ioModule

implicit none
save

type, public :: modelvar_type

  ! main model states and flux variable
  real, dimension(:), allocatable    :: raim   ! rain and melt output
  real, dimension(:), allocatable    :: sneqv  ! snow water equivalent (unit)
  real, dimension(:), allocatable    :: snow   ! check this
  real, dimension(:), allocatable    :: snowh  ! snow height
  
  ! other states and carryover variables
  real, dimension(:), allocatable    :: tprev
  real, dimension(:,:), allocatable  :: cs     ! 19-element vector per HRU used in snow19: (n_hrus,19)
  
  ! areally-averaged variables for output 
  real                               :: sneqv_comb, snowh_comb, snow_comb, raim_comb ! snow model
  
  contains

    procedure, public  :: Init 
    procedure, public  :: assignStates  

end type modelvar_type

contains   

  subroutine Init(this, namelist)

    ! define variables
    class(modelvar_type), intent(out) :: this
    type(namelist_type), intent(in)   :: namelist
    
    ! -- variable allocations (time dim not needed since forcings are one-rec scalars)
    allocate(this%raim  (1:namelist%n_hrus))
    allocate(this%sneqv (1:namelist%n_hrus))
    allocate(this%snowh (1:namelist%n_hrus))
    allocate(this%snow  (1:namelist%n_hrus))
    allocate(this%tprev (1:namelist%n_hrus))
    allocate(this%cs    (1:namelist%n_hrus, 1:19))
    
    ! -- default assignments
    this%raim(:)       = huge(1.0)
    this%sneqv(:)      = huge(1.0) 
    this%snowh(:)      = huge(1.0) 
    this%snowh(:)      = huge(1.0) 
    this%snow(:)       = huge(1.0) 
    this%tprev(:)      = huge(1.0) 
    this%cs(:,:)       = huge(1.0) 
    this%sneqv_comb    = huge(1.0) 
    this%snowh_comb    = huge(1.0) 
    this%snow_comb     = huge(1.0) 
    this%raim_comb     = huge(1.0) 
    
    ! -- estimate derived variables

  end subroutine Init
  
  subroutine assignStates(this, namelist)

    ! define variables
    class(modelvar_type), intent(inout) :: this
    type(namelist_type), intent(in)     :: namelist

    ! set single precision sac state variables to initial values
    if(namelist%warm_start_run .eq. 0) then
      ! we are not warm starting from a state file
      this%cs(:,1)    = 0      ! first/main component of cs() array is SWE (model 'WE'); could do something different here
      this%cs(:,2:19) = 0      !   set the rest of cs() to zero
      this%tprev(:)   = 0      ! prev. temp is needed
      
    else
      ! we *ARE* warm-starting from a state file
      ! read in external state files and overwrites namelist state variables 
 
      call read_snow17_state(this%cs, this%tprev, namelist)

    endif

    end subroutine assignStates

end module modelVarType
