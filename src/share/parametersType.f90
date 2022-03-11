module parametersType

use namelistRead, only: namelist_type

implicit none
save
private

type, public :: parameters_type

  !real, allocatable, dimension(:) :: bexp                      ! b parameter
  !real, allocatable, dimension(:) :: smcmax                    ! porosity (volumetric)
  !real, dimension(12)             :: LAIM                      ! monthly LAI
  !real                            :: LAI

  ! Snow17 model params in the snow param file
  character(len = 20), dimension(:), allocatable :: hru_id   ! local hru id
  real(dp), dimension(:), allocatable            :: hru_area   ! sq-km, needed for combination & routing conv.
  real(dp), dimension(:), allocatable            :: latitude   ! decimal degrees
  real(dp), dimension(:), allocatable            :: elev       ! m
  real(sp), dimension(:), allocatable            :: scf,mfmax,mfmin,uadj,si,pxtemp
  real(sp), dimension(:), allocatable            :: nmf,tipm,mbase,plwhc,daygm
  real(sp), dimension(11)                        :: adc   ! AW can we keep this the same for all HUCs, for now?

  contains

    procedure, public  :: Init
    procedure, public  :: read_snow17_params

end type parameters_type

contains

  subroutine Init(this, namelist)

    implicit none
    class(parameters_type)           :: this
    class(namelist_type), intent(in) :: namelist
    
    ! local variables
    n_hrus = namelist%n_hrus    ! could use associate here

    ! allocate variables
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
    
    ! assign from input parameters file
    call read_snow17_parameters(this, namelist%snow17_param_file)
    
    ! assign defaults (if any)
    !this%LAI        = huge(1.0)
    !this%VEG        = .true.

  end subroutine Init


  subroutine read_snow17_params(this, param_file_name)
    implicit none
    !use nrtype
   
    ! input/output variables
    class(parameters_type), intent(inout)  :: this
    character(len=1024), intent(in)        :: param_file_name
  
    ! local variables
    character(len=400)      :: readline
    character(len=50)		:: param
    integer    	            :: ios=0   ! specify i4b with nrtype?
    integer                 :: pos
    integer                 :: n_params_read  ! count number read
  
    ! open parameter file
    open(unit=51,file=trim(param_file_name),status='old')
  
    print*, 'Reading Snow17 parameters'
  
    ! --- now loop through parameter file and assign parameters 
    n_params_read = 0
    do while(ios .eq. 0)
      read(unit=51,FMT='(A)',IOSTAT=ios) readline
  
      if(ios == 0) then   ! means 'readline' was from the file
        !print*, '  ',trim(readline)
  
        ! Find the first instance of whitespace in line read. Split label vs data.
        pos      = scan(readline, '    ')
        param    = trim(readline(1:pos))
        readline = readline(pos+1:)  ! shorten readline to include only data
  
        ! assign line to the correct parameter array & type
        ! (following http://jblevins.org/log/control-file)
        ! should automatically read multiple values into the variable arrays if n_hrus > 1
        select case (param)
         case ('hru_id')
            read(readline, *, iostat=ios) this%hru_id
            n_params_read = n_params_read + 1
          case ('hru_area')
            read(readline, *, iostat=ios) this%hru_area
            n_params_read = n_params_read + 1
          case ('latitude')
            read(readline, *, iostat=ios) this%latitude
            n_params_read = n_params_read + 1
          case ('elev')
            read(readline, *, iostat=ios) this%elev
            n_params_read = n_params_read + 1
          case ('mfmax')
            read(readline, *, iostat=ios) this%mfmax
            n_params_read = n_params_read + 1
          case ('mfmin')
            read(readline, *, iostat=ios) this%mfmin
            n_params_read = n_params_read + 1
          case ('scf')
            read(readline, *, iostat=ios) this%scf
            n_params_read = n_params_read + 1
          case ('uadj')
            read(readline, *, iostat=ios) this%uadj
            n_params_read = n_params_read + 1
          case ('si')
            read(readline, *, iostat=ios) this%si
            n_params_read = n_params_read + 1
          case ('pxtemp')
            read(readline, *, iostat=ios) this%pxtemp
            n_params_read = n_params_read + 1
          case ('nmf')
            read(readline, *, iostat=ios) this%nmf
            n_params_read = n_params_read + 1
          case ('tipm')
            read(readline, *, iostat=ios) this%tipm
            n_params_read = n_params_read + 1
          case ('mbase')
            read(readline, *, iostat=ios) this%mbase
            n_params_read = n_params_read + 1
          case ('plwhc')
            read(readline, *, iostat=ios) this%plwhc
            n_params_read = n_params_read + 1
          case ('daygm')
            read(readline, *, iostat=ios) this%daygm
            n_params_read = n_params_read + 1
          case ('adc1')
            read(readline, *, iostat=ios) this%adc(1)
            n_params_read = n_params_read + 1
          case ('adc2')
            read(readline, *, iostat=ios) this%adc(2)
            n_params_read = n_params_read + 1
          case ('adc3')
            read(readline, *, iostat=ios) this%adc(3)
            n_params_read = n_params_read + 1
          case ('adc4')
            read(readline, *, iostat=ios) this%adc(4)
            n_params_read = n_params_read + 1
          case ('adc5')
            read(readline, *, iostat=ios) this%adc(5)
            n_params_read = n_params_read + 1
          case ('adc6')
            read(readline, *, iostat=ios) this%adc(6)
            n_params_read = n_params_read + 1
          case ('adc7')
            read(readline, *, iostat=ios) this%adc(7)
            n_params_read = n_params_read + 1
          case ('adc8')
            read(readline, *, iostat=ios) this%adc(8)
            n_params_read = n_params_read + 1
          case ('adc9')
            read(readline, *, iostat=ios) this%adc(9)
            n_params_read = n_params_read + 1
          case ('adc10')
            read(readline, *, iostat=ios) this%adc(10)
            n_params_read = n_params_read + 1
          case ('adc11')
            read(readline, *, iostat=ios) this%adc(11)
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


end module parametersType
