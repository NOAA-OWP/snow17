program test

    implicit none

    integer				:: ierr, ios=0
    integer				:: yr,mnth,dy,hr
    integer				:: read_flag
    character(len = 1024)		:: filename
    real			        :: pcp,tma,tmn,pet
    integer             :: start_year, start_month, start_day, start_hour

    ! --- code ------------------------------------------------------------------
    start_year  = 1970
    start_month = 2
    start_day   = 1

      ! make filename to read
      filename = '../../test_cases/ex1/input/forcing/forcing_pet.mixed_ens.HHWM8IL'

      open(50, file = trim(filename), form = 'formatted', action = 'read', iostat = ios)
      
      ! Skip 1-line header
      read(50, *)
      
      ! advance to first record needed in simulation 
      do while(ios .ge. 0)
        ! forcing could have any format (not fixed width)
        read (UNIT=50,FMT=*,IOSTAT=ios) yr, mnth, dy, hr, pcp, tma, tmn, pet
        print*, yr, mnth, dy, hr

        if(yr .eq. start_year .and. mnth .eq. start_month .and. dy .eq. start_day) then
          print*, 'found start date'
          exit
        end if
        
      end do
      
      ! try to backspace the file to the previous record
      !backspace 50  ! check!

      read (UNIT=50,FMT=*,IOSTAT=ios) yr, mnth, dy, hr, pcp, tma, tmn, pet
      print*, yr, mnth, dy, hr

end program
