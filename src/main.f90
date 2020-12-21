program test
  use define
  use verlet
  use forces
  use runsim
  implicit none
  integer :: i
  type (vector),allocatable :: posnew(:),velnew(:),a(:)

  !READ THE INPUT FILE AND WRITE TO SCREEN INITIAL CONDITIONS
  call readin()
  allocate(posnew(nob),velnew(nob),a(nob))

  a=acc(pos)
  
  !PRINT TO SCREEN INITIAL ACCELERATIONS
  print *, "Initial accelerations"
  do i=1,nob
    print '(a,e11.3,e11.3,e11.3)',names(i),a(i)%x,a(i)%y,a(i)%z
  end do
  
  !PREDICTION FOR FIRST NEW POSITIONS AND VELOCITIES
  posnew=nextpos(pos,v,a)
  velnew=nextvel(v,acc(posnew),a)
 
  print *, "Initial predictions"  
  do i=1,nob
    print '(a,e11.3,e11.3,e11.3,e11.3,e11.3,e11.3)',&
            names(i),&
            posnew(i)%x,posnew(i)%y,posnew(i)%z,&
            velnew(i)%x,velnew(i)%y,velnew(i)%z
  end do

  !START THE SIMULATION
  print *,
  print *, "----------------"
  print *, "BEGIN SIMULATION"
  print *, "----------------"
  call run()
  print *, "----------------"
  print *, "SIMULATION DONE"
  print *, "----------------"
  

end program test
