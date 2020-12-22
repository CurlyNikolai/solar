module define
  implicit none
  integer,parameter :: rk=selected_real_kind(6,15)
  type :: vector
    real(kind=rk) :: x,y,z
  end type vector
  interface operator(+)
    module procedure add
  end interface operator(+)
  interface operator(-)
    module procedure subtract
  end interface operator(-)
  interface operator(*)
    module procedure times,dot
  end interface operator(*)
  character(len=80) :: filein
  integer :: ios,nob,steps,printstep,savestep
  integer,allocatable :: ind(:)
  real(kind=rk),allocatable :: m(:)
  type (vector),allocatable :: v(:),pos(:)
  real(kind=rk) :: timestep,G=6.67408E-20
  character(len=10),allocatable :: names(:)


  contains

  !DEFINE MULTPILICATION BETWEEN SCALAR AND VECTOR
  type (vector) function times(c,v1)
    implicit none
    type (vector),intent(in) :: v1
    real,intent(in) :: c
    type (vector) :: v2
    v2=vector(c*v1%x,c*v1%y,c*v1%z)
    times=v2
  end function times

  !DEFINE DOTPRODUCT
  real function dot(v1,v2)
    implicit none
    type(vector),intent(in) :: v1,v2
    dot=v1%x*v2%x+v1%y*v2%y+v1%z*v2%z
  end function dot

  !DEFINE VECTOR ADDITION
  type (vector) function add(v1,v2)
    implicit none
    type (vector),intent(in) :: v1,v2
    real :: xnew,ynew,znew
    xnew=v1%x+v2%x
    ynew=v1%y+v2%y
    znew=v1%z+v2%z
    add=vector(xnew,ynew,znew)
  end function 

  !DEFINE VECTOR SUBTRACTION
  type (vector) function subtract(v1,v2)
    implicit none
    type (vector),intent(in) :: v1,v2
    real :: xnew,ynew,znew
    xnew=v1%x-v2%x
    ynew=v1%y-v2%y
    znew=v1%z-v2%z
    subtract=vector(xnew,ynew,znew)
  end function

  !DEFINE FUNCTION THAT RETURNS MAGNITUDE OF VECTOR
  real function veclen(v)
    implicit none
    type (vector),intent(in) :: v
    veclen=sqrt(v%x**2+v%y**2+v%z**2)
  end function veclen

 
  !SUB FOR READING THE INPUT FILE
  subroutine readin()
    implicit none
    real :: posx,posy,posz,vx,vy,vz
    integer :: i
    !OPEN ERROR FILE error.out
    open(unit=2,file="errors.out",iostat=ios,status='replace')
    if (ios/=0) then
      print '(a)',' **** Error in opening errors.out'
    end if

    !OPEN INPUT read.in FILE
    open(unit=1,file="input.dat",iostat=ios,status='old')
    if (ios/=0) then
      print '(a)',' **** Error in opening read.in'
    end if

    !READ PARAMETERS FROM read.in
    read(1,'(i2)',iostat=ios) nob
    if (ios/=0) then
      print '(a)',' **** Error in reading number of objects in read.in'
      write(2,'(a)') '**** Error in reading number of objects in read.in'
    end if
    allocate(ind(nob),m(nob),v(nob),pos(nob),names(nob))
    print '(a,i2)',"Number of objects = ",nob
    read(1,*,iostat=ios) timestep
    if (ios/=0) then
      print '(a)',' **** Error in reading timestep in read.in'
      write(2,'(a)') '**** Error in reading timestep in read.in'
    end if
    print '(a,f4.1)',"Chosen timestep   = ",timestep
    read(1,*,iostat=ios) steps
    if (ios/=0) then
      print '(a)',' **** Error in reading number of steps in read.in'
      write(2,'(a)') '**** Error in reading number of steps in read.in'
    end if
    print '(a,i10)',"Number of steps   = ",steps
    read(1,*,iostat=ios) printstep
    if (ios/=0) then
      print '(a)',' **** Error in reading print step interval in read.in'
      write(2,'(a)') '**** Error in reading print step interval in read.in'
    end if
    print '(a,i8)',"Print every nstep = ",printstep
    read(1,*,iostat=ios) savestep
    if (ios/=0) then
      print '(a)',' **** Error in reading write step interval in read.in'
      write(2,'(a)') '**** Error in reading write step interval in read.in'
    end if
    print '(a,i8)',"Save every nstep = ",savestep
    print *
    do i=1,nob
      if (i>nob .or. ios<0) exit
      read(1,'(a10,i3,e10.3,e10.3,e10.3,e10.3,e10.3,e10.3,e10.3)',iostat=ios) names(i),ind(i),m(i),posx,posy,posz,vx,vy,vz
      if (ios/=0) then
        print '(a)',' **** Error in reading object data in read.in'
        write(2,'(a)') '**** Error in reading object data in read.in'
        exit
      end if
      pos(i)=vector(posx,posy,posz)
      v(i)=vector(vx,vy,vz)
      print '(a,a)',"Name of object: ",names(i)
      print '(a,i2)',"Index of object: ",ind(i)
      print '(a,e10.3)',"Mass [kg]: ",m(i)
      print '(a,e10.3,e10.3,e10.3)',"Initial position (x,y) [km]: ",pos(i)%x,pos(i)%y,pos(i)%z
      print '(a,e10.3,e10.3,e10.3)',"Initial velocity (x,y) [km/s]: ",v(i)%x,v(i)%y,v(i)%z
      print *
    end do

    close(1)
    close(2)
  end subroutine readin

end module define
