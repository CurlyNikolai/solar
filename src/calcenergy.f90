module energy
  use define
  implicit none
  
contains

  function ekin(v)
    implicit none
    real :: ekin(nob)
    type (vector),intent(in) :: v(nob)
    integer :: i
    do i=1,nob
      ekin(i)=0.5*m(i)*veclen(v(i))**2
    end do
  end function

  function epot(a,pos)
    implicit none
    real :: epot(nob)
    type (vector),intent(in) :: a(nob),pos(nob)
    integer :: i
    do i=1,nob
      epot(i)=m(i)*a(i)*pos(i)
    end do
  end function epot

  real function etot(v,a,pos)
    implicit none
    real :: e(nob),ehelp
    type (vector) :: v(nob),a(nob),pos(nob)
    integer :: i
 
    ehelp=0.0
    e=ekin(v)+epot(a,pos)
    do i=1,nob
      ehelp=ehelp+e(i)
    end do 

    etot=ehelp

  end function etot

end module energy
