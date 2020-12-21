module verlet
  use define
  implicit none

contains

  !FUNCTION FOR PREDICTING NEXT POSITION VECTOR
  function nextpos(pos,v,a)
    implicit none
    type (vector),intent(in) :: pos(nob),v(nob),a(nob)
    type (vector) :: nextpos(nob)
    integer :: i

    do i=1,nob
      nextpos(i)=pos(i)+timestep*v(i)+0.5*timestep**2*a(i)
    end do

  end function nextpos

  !FUNCTION FOR PREDICTING NEXT VELOCITY VECTOR
  function nextvel(v,a_new,a_old)
    implicit none
    type (vector),intent(in) :: v(nob),a_new(nob),a_old(nob)
    type (vector) :: nextvel(nob)
    integer :: i

    do i=1,nob
      nextvel(i)=v(i)+0.5*timestep*(a_old(i)+a_new(i))
    end do

  end function
  

end module verlet
