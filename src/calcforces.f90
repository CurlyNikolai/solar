module forces
  use define
  implicit none


contains

  !FUNCTION FOR CALCULATING ACCELERATION VECTOR
  function acc(pos)
    implicit none
    type (vector),intent(in) :: pos(nob)
    integer :: i,j
    type (vector) :: diff,acc(nob)
    type (vector) :: ahelp(nob)

    ahelp=vector(0,0,0)

    do i=1,nob
      do j=1,nob
        if (i==j) cycle
        diff=pos(i)-pos(j)
        ahelp(i)=ahelp(i)+(-G*m(j)/(veclen(diff)**3))*(diff)
      end do
    end do
   
    acc=ahelp

  end function acc


end module forces
