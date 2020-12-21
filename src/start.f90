module runsim
  use define
  use verlet
  use forces
  use energy
  implicit none

contains

  !SUB FOR RUNNING SIMULATION
  subroutine run()
    implicit none
    type (vector) :: posnow(nob),velnow(nob),posnext(nob),velnext(nob),anow(nob),anext(nob)
    real :: etotnow
    integer :: i,j

    !DEFINE THE FIRST POSITIONS, ACCELERATIONS AND VELOCITIES
    posnow=pos
    anow=acc(posnow)
    velnow=v

    !CREATE MOVIE FILE TO SAVE POSITIONS
    open(unit=1,file="output.dat",iostat=ios,status='replace')
    if (ios/=0) then
      print '(a)',"**** Error in opening movie.xyz"
    end if

    !SIMULATION LOOP, EXIT WHEN ITERATION STEP EXCEEDS NUMBER OF STEPS TO SIMULATE
    i=1
    do
      if (i>steps) exit
      !PREDICT NEXT POSITIONS
      posnext=nextpos(posnow,velnow,anow)
      anext=acc(posnext)
      velnext=nextvel(velnow,anext,anow)

      !UPDATE POSITIONS
      posnow=posnext
      anow=anext
      velnow=velnext
      etotnow=etot(velnow,anow,posnow)
      
      !WRITE FIRST STEP TO SCREEN AND MOVIE FILE
      if (i==1) then
        print '(i1)',nob
        print '(a,i5,a,f7.3,a,i10,a,e11.3,f10.1,a)',&
                "Framenumber: ",i/savestep,&
                " Time [yr]: ",i*timestep/(31558149.504),&
                " Step: ",i,&
                " Energy: ",etotnow,&
                100.0*i/steps,"% done"

        do j=1,nob
          print '(a,i3,e11.3,e11.3,e11.3,e11.3,e11.3,e11.3)',&
                names(j),ind(j),&
                posnow(j)%x,posnow(j)%y,posnow(j)%z,&
                velnow(j)%x,velnow(j)%z,velnow(j)%z
        end do

        write(1,'(i1)') nob
        write(1,'(a,i5,a,f7.3,a,i10,a,e11.3)') &
                  "Framenumber: ",i/savestep,&
                  " Time [yr]: ",i*timestep/(31558149.504),&
                  " Step: ",i,&
                  " Energy: ",etotnow

        do j=1,nob
          write(1,'(a10,i3,e11.3,e11.3,e11.3,e11.3,e11.3,e11.3)')&
                   names(j),ind(j),&
                   posnow(j)%x,posnow(j)%y,posnow(j)%z,&
                   velnow(j)%x,velnow(j)%y,velnow(j)%z
        end do
      end if

      !PRINT EVERY STEP DEFINED BY INPUT FILE TO SCREEN
      if (mod(i,printstep)==0) then
        print '(i1)',nob
        print '(a,i5,a,f7.3,a,i10,a,e11.3,f10.1,a)',&
                "Framenumber: ",i/savestep,&
                " Time [yr]: ",i*timestep/(31558149.504),&
                " Step: ",i,&
                " Energy: ",etotnow,&
                100.0*i/steps,"% done"

        do j=1,nob
          print '(a,i3,e11.3,e11.3,e11.3,e11.3,e11.3,e11.3,e11.3)',&
                  names(j),ind(j),&
                  posnow(j)%x,posnow(j)%y,posnow(j)%z,&
                  velnow(j)%x,velnow(j)%y,velnow(j)%z
        end do
      end if

      !WRITE EVERY STEP DEFINED BY INPUT FILE TO MOVIE FILE
      if (mod(i,savestep)==0) then
        write(1,'(i1)') nob
        write(1,'(a,i5,a,f7.3,a,i10,a,e11.3)') &
                  "Framenumber: ",i/savestep,&
                  " Time [yr]: ",i*timestep/(31558149.504),&
                  " Step: ",i,&
                  " Energy: ",etotnow

        do j=1,nob
          write(1,'(a10,i3,e11.3,e11.3,e11.3,e11.3,e11.3,e11.3)')&
                    names(j),ind(j),&
                    posnow(j)%x,posnow(j)%y,posnow(j)%z,&
                    velnow(j)%x,velnow(j)%y,velnow(j)%z
        end do
      end if

      !WRITE FINAL STEP TO SCREEN AND MOVIE FILE
      if (i==steps) then
        print '(i1)',nob
        print '(a,i5,a,f7.3,a,i10,a,e11.3,f10.1,a)',&
                "Framenumber: ",i/savestep,&
                " Time [yr]: ",i*timestep/(31558149.504),&
                " Step: ",i,&
                " Energy: ",etotnow,&
                100.0*i/steps,"% done"
        do j=1,nob
          print '(a,i3,e11.3,e11.3,e11.3,e11.3,e11.3,e11.3)',&
                  names(j),ind(j),&
                  posnow(j)%x,posnow(j)%y,posnow(j)%z,&
                  velnow(j)%x,velnow(j)%y,velnow(j)%z
        end do
        write(1,'(i1)') nob
        write(1,'(a,i5,a,f7.3,a,i10,a,e11.3)') &
                  "Framenumber: ",i/savestep,&
                  " Time [yr]: ",i*timestep/(31558149.504),&
                  " Step: ",i,&
                  " Energy: ",etotnow
        do j=1,nob
          write(1,'(a10,i3,e11.3,e11.3,e11.3,e11.3,e11.3,e11.3)')&
                    names(j),ind(j),&
                    posnow(j)%x,posnow(j)%y,posnow(j)%z,&
                    velnow(j)%x,velnow(j)%y,velnow(j)%z
        end do
      end if
      i=i+1
    end do

    close(1) 

  end subroutine run


end module runsim
