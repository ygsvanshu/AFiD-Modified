!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                         !
!    FILE: WriteFlowFieldSnapshot.F90                     !
!    CONTAINS: subroutine WriteFlowFieldSnapshot          !
!                                                         !
!    PURPOSE: Write down the full snapshot of temp,       !
!    vx,vy,vz for post processing purposes                !
!                                                         !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!! Modified on 30/12/2019 by Vanshu [Modification #1]

subroutine WriteFlowFieldSnapshot

    use param
    use local_arrays, only:vz,vy,vx,temp

    implicit none

    character*30 :: filnam1,dsetname,namfile
    integer :: itime
    character*8 :: citime

    itime = nint(time)
    write(citime,"(I8.8)") itime

    filnam1 = trim('continua_temp')
    namfile=trim(trim(filnam1)//trim(citime)//'.h5')
    call HdfWriteRealHalo3D(namfile,temp)

    filnam1 = trim('continua_vx')
    namfile=trim(trim(filnam1)//trim(citime)//'.h5')
    call HdfWriteRealHalo3D(namfile,vx)
    
    filnam1 = trim('continua_vy')
    namfile=trim(trim(filnam1)//trim(citime)//'.h5')
    call HdfWriteRealHalo3D(namfile,vy)
    
    filnam1 = trim('continua_vz')
    namfile=trim(trim(filnam1)//trim(citime)//'.h5')
    call HdfWriteRealHalo3D(namfile,vz)

end subroutine WriteFlowFieldSnapshot

!! End [Modification #1]

