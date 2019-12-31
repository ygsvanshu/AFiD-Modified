!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                         ! 
!    FILE: StatRoutines.F90                               !
!    CONTAINS: subroutine CalcStats,WriteStats            !
!                                                         ! 
!    PURPOSE: Calculates and writes out statistics for    !
!     the flow field. All quantities are averaged in the  !
!     two horizontal (homogeneous) directions             !
!                                                         !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!! Copied from earlier version on 31/12/2019 by Vanshu [Modification #2]

      subroutine CalcStats
      use param
      use local_arrays, only: vz,vy,vx,temp
      use decomp_2d, only: xstart,xend
      use stat_arrays
      use mpih
      implicit none
      real :: usnzm,usnym,factor
      integer :: i,j,k

      nstatsamples = nstatsamples + 1
      tstat        = tstat        + dt

      usnym = 1.0/nym
      usnzm = 1.0/nzm
      factor= usnym*usnzm*dt

      do i=xstart(3),xend(3)
       do j=xstart(2),xend(2)
        do k=1,nxm
! RS: Mean quantities
         vx_m1(k)   = vx_m1(k)   +   vx(k,j,i)*factor
         vy_m1(k)   = vy_m1(k)   +   vy(k,j,i)*factor
         vz_m1(k)   = vz_m1(k)   +   vz(k,j,i)*factor
         temp_m1(k) = temp_m1(k) + temp(k,j,i)*factor
! RS: Second moments
         vx_m2(k)   = vx_m2(k)   +   vx(k,j,i)**2*factor
         vy_m2(k)   = vy_m2(k)   +   vy(k,j,i)**2*factor
         vz_m2(k)   = vz_m2(k)   +   vz(k,j,i)**2*factor
         temp_m2(k) = temp_m2(k) + temp(k,j,i)**2*factor
! RS: Third moments
         vx_m3(k)   = vx_m3(k)   +   vx(k,j,i)**3*factor
         vy_m3(k)   = vy_m3(k)   +   vy(k,j,i)**3*factor
         vz_m3(k)   = vz_m3(k)   +   vz(k,j,i)**3*factor
         temp_m3(k) = temp_m3(k) + temp(k,j,i)**3*factor
! RS: Fourth moments
         vx_m4(k)   = vx_m4(k)   +   vx(k,j,i)**4*factor
         vy_m4(k)   = vy_m4(k)   +   vy(k,j,i)**4*factor
         vz_m4(k)   = vz_m4(k)   +   vz(k,j,i)**4*factor
         temp_m4(k) = temp_m4(k) + temp(k,j,i)**4*factor
! RS: heat flux 
         tempvx_m1(k)= tempvx_m1(k)+ temp(k,j,i)*vx(k,j,i)*factor
        enddo
       enddo
      enddo

      return  
      end
!    
!***********************************************************************
      subroutine WriteStats
      use mpih
      use param
      use stat_arrays
      use hdf5

      implicit none

      integer :: nstatsamples_old
      real    :: tstat_old

      character*30 dsetname_vxm1
      character*30 dsetname_vym1
      character*30 dsetname_vzm1

      character*30 dsetname_vxm2
      character*30 dsetname_vym2
      character*30 dsetname_vzm2

      character*30 dsetname_vxm3
      character*30 dsetname_vym3
      character*30 dsetname_vzm3

      character*30 dsetname_vxm4
      character*30 dsetname_vym4
      character*30 dsetname_vzm4

      character*30 dsetname_tempm1
      character*30 dsetname_tempm2
      character*30 dsetname_tempm3
      character*30 dsetname_tempm4

      character*30 dsetname_tempvxm1
      
      character*30 dsetname_dissth
      character*30 dsetname_disste
      character*30 filnam,dsetname,dsetname2
      logical :: fexist

      filnam = trim('stafield_master.h5')

      dsetname_vxm1 = trim('vx_m1')
      dsetname_vym1 = trim('vy_m1')
      dsetname_vzm1 = trim('vz_m1')

      dsetname_vxm2 = trim('vx_m2')
      dsetname_vym2 = trim('vy_m2')
      dsetname_vzm2 = trim('vz_m2')

      dsetname_vxm3 = trim('vx_m3')
      dsetname_vym3 = trim('vy_m3')
      dsetname_vzm3 = trim('vz_m3')

      dsetname_vxm4 = trim('vx_m4')
      dsetname_vym4 = trim('vy_m4')
      dsetname_vzm4 = trim('vz_m4')

      dsetname_tempm1 = trim('temp_m1')
      dsetname_tempm2 = trim('temp_m2')
      dsetname_tempm3 = trim('temp_m3')
      dsetname_tempm4 = trim('temp_m4')

      dsetname_tempvxm1 = trim('tempvx_m1')

      dsetname_dissth = trim('dissth')
      dsetname_disste = trim('disste')

      dsetname = trim('averaging_time')
      dsetname2 = trim('averaging_time2')

      inquire(file=filnam,exist=fexist)
      if (.not.fexist) then 
        if(ismaster) write(6,*) 'Unable to read statistical files'
        if(ismaster) write(6,*) 'Restarting statistics from zero' 
        readstats=.false.
      end if
       

      if (ismaster) then
       if(readstats) then
        call HdfSerialReadIntScalar(dsetname,filnam,nstatsamples_old)
        call HdfSerialReadRealScalar(dsetname2,filnam,tstat_old)
        nstatsamples = nstatsamples + nstatsamples_old
        tstat       = tstat       + tstat_old
       else 
        call HdfCreateBlankFile(filnam)
       endif
      end if

      call StatReadReduceWrite(vx_m1,filnam,dsetname_vxm1)
      call StatReadReduceWrite(vy_m1,filnam,dsetname_vym1)
      call StatReadReduceWrite(vz_m1,filnam,dsetname_vzm1)

      call StatReadReduceWrite(vx_m2,filnam,dsetname_vxm2)
      call StatReadReduceWrite(vy_m2,filnam,dsetname_vym2)
      call StatReadReduceWrite(vz_m2,filnam,dsetname_vzm2)

      call StatReadReduceWrite(vx_m3,filnam,dsetname_vxm3)
      call StatReadReduceWrite(vy_m3,filnam,dsetname_vym3)
      call StatReadReduceWrite(vz_m3,filnam,dsetname_vzm3)

      call StatReadReduceWrite(vx_m4,filnam,dsetname_vxm4)
      call StatReadReduceWrite(vy_m4,filnam,dsetname_vym4)
      call StatReadReduceWrite(vz_m4,filnam,dsetname_vzm4)

      call StatReadReduceWrite(temp_m1,filnam,dsetname_tempm1)
      call StatReadReduceWrite(temp_m2,filnam,dsetname_tempm2)
      call StatReadReduceWrite(temp_m3,filnam,dsetname_tempm3)
      call StatReadReduceWrite(temp_m4,filnam,dsetname_tempm4)

      call StatReadReduceWrite(tempvx_m1,filnam,dsetname_tempvxm1)

      if(disscal) then 
       call StatReadReduceWrite(dissth,filnam,dsetname_dissth)
       call StatReadReduceWrite(disste,filnam,dsetname_disste)
      end if

      if (ismaster) then

       write(*,*) 'tstat',tstat
       call HdfSerialWriteIntScalar(dsetname,filnam,nstatsamples)
       call HdfSerialWriteRealScalar(dsetname2,filnam,tstat)

       dsetname = trim('X_cordin')
       call HdfSerialWriteReal1D(dsetname,filnam,xm,nxm)

       dsetname = trim('Rayleigh Number')
       call HdfSerialWriteRealScalar(dsetname,filnam,ray)

       dsetname = trim('Prandtl Number')
       call HdfSerialWriteRealScalar(dsetname,filnam,pra)

      endif

      return  
      end

!! End [Modification #2]