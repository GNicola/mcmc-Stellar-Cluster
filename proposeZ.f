      subroutine proposeZ(scfac,idum)
c *** provides the next proposed step of the Markov chain
      implicit none
      include 'parmsZ.inc'
      integer i, kstep
      real gasdev
      real*8 scfac,Logparm,parmx(nparms)
      integer idum       
      external gasdev


      parmx(1)   = 10**(parms(1))
      parmx(2)   = parms(2)
      parmx(3)   = parms(3)
      parmx(4)   = parms(4)

!123   continue

      do i=1,nparms
!        write(*,*) i, parms(i) 
123   continue
        Logparm = log10(parmx(i)) + jumps(i) * dble(gasdev(idum)) * scfac
        if (i .eq. 1) then 
          parms(i) = (Logparm)
        else 
          parms(i) = 10**(Logparm)
        endif
!         write(*,*) i, parms(i),'pippo'         
        if ((parms(1).lt. minAge)  .or.(parms(1).gt. maxAge)) goto 123
        if ((parms(2).lt. minZ)    .or.(parms(2).gt. maxZ))   goto 123
        if ((parms(3).lt. minDM)   .or.(parms(3).gt. maxDM))  goto 123
        if ((parms(4).lt. minAv)   .or.(parms(4).gt. maxAv))  goto 123
      enddo

!      write(*,*) 'propose works'
      return
      end
