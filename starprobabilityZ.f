      subroutine starprobabilityz(LK)
      implicit none
      include 'parmsZ.inc'
      integer i,j, nzero
      real*8 chiU,chiBV,chiVV,chiVI,chiVR,sigma,chi,prod
      real*8 DM,LK,EBV,EVI,EVR,PL,P(nobs)

c *** local values of DM and E(B-V)  
      DM  = parms(3)
      EBV = parms(4)

c *** initialise probability
      PL = 0.d0
      nzero = 0  
      prod = 1.d0

      EVI = 1.30d0*EBV  !Reddening in V-I 
      EVR = 0.60d0*EBV  !Reddening in V-R      

!      open(14,file='../output/starprob.check')
      do i=1, nobs


         chiVV =  (ObsVV(i) - (TheoVV(1)          +DM)  )
         chiBV =  (ObsBV(i) - (TheoBB(1)-TheoVV(1)+EBV) )
         chiVI =  (ObsVI(i) - (TheoVV(1)-TheoII(1)+EVI) )
         chiVR =  (ObsVR(i) - (TheoVV(1)-TheoRR(1)+EVR) )

         sigma = errVV(i)**2 + errBV(i)**2 !+ errVI(i)**2 + errVR(i)**2

         chi = dsqrt( chiVV**2 + chiBV**2 )/dsqrt(sigma)


         do j=2, 398

           chiVV =  (ObsVV(i) - (TheoVV(j)          +DM)  )
           chiBV =  (ObsBV(i) - (TheoBB(j)-TheoVV(j)+EBV) )
           chiVI =  (ObsVI(i) - (TheoVV(j)-TheoII(j)+EVI) )
           chiVR =  (ObsVR(i) - (TheoVV(1)-TheoRR(1)+EVR) )

           sigma = errVV(i)**2 + errBV(i)**2 !+ errVI(i)**2 + errVR(i)**2

           chi = min(chi,dsqrt( chiVV**2 + chiBV**2 )/dsqrt(sigma))

!           prod = prod * derf(chi)          
!           write(14,*) chiVV, chiBV, chiVI, chi 
         enddo

c ***    probability of star i
         P(i) = 1.d0 - derf(chi) 

         if(derf(chi) .lt. 1.d0) then
            PL = PL + dlog(P(i))
         else
            nzero = nzero + 1
         endif
      enddo
c     probability (including tempering factor beta)
      LK = dexp(beta * PL)

c      write(*,*) LK

      if(nzero .gt. 0) write(*,*) nzero,'stelle non contate' 

!      close(14)
      
c      stop

      return
      end


