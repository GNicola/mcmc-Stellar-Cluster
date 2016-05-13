      subroutine methastZ(Pnew,Pold,reject)
c *** Metropolis-Hastings decision maker
      implicit none
      real*8 Pnew,Pold
      logical reject
      real*8 dchisq,pjump,prob,alpha
      integer idum
      real ran1
      external ran1

      idum = time()
      idum = mod(idum,50000)


!      write(*,*) Pnew, Pold


      alpha = min(Pnew/Pold,1.d0)

      prob = dble(ran1(idum))

      if ( prob .lt. alpha ) then
c *** OK, accept the proposal
           reject = .false.
!	   write(*,*) alfa,'>',prob
      else
c *** No, reject the proposal
           reject = .true.
!	   write(*,*) chsold, chsnew, alfa,'<',prob
      end if

      return
      end
