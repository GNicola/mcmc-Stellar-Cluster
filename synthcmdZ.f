      subroutine synthcmdZ(LK)
      implicit none
      include 'parmsZ.inc'
      integer jage, izz 
      integer i,j,k, idum
      real*8 Age,ZZ,DM,EBV,LK,TheoAge,TheoZZ,theoz1,theoz2,theoaa1,theoaa2
      real ran1
      external ran1

      idum = time()
      idum = mod(idum,50000)

      Age = parms(1)
      ZZ  = parms(2)
      DM  = parms(3)
      EBV = parms(4)

c *** selecttion Padova's set of isochrones
c     choose the Z index
23       format(4f6.4,i3)
24       format(4f5.3,i2)

      izz= nint((ZZ - minZ)/deltaZ)+1
      TheoZZ= gridzz(izz) 
!      theoz1= gridzz(izz-1)
!      theoz2= gridzz(izz+1)

c     choose the Age index
      jage= nint((Age - minAge)/deltaAge)+1
      TheoAge= gridaa(jage)
!      theoaa1= gridaa(jage-1)
!      theoaa2= gridaa(jage+1)

!      write(*,23) Age, theoaa1, TheoAge, theoaa2, jage 
!      write(*,24) ZZ,theoz1, TheoZZ, theoz2, izz
!      stop

c      open(91,file='./check.dat')
      do k=1,400
         TheoT(k) = gridlogT(izz,jage,k) 
         TheoL(k) = gridlogL(izz,jage,k) 
         TheoBB(k) = gridB(izz,jage,k) 
         TheoVV(k) = gridV(izz,jage,k) 
         TheoII(k) = gridU(izz,jage,k) 
         TheoRR(k) = gridI(izz,jage,k) 
c         write(91,*) TheoBB(k)-TheoVV(k)+EBV, TheoVV(k)+DM, TheoRR(k)
      enddo
c      write(*,*)DM, EBV
c      close(91)
!      call randmassZ(izz,jage) 
      call starprobabilityZ(LK)

      return
      end       







