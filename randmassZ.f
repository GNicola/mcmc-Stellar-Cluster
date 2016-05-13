      subroutine randmassZ(izz,jage)
      implicit none
      include 'parmsZ.inc'
      integer jage, izz, niso
      integer i, j, k, seed, ir,eta
      real*8 Age, DM, EBV, ZZ
      real*8 lLiso(maxisom), lTiso(maxisom), miso(maxisom),lTiso2(maxisom), lLiso2(maxisom) 
      real*8 BBiso(maxisom), VViso(maxisom), IIiso(maxisom), RRiso(maxisom)
      real*8 RRiso2(maxisom), BBiso2(maxisom), VViso2(maxisom), IIiso2(maxisom)
      character*5 nome
      character*19 namenewiso     
      real*8 lintrp
      real ran1
      external lintrp, ran1

      seed = time()
      seed = mod(seed,50000)

      Age = 10.d0**parms(1)
      ZZ  = parms(2)
      
      niso= mnum(izz,jage)
      miso(:) = gridmass(izz,jage,:)
      lTiso(:) = gridlogT(izz,jage,:)
      lLiso(:) = gridlogL(izz,jage,:)
      BBiso(:) = gridB(izz,jage,:)
      VViso(:) = gridV(izz,jage,:)
      IIiso(:) = gridI(izz,jage,:)

c     generate random masses
      do k=1,nrnd

         rmass(k) = 1.2d0 + (miso(niso)-1.2d0)*dble(ran1(seed))
!	 write(*,*) rmass(k),seed,dble(ran1(seed)), miso(niso),niso
         do i=2, niso                                                      ! I generete 1000 random masses and their   
            if (rmass(k) .gt. miso(i-1) .and. rmass(k) .le. miso(i) ) then
               ir = i
               goto 222
            endif
         enddo   
 
222      continue
!	 write(*,*) miso(ir-1),lTiso(ir-1),miso(ir),lTiso(ir),rmass(k)
         TheoT(k) = lintrp(miso(ir-1),(ir-1),miso(ir),lTiso(ir),rmass(k)) 
         TheoL(k) = lintrp(miso(ir-1),lLiso(ir-1),miso(ir),lLiso(ir),rmass(k)) 
         TheoBB(k) = lintrp(miso(ir-1),BBiso(ir-1),miso(ir),BBiso(ir),rmass(k)) 
         TheoVV(k) = lintrp(miso(ir-1),VViso(ir-1),miso(ir),VViso(ir),rmass(k)) 
         TheoII(k) = lintrp(miso(ir-1),IIiso(ir-1),miso(ir),IIiso(ir),rmass(k)) 
         TheoRR(k) = lintrp(miso(ir-1),RRiso(ir-1),miso(ir),RRiso(ir),rmass(k)) 


c        interpolate
!         call splint(miso,lTiso,lTiso2,niso,rmass(k),TheoT(k))
!         call splint(miso,lLiso,lLiso2,niso,rmass(k),TheoL(k))
!         call splint(miso,BBiso,BBiso2,niso,rmass(k),TheoBB(k))
!         call splint(miso,VViso,VViso2,niso,rmass(k),TheoVV(k))
!         call splint(miso,IIiso,IIiso2,niso,rmass(k),TheoII(k))
      enddo

c *** debugging output
      open(18,file='test_interp1_pd.check')
      do k=1,maxisom
         write(18,'(12e17.9)') gridmass(izz-1,jage-1,k),gridB(izz-1,jage-1,k),gridV(izz-1,jage-1,k),
     &                         gridmass(izz-1,jage,k),gridB(izz-1,jage,k),gridV(izz-1,jage,k),
     &                         gridmass(izz,jage-1,k),gridB(izz,jage-1,k),gridV(izz,jage-1,k),
     &                         gridmass(izz,jage,k),gridB(izz,jage,k),gridV(izz,jage,k)
      enddo
      close(18)
      open(19,file='test_interp2_pd.check')  
      do k=1,niso              
      DM  = parms(3)
      EBV = parms(4)
         write(19,'(3e17.9)') miso(k),BBiso(k)-VViso(k),VViso(k)
        ! BBVV(k) = BBiso(k) - VViso(k)
      enddo
      close(19)

c *** 'official' output
      DM  = parms(3)
      EBV = parms(4)
 
c ******  CHECK  ******************************************************************************     
    !  eta = int(Age/(10.d0**(5.d0)))
    !  write(nome,'(i5)') eta

    !  namenewiso = nome//'.check'
    !  open(20,file='../output/check/interMassnew'//namenewiso)
    !  do k=1,nrnd
    !     write(20,'(5e17.9)') rmass(k),TheoT(k),TheoL(k),TheoBB(k)-TheoVV(k)+EBV,TheoVV(k)+DM
    !  enddo 
    !  close(20)
c *********************************************************************************************
      

      return
      end
