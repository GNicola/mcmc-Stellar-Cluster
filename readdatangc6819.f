      subroutine readdata()
      implicit none
      include 'parmsZ.inc'
      integer Auner,Sanders
      integer k,num,j,i,ID,RAh,Ram,DEd,DEm,WIYN,CfA,Pr,mancanti
      real*8 RAs,DEs,vvv,bmv,RV,eRV,ei,ga
      real*8 vv,bv,vr,vi,v2,bv2,v3,bv3,v4,bv4,jj,jk,datas(maxisom)
      character*4 Class
      character*20 Co 

c *** open the file with data
      open(11,file='../input/ngc6819_RV.dat')
21    format(i6,2i3,f6.2,2i3,f5.1,f6.2,f5.2,2i3,f8.2,f6.2,f7.2,i3,a4,f9.3,a20)	
      open(13,file='../input/ngc6819.dat')
23    format(i6,f8.3,3f7.3,f8.3,f7.3,f8.3,f7.3,f8.3,f7.3,f8.3,f7.3,i7,i6)

      open(16,file='../input/blue_straggle.dat')
!      open(15,file='../output/observe1.check')
      open(12,file='../output/observe2.check')

c     skip first line
      read(11,*)
c     initialise index 
      k=0
      do while (.true.)
         read(11,21,END=100) ID,RAh,Ram,RAs,DEd,DEm,DEs,vvv,bmv,WIYN,CfA,RV,eRV,ei,Pr,Class,ga,Co   
         if(Pr .ge. 90 .and. Class .eq. ' SM ') then
           datas(k) = ID
           k = k+1
           write(*,*) Pr, Class, k
         endif
      enddo

100   continue
      num = k
      
      mancanti = 0
      k = 1 
      do j=1, 1305 
        read(13,23) ID,vv,bv,vr,vi,v2,bv2,v3,bv3,v4,bv4,jj,jk,Auner,Sanders
        do i=1, num
           if((datas(i).eq. ID).and.(vv .ne. 0.d0)  .and. (vv .lt. 16.5d0)) then
C            V mag
             ObsVV(k) = vv
C            B-V col
             ObsBV(k) = bv
C            V-I col
             ObsVI(k) = vi
C            V-R col
             ObsVR(k) = vr
             errBV(k)  = 0.1d0 
             errVV(k)  = 0.1d0
             errVI(k)  = 0.1d0
             errVR(k)  = 0.1d0
             write(12,*) i, k, ObsVV(k),ObsBV(k),ObsVI(k),ObsVR(k) 
             k = k+1 
           endif
           if(datas(i) .eq. ID .and. vv .ne. 0.d0) then
             mancanti = mancanti + 1
             write(*,*) mancanti
!             write(15,*) i, mancanti, v2, bv2, vi, vr
           endif
         enddo
      enddo
      nobs = k-1
      write(*,*) nobs
      close(11)
      close(12)
      close(13)
      close(15)
      
      return
      end
