      subroutine readdata()
      implicit none
      include 'parmsZ.inc'
      integer k,ID,a,b,c,d
      real*8 gg,rr,ii,VV,BV,ev,ebv,ub,eub,vi,evi,vr,evr,xx,yy
 
      open(12,file='../ngc6811/checknew.dat') 
c *** open the file with data
      open(11,file='../input/ngc6811_new.dat')
c     skip first line
      read(11,*)
c     initialise index 
      k=1
      do while (.true.)
         read(11,'(i7,4x,f6.3,4x,f6.3,4x,f6.3)',END=200) ID,gg,rr,ii   
C         V mag
          ObsVV(k) = gg - 0.528d0 * (gg - rr) - 0.091d0
C         B-V col
          ObsBV(k) = 1.024d0 * (gg - rr) + 0.170d0
C         V-I col
         if(ii .ne. 0.d0) then 
             ObsVI(k) = 2.276d0 * (rr - ii) + 0.419d0
         else 
              ObsVI(k) = 0.d0
         endif
         errBV(k)  = 0.1d0 
         errVV(k)  = 0.1d0    
         errVI(k)  = 0.1d0  

         write(12,*) k, ObsVV(k),ObsBV(k),ObsVI(k) 

         k = k+1

      enddo

200   continue

      nobs = k-1

      close(11)
      return
      end
