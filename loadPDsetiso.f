      subroutine loadpdsetiso()
      implicit none
      include 'parmsZ.inc'  
      integer j,k,i,l, ip
      real*8 z1, t1, m1, lg, mb, imf, uu, bb, vv, rr, ii, jj, hh, kk 
      real*8 mm, ll, tt
      character*4 label
      character*12 filename
      character line
      logical iflag 

c *** the format for the output from PD database
c *** UPDATE: no format needed, the file is tab-spaced! :) 


!      write(*,*) 'Loading grid files:'    
      do i=1,nfiles
         l = ((i-1)*deltaZ + minZ)*100000 
         write(label,'(i4)') l
         filename = label//'.dat'
!         write(*,*) filename,l 

c ***    initialisation
         k = 1
         j = 0
         open(18,file='../input/'//filename)
23       format(f6.4,f7.4,f11.8,f7.4,f8.4,2f7.4,9f7.3,f11.8,i1)


c ***    infinite loop, but we will use 'end=999' to exit
         do while (.true.)             
c           read the line into a string first
            read(18,'(a)',end=999) line
c           next line does not start with '#': go back and read it properly
            if ( line .ne. '#') then
               backspace(18) 
               read(18,*)z1, t1, mm, m1, ll, tt, lg, mb, uu, bb, vv, rr,
     &                   ii, jj, hh, kk, imf, ip
c              if the mass is 0.1, we are starting a new block (i.e. a new Age): 
c              increment j (Age), reset k (mass)       
               if (mm .eq. 0.1d0) then
c                  
                  iflag = .true.
c
                  k=1
                  j=j+1
               endif
            if(ip.eq. 4 .and. iflag) then        
               iflag = .false.
               goto 231
            endif 

c           put mm, ll, tt in the arrays mass, logL, logT
c ***       indices: i->melallicity; j->Age; k->mass       
            if(ip .ge. 4) goto 231            !------------------------>IT MUST BE ACTIVE JUST WHEN YOU COMPILE mcmc6811_pd.x
            gridmass(i,j,k) = mm
            gridlogT(i,j,k) = tt
            gridlogL(i,j,k) = ll
            gridU(i,j,k) = uu
            gridB(i,j,k) = bb
            gridV(i,j,k) = vv
            gridR(i,j,k) = rr
            gridI(i,j,k) = ii
   !         if(ip .eq. 7) then
   !            gridph(i,j,k) = 5
   !         else
            gridph(i,j,k)= ip
   !        endif
   !        if(ip .eq. 8)  gridph(i,j,k) = 6
            
c              increment k
               k = k+1
            endif
            mnum(i,j) = k-1

231         continue            

         enddo

999      continue

         write(*,*) j
      
         if (j.ne.nage) write(*,*) 'Warning: read ',j,
     &                  'age in file ',filename

         close(18)
     
      enddo
    
      return
      end
