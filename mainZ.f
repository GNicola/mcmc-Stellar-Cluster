      program main_program
      implicit none
      include 'parmsZ.inc'
      integer i,j,met,idum,k,ktemp,kfail,ktried,kount,totfail
      real*8 parms0(nparms),LKold,LKnew,scfac
      logical reject

      
c *** initialise the random number generator
      idum = time()
      idum = mod(idum,50000)
      
c *** load observed quantities       
      call readdata()
      !do i=1,nobs
      !   write(*,*) i, ObsVV(i), ObsBV(i), ObsVI(i)
      !enddo
      !stop

c *** prepare the values of Z
      do i=1,nfiles
           gridzz(i) = minZ +(i-1)*deltaZ
      enddo
c *** prepare the values of age
      do j=1,nage
           gridaa(j) = minAge + (j-1)*deltaAge
      enddo

c *** load isochrones data
      call loadPDsetiso()  
      !do i=1,nfiles
      !   do j=1,nmet
      !      write(*,*) i, j, gridmass(i,j,1)
      !   enddo
      !enddo
      
c *** initial conditions for fitting parameters
      open(19,file='../'//cluster//'/init_parms.inp')
      do i=1,nparms
         read(19,*) parms(i), jumps(i)
      enddo
      close(19)

c *** output file: chi2, params
      open(12,file='../'//cluster//'/params_pdnew.check')
13    format(i8,9e17.9)
14    format(2i8,9e17.9)

c *** single components of chi2
      open(22,file='../'//cluster//'/likelihoodnew.check')
23    format(i8,e17.9)

c *** initialisations 
      scfac = 1.d0
      beta  = 1.d0
      k = 0
      call synthcmdZ(LKold)

!      chi2o = Probability
      write(22,23) 1, LKold
      write(12,13) 1, (parms(i), i=1,nparms)
c --- MAIN LOOP -------------------------------------------------------

      do ktemp = 2, maxtry
         k = k + 1

102      continue


c ***    save values of the parameters before the jump
         do i=1,nparms
            parms0(i) = parms(i)
         enddo
         kount = 0 

         beta = 1.d0
         kfail = 0
      
101      continue

c ***    propose new set of parameters
         call proposeZ(scfac,idum)

c ***    evolve with the new parameters and get a chi2
         call synthcmdZ(LKnew)

!         write(*,*) k , LKnew, LKold  

c ***    MH decision
         call methastZ(LKnew,LKold,reject)

         if (reject) then
c           failed step: revert to previous values
            do i=1,nparms
               parms(i) = parms0(i)
            enddo
            kfail   = kfail+1
	    totfail = totfail +kfail
!            beta = 1.d0/((kfail)**2.d0)
            goto 101
         else
c           successful step: go on!

            LKold = LKnew

            write(*,'(a13,i9,a21,e17.9,a16,i9,a8,f7.1)') 'Step number: ',ktemp, 
     &      '; current Likelihood:',
     &      LKnew,'; failed steps: ',kfail,'beta: ',beta 

            if(beta .ne. 1.d0) goto 102
         endif


c ***    write fitting parameters to file 

      write(12,13) k, (parms(i), i=1,nparms)
      write(22,23) k, LKnew
      flush(12)

c ***    now the new chi2 becomes the old chi2 before next step begins
!         LKold = LKnew

      enddo

c --- END MAIN LOOP ---------------------------------------------------


      close(12)
      close(22)
      stop 
      end






