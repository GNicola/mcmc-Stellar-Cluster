c *** Decide which cluster yoyu want to study including its input file
   !   include 'ngc6819.inc'
      include 'ngc6811.inc'

c *** OBSERV. DATA
      integer maxobs, nobs
      parameter(maxobs=2000)       
      real*8 ObsUB(maxobs),ObsBV(maxobs),ObsVV(maxobs),ObsVI(maxobs),ObsVR(maxobs)
      real*8 errUB(maxobs),errBV(maxobs),errVV(maxobs),errVI(maxobs),errVR(maxobs) 
      common /obs_data/ ObsUB, ObsBV, ObsVV, ObsVR, ObsVI, errUB, errBV, errVV, errVI, errVR, nobs

c *** PD Isochrones
      integer mnum(nfiles,nage)
      integer gridph(nfiles,nage,maxisom)  
      real*8 gridaa(nage), gridzz(nfiles)
      real*8 gridmass(nfiles,nage,maxisom), gridlogT(nfiles,nage,maxisom), gridlogL(nfiles,nage,maxisom)
      real*8 gridU(nfiles,nage,maxisom), gridB(nfiles,nage,maxisom), gridV(nfiles,nage,maxisom)
      real*8 gridI(nfiles,nage,maxisom), gridR(nfiles,nage,maxisom) 
      common /grid_pd_data/ gridaa, gridzz, gridmass, gridlogT, gridlogL, gridU, gridB, gridV, gridI, gridR, gridph, mnum

c *** Random masses
      integer nrnd
      parameter(nrnd=1000)
      real*8 rmass(nrnd)
      real*8 TheoT(nrnd),TheoL(nrnd),TheoUU(nrnd),TheoBB(nrnd),TheoVV(nrnd),TheoII(nrnd),TheoRR(nrnd)
      common /interp_iso/ rmass, TheoT, TheoL, TheoUU, TheoBB, TheoVV, TheoII, TheoRR

c *** MCMC parameters
      integer nparms, maxtry
      parameter(nparms=4,maxtry=30000)
      real*8 parms(nparms),jumps(nparms) 
      common /parameters/ parms, jumps
      real*8 beta
      common /tempering/ beta
