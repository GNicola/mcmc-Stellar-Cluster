c *** File data
      character*7 cluster
      parameter(cluster=('ngc6819'))

c *** PD Isochrones
      integer nfiles, nage, maxisom
      real*8 deltaAge, deltaZ
      parameter(nfiles=29,nage=300,maxisom=500,deltaZ=0.00025, deltaAge=0.0002) 

c *** Propose
      real*8 minAge, minZ, minDM, minAv
      real*8 maxAge, maxZ, maxDM, maxAv
      parameter(minAge=9.37d0, minZ=0.022d0, minDM=12.d0, minAv=0.02d0,   
     &          maxAge=9.43d0, maxZ=0.029d0, maxDM=13.d0, maxAv=0.18d0)


!!!!!!!!!!!!!!!! NOTE: delete "if" in loadPDsetiso.f because it works just for ngc6811 !!!!!!!!!!!!
