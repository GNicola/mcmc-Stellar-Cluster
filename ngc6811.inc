c *** File data
      character*7 cluster
      parameter(cluster=('ngc6811'))

c *** PD Isochrones
      integer nfiles, nage, maxisom
      real*8 deltaAge, deltaZ
      parameter(nfiles=37,nage=301,maxisom=500,deltaZ=0.00025,deltaAge = 0.0005) 

c *** Propose
      real*8 minAge, minZ, minDM, minAv
      real*8 maxAge, maxZ, maxDM, maxAv
      parameter(minAge=8.95d0, minZ=0.009d0, minDM=9.5d0, minAv=0.02d0,   
     &          maxAge=9.10d0, maxZ=0.018d0, maxDM=10.5d0, maxAv=0.18d0)

!!!!!!!!!!!!!!!! NOTE: active "if" in loadPDsetiso.f because it is necessary for ngc6811 !!!!!!!!!!!!
