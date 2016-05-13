	FUNCTION GASDEV(IDUM)
	
!C  ***	Returns a normally distributed deviate with zero mean and unit
!c	variance, using RAN1(IDUM) as the source of uniform deviates.

	SAVE

	DATA ISET/0/
	
	IF(ISET.EQ.0)THEN
1	  V1 = 2.*RAN1(IDUM)-1.
	  V2 = 2.*RAN1(IDUM)-1.
	  R = V1*V1 + V2*V2
	  IF(R.GE.1.)GO TO 1
	  FAC = SQRT(-2.*LOG(R)/R)
	  GSET = V1*FAC
	  GASDEV = V2*FAC
	  ISET = 1
	ELSE
	  GASDEV = GSET
	  ISET = 0
	END IF

	RETURN
	END
