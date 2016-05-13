	FUNCTION RAN1(IDUM)
	
	SAVE

!C  ***	Returns a uniform random deviate between 0.0 and 1.0. Set IDUM
!c	to any negative value to initialise or reinitialise the sequence

	DIMENSION R(97)
	integer m1,m2,m3
	real rm1,rm2
	PARAMETER(M1=259200, IA1 = 7141, IC1 = 54773, RM1 = 1./M1) 
	PARAMETER(M2=134456, IA2 = 8121, IC2 = 28411, RM2 = 1./M2) 
	PARAMETER(M3=243000, IA3 = 4561, IC3 = 51349)
	DATA IFF/0/
	
	
	

	IF(IDUM.LT.0 .OR. IFF.EQ.0)THEN
	  IFF = 1
	  IX1 = MOD(IC1-IDUM,M1)
	  IX1 = MOD(IA1*IX1+IC1,M1)
	  IX2 = MOD(IX1,M2)
	  IX1 = MOD(IA1*IX1+IC1,M1)
	  IX3 = MOD(IX1,M3)
	  DO 11 J=1, 97
	    IX1 = MOD(IA1*IX1+IC1,M1)
	    IX2 = MOD(IA2*IX2+IC2,M2)
	    R(J) = (FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
11	  CONTINUE
	  IDUM = 1
	END IF

	IX1 = MOD(IA1*IX1+IC1,M1)
	IX2 = MOD(IA2*IX2+IC2,M2)
	IX3 = MOD(IA3*IX3+IC3,M3)
	J = 1 + (97*IX3)/M3
	IF(J.GT.97 .OR. J.LT.1)PAUSE
	RAN1 = R(J)
	R(J) = (FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1

	RETURN
	END
