PRO gcirc,u,ra1,dc1,ra2,dc2,dis                         
;+
; NAME:
;     GCIRC
; PURPOSE:
;     Computes rigorous great circle arc distances.  
; EXPLANATION:
;     Input position can either be either radians, sexagesimal RA, Dec or
;     degrees.   All computations are double precision. 
;
; CALLING SEQUENCE:
;      GCIRC, U, RA1, DC1, RA2, DC2, DIS
;
; INPUTS:
;      U    -- integer = 0,1, or 2: Describes units of inputs and output:
;              0:  everything radians
;              1:  RAx in decimal hours, DCx in decimal
;                       degrees, DIS in arc seconds 
;              2:  RAx and DCx in degrees, DIS in arc seconds
;      RA1  -- Right ascension or longitude of point 1
;      DC1  -- Declination or latitude of point 1
;      RA2  -- Right ascension or longitude of point 2
;      DC2  -- Declination or latitude of point 2
;
; OUTPUTS:
;      DIS  -- Angular distance on the sky between points 1 and 2
;              See U above for units;  double precision  
;
; PROCEDURE:
;      "Haversine formula" see 
;      http://en.wikipedia.org/wiki/Great-circle_distance
;
; NOTES:
;       (1) If RA1,DC1 are scalars, and RA2,DC2 are vectors, then DIS is a
;       vector giving the distance of each element of RA2,DC2 to RA1,DC1.
;       Similarly, if RA1,DC1 are vectors, and RA2, DC2 are scalars, then DIS
;       is a vector giving the distance of each element of RA1, DC1 to 
;       RA2, DC2.    If both RA1,DC1 and RA2,DC2 are vectors then DIS is a
;       vector giving the distance of each element of RA1,DC1 to the 
;       corresponding element of RA2,DC2.    If the input vectors are not the 
;       same length, then excess elements of the longer ones will be ignored.
;
;       (2) The function SPHDIST provides an alternate method of computing
;        a spherical distance.
;
;       (3) The haversine formula can give rounding errors for antipodal
;       points.
;
; PROCEDURE CALLS:
;      None
;
;   MODIFICATION HISTORY:
;      Written in Fortran by R. Hill -- SASC Technologies -- January 3, 1986
;      Translated from FORTRAN to IDL, RSH, STX, 2/6/87
;      Vector arguments allowed    W. Landsman    April 1989
;      Prints result if last argument not given.  RSH, RSTX, 3 Apr. 1998
;      Remove ISARRAY(), V5.1 version        W. Landsman   August 2000
;      Added option U=2                      W. Landsman   October 2006
;      Use double precision for U=0 as advertised R. McMahon/W.L.  April 2007
;      Use havesine formula, which has less roundoff error in the 
;             milliarcsecond regime      W.L. Mar 2009
;-
 compile_opt idl2
 On_error,2                            ;Return to caller

 npar = N_params()
 IF (npar ne 6) and (npar ne 5) THEN BEGIN
   print,'Calling sequence:  GCIRC,U,RA1,DC1,RA2,DC2[,DIS]'
   print,'   U = 0  ==> Everything in radians'
   print, $
   '   U = 1  ==> RAx decimal hours, DCx decimal degrees, DIS arc sec'
   print,'   U = 2  ==> RAx, DCx decimal degrees, DIS arc sec'
   RETURN
 ENDIF


 d2r    = !DPI/180.0d0
 as2r   = !DPI/648000.0d0
 h2r    = !DPI/12.0d0

; Convert input to double precision radians
 CASE u OF
   0:  BEGIN
          rarad1 = double(ra1)
          rarad2 = double(ra2)
          dcrad1 = double(dc1)
          dcrad2 = double(dc2)
       END
   1:  BEGIN
          rarad1 = ra1*h2r
          rarad2 = ra2*h2r
          dcrad1 = dc1*d2r
          dcrad2 = dc2*d2r
       END
   2:  BEGIN  
          rarad1 = ra1*d2r
          rarad2 = ra2*d2r
          dcrad1 = dc1*d2r
          dcrad2 = dc2*d2r
        END
   ELSE:  MESSAGE, $
                'U must be 0 (radians), 1 ( hours, degrees) or 2 (degrees)'
 ENDCASE

 deldec2 = (dcrad2-dcrad1)/2.0d
 delra2 =  (rarad2-rarad1)/2.0d
 sindis = sqrt( sin(deldec2)*sin(deldec2) + $
	  cos(dcrad1)*cos(dcrad2)*sin(delra2)*sin(delra2) )
 dis = 2.0d*asin(sindis) 

 IF (u ne 0) THEN dis = dis/as2r

 IF (npar eq 5) && (N_elements(dis) EQ 1) THEN BEGIN
    IF (u ne 0) && (dis ge 0.1) && (dis le 1000)  $
       THEN fmt = '(F10.4)' $
       ELSE fmt = '(E15.8)'
    IF (u ne 0) THEN units = ' arcsec' ELSE units = ' radians'
    print,'Angular separation is ' + string(dis,format=fmt) + units
 ENDIF

 RETURN
 END

