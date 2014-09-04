PRO POSANG,u,ra1,dc1,ra2,dc2,angle
;+
; NAME:
;       POSANG
; PURPOSE:
;       Computes rigorous position angle of source 2 relative to source 1
;       
; EXPLANATION:
;       Computes the rigorous position angle of source 2 (with given RA, Dec) 
;       using source 1 (with given RA, Dec) as the center.
; 
; CALLING SEQUENCE:
;       POSANG, U, RA1, DC1, RA2, DC2, ANGLE
;
; INPUTS:
;       U    -- Describes units of inputs and output:
;               0:  everything radians
;               1:  RAx in decimal hours, DCx in decimal
;                       degrees, ANGLE in degrees
;       RA1  -- Right ascension of point 1
;       DC1  -- Declination of point 1
;       RA2  -- Right ascension of point 2
;       DC2  -- Declination of point 2
;
;   OUTPUTS:
;       ANGLE-- Angle of the great circle containing [ra2, dc2] from
;               the meridian containing [ra1, dc1], in the sense north
;               through east rotating about [ra1, dc1].  See U above 
;               for units.
;
;   PROCEDURE:
;       The "four-parts formula" from spherical trig (p. 12 of Smart's
;       Spherical Astronomy or p. 12 of Green' Spherical Astronomy).
;
;   EXAMPLE:
;       For the star 56 Per, the Hipparcos catalog gives a position of 
;       RA = 66.15593384, Dec = 33.94988843 for component A, and 
;       RA = 66.15646079, Dec =  33.96100069 for component B.   What is the
;       position angle of B relative to A?
;
;       IDL> RA1 = 66.15593384/15.d   & DC1 = 33.95988843
;       IDL> RA2 = 66.15646079/15.d   & DC2 = 33.96100069
;       IDL> posang,1,ra1,dc1,ra2,dc2, ang
;            will give the answer of ang = 21.4 degrees
;   NOTES:
;       (1) If RA1,DC1 are scalars, and RA2,DC2 are vectors, then ANGLE is a
;       vector giving the position angle between each element of RA2,DC2 and 
;       RA1,DC1.   Similarly, if RA1,DC1 are vectors, and RA2, DC2 are scalars,
;       then DIS is a vector giving the position angle of each element of RA1, 
;       DC1 and RA2, DC2.    If both RA1,DC1 and RA2,DC2 are vectors then ANGLE 
;       is a vector giving the position angle between each element of RA1,DC1 
;       and the corresponding element of RA2,DC2.    If then vectors are not the
;       same length, then excess elements of the longer one will be ignored.
;
;       (2) Note that POSANG is not commutative -- the position angle between
;        A and B is theta, then the position angle between B and A is 180+theta 
;   PROCEDURE CALLS:
;        ISARRAY()
;   HISTORY:
;       Modified from GCIRC, R. S. Hill, RSTX, 1 Apr. 1998
;       Use V6.0 notation W.L. Mar 2011
;
;-
 On_error,2                            ;Return to caller
 compile_opt idl2

 npar = N_params()
 IF (npar lt 5)  THEN BEGIN
   print,'Calling sequence:  POSANG,U,RA1,DC1,RA2,DC2,ANGLE'
   print,'   U = 0  ==> Everything in radians'
   print, $
   '   U = 1  ==> RAx decimal hours, DCx decimal degrees, ANGLE degrees'
   RETURN
ENDIF

scalar = (~isarray(ra1) ) && (~isarray(ra2) )
IF scalar THEN BEGIN
    IF (ra1 eq ra2) && (dc1 eq dc2) THEN BEGIN
       angle = 0.0d0
       IF npar eq 5 THEN $
           print,'Positions are equal:  ', ra1, dc1
       RETURN
    ENDIF
ENDIF

d2r    = !DPI/180.0d0
h2r    = !DPI/12.0d0

CASE u OF
   0:  BEGIN                    
          rarad1 = ra1
          rarad2 = ra2
          dcrad1 = dc1
          dcrad2 = dc2
       END
   1:  BEGIN                    
          rarad1 = ra1*h2r
          rarad2 = ra2*h2r
          dcrad1 = dc1*d2r
          dcrad2 = dc2*d2r
       END
   ELSE:  MESSAGE, $
                'U must be 0 for radians or 1 for hours, degrees, arcsec'
ENDCASE

radif  = rarad2-rarad1
angle  = atan(sin(radif),cos(dcrad1)*tan(dcrad2)-sin(dcrad1)*cos(radif))

IF (u ne 0) THEN angle = angle/d2r  

IF (npar eq 5) && (scalar) THEN BEGIN
    IF (u ne 0) && (abs(angle) ge 0.1) $
       THEN fmt = '(F14.8)' $
       ELSE fmt = '(E15.8)'
    units = (u ne 0) ? ' degrees' : ' radians'   
    print,'Position angle of target 2 about target 1 is ' $
        + string(angle,format=fmt) + units
ENDIF

RETURN
END                   
