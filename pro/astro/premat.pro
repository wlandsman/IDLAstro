function premat, equinox1, equinox2, FK4 = FK4
;+
; NAME:
;       PREMAT
; PURPOSE:
;       Return the precession matrix needed to go from EQUINOX1 to EQUINOX2.  
; EXPLANTION:
;       This matrix is used by the procedures PRECESS and BARYVEL to precess 
;       astronomical coordinates
;
; CALLING SEQUENCE:
;       matrix = PREMAT( equinox1, equinox2, [ /FK4 ] )
;
; INPUTS:
;       EQUINOX1 - Original equinox of coordinates, numeric scalar.  
;       EQUINOX2 - Equinox of precessed coordinates.
;
; OUTPUT:
;      matrix - double precision 3 x 3 precession matrix, used to precess
;               equatorial rectangular coordinates
;
; OPTIONAL INPUT KEYWORDS:
;       /FK4   - If this keyword is set, the FK4 (B1950.0) system precession
;               angles are used to compute the precession matrix.   The 
;               default is to use FK5 (J2000.0) precession angles
;
; EXAMPLES:
;       Return the precession matrix from 1950.0 to 1975.0 in the FK4 system
;
;       IDL> matrix = PREMAT( 1950.0, 1975.0, /FK4)
;
; PROCEDURE:
;       FK4 constants from "Computational Spherical Astronomy" by Taff (1983), 
;       p. 24. (FK4). FK5 constants from "Astronomical Almanac Explanatory
;       Supplement 1992, page 104 Table 3.211.1.
;
; REVISION HISTORY
;       Written, Wayne Landsman, HSTX Corporation, June 1994
;       Converted to IDL V5.0   W. Landsman   September 1997
;-    
  On_error,2                                           ;Return to caller

  npar = N_params()

   if ( npar LT 2 ) then begin 

     print,'Syntax - PREMAT, equinox1, equinox2, /FK4]'
     return,-1 

  endif 

  deg_to_rad = !DPI/180.0d
  sec_to_rad = deg_to_rad/3600.d0

   t = 0.001d0*( equinox2 - equinox1)

 if ~keyword_set( FK4 )  then begin  
           st = 0.001d0*( equinox1 - 2000.d0)
;  Compute 3 rotation angles
           A = sec_to_rad * T * (23062.181D0 + ST*(139.656D0 +0.0139D0*ST) $
            + T*(30.188D0 - 0.344D0*ST+17.998D0*T))

           B = sec_to_rad * T * T * (79.280D0 + 0.410D0*ST + 0.205D0*T) + A

        C = sec_to_rad * T * (20043.109D0 - ST*(85.33D0 + 0.217D0*ST) $
              + T*(-42.665D0 - 0.217D0*ST -41.833D0*T))

 endif else begin  

           st = 0.001d0*( equinox1 - 1900.d0)
;  Compute 3 rotation angles

           A = sec_to_rad * T * (23042.53D0 + ST*(139.75D0 +0.06D0*ST) $
            + T*(30.23D0 - 0.27D0*ST+18.0D0*T))

           B = sec_to_rad * T * T * (79.27D0 + 0.66D0*ST + 0.32D0*T) + A

           C = sec_to_rad * T * (20046.85D0 - ST*(85.33D0 + 0.37D0*ST) $
              + T*(-42.67D0 - 0.37D0*ST -41.8D0*T))

 endelse  

  sina = sin(a) &  sinb = sin(b)  & sinc = sin(c)
  cosa = cos(a) &  cosb = cos(b)  & cosc = cos(c)

  r = dblarr(3,3)
  r[0,0] = [ cosa*cosb*cosc-sina*sinb, sina*cosb+cosa*sinb*cosc,  cosa*sinc]
  r[0,1] = [-cosa*sinb-sina*cosb*cosc, cosa*cosb-sina*sinb*cosc, -sina*sinc]
  r[0,2] = [-cosb*sinc, -sinb*sinc, cosc]

  return,r
  end
