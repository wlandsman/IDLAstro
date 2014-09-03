pro PRECESS_CD, cd, epoch1, epoch2, crval_old, crval_new, FK4 = FK4     
;+
; NAME:
;       PRECESS_CD
;
; PURPOSE:
;       Precess the CD (coordinate description) matrix from a FITS header 
; EXPLANATION:
;       The CD matrix is precessed from EPOCH1 to EPOCH2.  Called by HPRECESS
;
; CALLING SEQUENCE:
;       PRECESS_CD, cd, epoch1, epoch2, crval_old, crval_new, [/FK4]  
;
; INPUTS/OUTPUT:
;       CD - 2 x 2 CD (coordinate description) matrix in any units
;               (degrees or radians).  CD will altered on output to contain 
;               precessed values in the same units.    On output CD will always
;               be double precision no matter how input.
;
; INPUTS:
;       EPOCH1 - Original equinox of coordinates, scalar (e.g. 1950.0).  
;       EPOCH2 - Equinox of precessed coordinates, scalar (e.g. 2000.0)
;       CRVAL_OLD - 2 element vector containing RA and DEC in DEGREES
;               of the reference pixel in the original equinox
;       CRVAL_NEW - 2 elements vector giving CRVAL in the new equinox 
;
; INPUT KEYWORD:
;       /FK4 - If this keyword is set, then the precession constants are taken
;             in the FK4 reference frame.   The default is the FK5 frame.
;
; RESTRICTIONS:
;       PRECESS_CD should not be used more than 2.5 centuries from the
;       year 1900.      
;
; PROCEDURE:
;       Adapted from the STSDAS program FMATPREC.  Precession changes the
;       location of the north pole, and thus changes the rotation of
;       an image from north up.  This is reflected in the precession of the
;       CD matrix.   This is usually a very small change. 
;
; PROCEDURE CALLS:
;       PRECESS
;
; REVISION HISTORY:
;       Written, Wayne Landsman, ST Systems  February 1988
;       Fixed sign error in computation of SINRA     March 1992
;       Added /FK4 keyword                           Feb 1994
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Use B/Jprecess for conversion between 1950 and 2000 W.L. Aug 2009
;-    
  On_error,2
  compile_opt idl2

  if N_params() LT 3 then begin    
      print,'Syntax: precess_cd, cd, epoch1, epoch2, crval_old, crval_new
      return
   endif 

   deg_to_rad = !DPI/180.0D
   crvalold = crval_old * deg_to_rad
   crvalnew = crval_new * deg_to_rad

   sec_to_rad = deg_to_rad/3600.d0
   t = 0.001d0 * (epoch2-epoch1)

;   Compute C - inclination of the mean equator in the new equinox relative
;   to that of the old equinox

   if keyword_set(FK4) then begin

        st = 0.001d0 * (epoch1-1900.d0)

        C = sec_to_rad * T * ( 20046.85D0 - ST*(85.33D0 + 0.37D0*ST) $
                + T*(-42.67D0 - 0.37D0*ST -41.8D0*T))

   endif else begin

        st = 0.001d0*( epoch1 - 2000.d0)

        C = sec_to_rad * T * (20043.109D0 - ST*(85.33D0 + 0.217D0*ST) $
                + T*(-42.665D0 - 0.217D0*ST -41.833D0*T))
   endelse

; Get RA of old pole in new coordinates

  pole_ra = 0. & pole_dec = 90.d       ;Coordinates of old pole (RA is arbitrary)
  if (epoch1 EQ 2000) && (epoch2 EQ 1950) then begin
    bprecess, pole_ra, pole_dec,pra,pdec
    pole_ra =  pra
  endif else if (epoch1 EQ 1950) and (epoch2 EQ 2000) then begin
     bprecess, pole_ra, pole_dec,pra,pdec
     pole_ra =  pra    
  endif else precess, pole_ra, pole_dec, epoch1, epoch2, FK4 = FK4    

  sind1 = sin( crvalold[1] ) &  sind2 = sin( crvalnew[1] )
  cosd1 = cos( crvalold[1] ) &  cosd2 = cos( crvalnew[1] )
  sinra = sin( crvalnew[0] - pole_ra*deg_to_rad)    ;Fixed sign error Mar-92
  cosfi = (cos(c) - sind1*sind2)/( cosd1*cosd2 )
  sinfi = ( abs(sin(c) ) * sinra) / cosd1 
  r = [ [cosfi, sinfi], [-sinfi, cosfi] ]

  cd = r # cd            ;Rotate to new north pole

  return
  end
