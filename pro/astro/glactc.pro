pro glactc,ra,dec,year,gl,gb,j, degree=degree, fk4 = fk4, $
   SuperGalactic = superGalactic
;+
; NAME:  
;       GLACTC
; PURPOSE:
;        Convert between celestial and Galactic (or Supergalactic) coordinates.
; EXPLANATION:
;       Program to convert right ascension (ra) and declination (dec) to
;       Galactic longitude (gl) and latitude (gb) (j=1) or vice versa (j=2).
;
; CALLING SEQUENCE: 
;       GLACTC, ra, dec, year, gl, gb, j, [ /DEGREE, /FK4, /SuperGalactic ]
;
; INPUT PARAMETERS: 
;       year     equinox of ra and dec, scalar       (input)
;       j        direction of conversion     (input)
;               1:  ra,dec --> gl,gb
;               2:  gl,gb  --> ra,dec
;
; INPUTS OR OUTPUT PARAMETERS: ( depending on argument J )
;       ra       Right ascension, hours (or degrees if /DEGREES is set), 
;                         scalar or vector
;       dec      Declination, degrees,scalar or vector
;       gl       Galactic longitude, degrees, scalar or vector
;       gb       Galactic latitude, degrees, scalar or vector
;
;       All results forced double precision floating.
;
; OPTIONAL INPUT KEYWORD PARAMETERS:
;       /DEGREE - If set, then the RA parameter (both input and output) is 
;                given in degrees rather than hours. 
;       /FK4 - If set, then the celestial (RA, Dec) coordinates are assumed
;              to be input/output in the FK4 system.    By default,  coordinates
;              are assumed to be in the FK5 system.    For B1950 coordinates,
;              set the /FK4 keyword *and* set the year to 1950.
;       /SuperGalactic - If set, the GLACTC returns SuperGalactic coordinates
;              as defined by deVaucouleurs et al. (1976) to account for the 
;              local supercluster. The North pole in SuperGalactic coordinates 
;              has Galactic coordinates l = 47.47, b = 6.32, and the origin is 
;              at Galactic coordinates l = 137.37, b= 0 
;              
; EXAMPLES:
;       Find the Galactic coordinates of Altair (RA (J2000): 19 50 47 
;       Dec (J2000): 08 52 06)
;
;       IDL> glactc, ten(19,50,47),ten(8,52,6),2000,gl,gb,1
;       ==> gl = 47.74, gb = -8.91
;
; PROCEDURE CALLS:
;       BPRECESS, JPRECESS, PRECESS
; HISTORY: 
;       FORTRAN subroutine by T. A. Nagy, 21-MAR-78.
;       Conversion to IDL, R. S. Hill, STX, 19-OCT-87.
;       Modified to handle vector input, E. P. Smith, GSFC, 14-OCT-94
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Added DEGREE keyword, C. Markwardt, Nov 1999
;       Major rewrite, default now FK5 coordinates, added /FK4 keyword
;       use external precession routines    W. Landsman   April 2002
;       Add /Supergalactic keyword W. Landsman  September 2002
;       Fix major bug when year not 2000 and /FK4 not set W. Landsman July 2003
;-
 On_error,2
 compile_opt idl2

if N_params() lt 6 then begin
     print,'Syntax -  glactc, ra, dec, year, gl, gb, j, [/DEGREE, /FK4]'
     print,'j = 1: ra,dec --> gl,gb   j = 2:  gl,gb -->ra,dec'
     return
endif
radeg = 180.0d/!DPI
;
; Galactic pole at ra 12 hrs 49 mins, dec 27.4 deg, equinox B1950.0
; position angle from Galactic center to equatorial pole = 123 degs.

 if keyword_set(SuperGalactic) then begin
    rapol = 283.18940711d/15.0d & decpol = 15.64407736d
    dlon =  26.73153707 
 endif else begin 
   rapol = 12.0d0 + 49.0d0/60.0d0 
   decpol = 27.4d0
   dlon = 123.0d0
 endelse
   sdp = sin(decpol/radeg)
   cdp = sqrt(1.0d0-sdp*sdp)
   radhrs=radeg/15.0d0

 ;
; Branch to required type of conversion.   Convert coordinates to B1950 as 
; necessary
case j of                   
    1:  begin
        if ~keyword_set(degree) then  ras = ra*15.0d else ras =ra
        decs = dec
        if ~keyword_set(fk4) then begin
                 if year NE 2000 then precess,ras,decs,year,2000
                 bprecess,ras,decs,ra2,dec2
                 ras = ra2
                 decs = dec2
       endif else if year NE 1950 then precess,ras,decs,year,1950,/fk4
        ras = ras/radeg - rapol/radhrs 
        sdec = sin(decs/radeg)
        cdec = sqrt(1.0d0-sdec*sdec)
        sgb = sdec*sdp + cdec*cdp*cos(ras)
        gb = radeg * asin(sgb)
        cgb = sqrt(1.0d0-sgb*sgb)
        sine = cdec * sin(ras) / cgb
        cose = (sdec-sdp*sgb) / (cdp*cgb)
        gl = dlon - radeg*atan(sine,cose)
        ltzero=where(gl lt 0.0, Nltzero)
        if Nltzero ge 1 then gl[ltzero]=gl[ltzero]+360.0d0
        return
        end
    2:  begin
        sgb = sin(gb/radeg)
        cgb = sqrt(1.0d0-sgb*sgb)
        sdec = sgb*sdp + cgb*cdp*cos((dlon-gl)/radeg)
        dec = radeg * asin(sdec)
        cdec = sqrt(1.0d0-sdec*sdec)
        sinf = cgb * sin((dlon-gl)/radeg) / cdec
        cosf = (sgb-sdp*sdec) / (cdp*cdec)
        ra = rapol + radhrs*atan(sinf,cosf)
        ra = ra*15.0d
         if ~keyword_set(fk4) then begin
                    ras = ra & decs = dec
                  jprecess,ras,decs,ra,dec
                  if year NE 2000 then precess,ra,dec,2000,year
        endif else if year NE 1950 then begin
                  precess,ra,dec,1950,year,/fk4
        endif 
                   
        gt36 = where(ra gt 360.0, Ngt36)
        if Ngt36 ge 1 then ra[gt36] = ra[gt36] - 360.0d0
        if ~keyword_set(degree) then      ra = ra / 15.0D0

   
        return
        end
endcase
end
