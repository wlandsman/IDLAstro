PRO sunpos, jd, ra, dec, longmed, oblt, RADIAN = radian
;+
; NAME:
;       SUNPOS
; PURPOSE:
;       To compute the RA and Dec of the Sun at a given date.
;
; CALLING SEQUENCE:
;       SUNPOS, jd, ra, dec, [elong, obliquity, /RADIAN ]
; INPUTS:
;       jd    - The Julian date of the day (and time), scalar or vector
;               usually double precision
; OUTPUTS:
;       ra    - The right ascension of the sun at that date in DEGREES
;               double precision, same number of elements as jd
;       dec   - The declination of the sun at that date in DEGREES
;
; OPTIONAL OUTPUTS:
;       elong - Ecliptic longitude of the sun at that date in DEGREES.
;       obliquity - the obliquity of the ecliptic, in DEGREES
;
; OPTIONAL INPUT KEYWORD:
;       /RADIAN - If this keyword is set and non-zero, then all output variables 
;               are given in Radians rather than Degrees
;
; NOTES:
;       Patrick Wallace (Rutherford Appleton Laboratory, UK) has tested the
;       accuracy of a C adaptation of the sunpos.pro code and found the 
;       following results.   From 1900-2100 SUNPOS  gave 7.3 arcsec maximum 
;       error, 2.6 arcsec RMS.  Over the shorter interval 1950-2050 the figures
;       were 6.4 arcsec max, 2.2 arcsec RMS.  
;
;       The returned RA and Dec are in the given date's equinox.
;
;       Procedure was extensively revised in May 1996, and the new calling
;       sequence is incompatible with the old one.
; METHOD:
;       Uses a truncated version of Newcomb's Sun.    Adapted from the IDL
;       routine SUN_POS by CD Pike, which was adapted from a FORTRAN routine
;       by B. Emerson (RGO).
; EXAMPLE:
;       (1) Find the apparent RA and Dec of the Sun on May 1, 1982
;       
;       IDL> jdcnv, 1982, 5, 1,0 ,jd      ;Find Julian date jd = 2445090.5   
;       IDL> sunpos, jd, ra, dec
;       IDL> print,adstring(ra,dec,2)
;                02 31 32.61  +14 54 34.9
;
;       The Astronomical Almanac gives 02 31 32.58 +14 54 34.9 so the error
;               in SUNPOS for this case is < 0.5".      
;
;       (2) Find the apparent RA and Dec of the Sun for every day in 1997
;
;       IDL> jdcnv, 1997,1,1,0, jd                ;Julian date on Jan 1, 1997
;       IDL> sunpos, jd+ dindgen(365), ra, dec    ;RA and Dec for each day 
;
; MODIFICATION HISTORY:
;       Written by Michael R. Greason, STX, 28 October 1988.
;       Accept vector arguments, W. Landsman     April,1989
;       Eliminated negative right ascensions.  MRG, Hughes STX, 6 May 1992.
;       Rewritten using the 1993 Almanac.  Keywords added.  MRG, HSTX, 
;               10 February 1994.
;       Major rewrite, improved accuracy, always return values in degrees
;       W. Landsman  May, 1996 
;       Added /RADIAN keyword,    W. Landsman       August, 1997
;       Converted to IDL V5.0   W. Landsman   September 1997
;-
 On_error,2
 compile_opt idl2
;                       Check arguments.
 if N_params() LT 3 then begin 
     print, 'Syntax - SUNPOS, jd, ra, dec, [elong, obliquity, /RADIAN] '
     print, 'Inputs  -  jd (Julian date)'
     print, 'Outputs - Apparent RA and Dec, longitude, & obliquity'
     print, 'All angles in DEGREES unless /RADIAN is set'
     return
 endif

 dtor = !DPI/180.0d       ;(degrees to radian, double precision)

;  form time in Julian centuries from 1900.0

 t = (jd - 2415020.0d)/36525.0d0

;  form sun's mean longitude

 l = (279.696678d0+((36000.768925d0*t) mod 360.0d0))*3600.0d0

;  allow for ellipticity of the orbit (equation of centre)
;  using the Earth's mean anomaly ME

 me = 358.475844d0 + ((35999.049750D0*t) mod 360.0d0)
 ellcor  = (6910.1d0 - 17.2D0*t)*sin(me*dtor) + 72.3D0*sin(2.0D0*me*dtor)
 l = l + ellcor

; allow for the Venus perturbations using the mean anomaly of Venus MV

 mv = 212.603219d0 + ((58517.803875d0*t) mod 360.0d0) 
 vencorr = 4.8D0 * cos((299.1017d0 + mv - me)*dtor) + $
          5.5D0 * cos((148.3133d0 +  2.0D0 * mv  -  2.0D0 * me )*dtor) + $
          2.5D0 * cos((315.9433d0 +  2.0D0 * mv  -  3.0D0 * me )*dtor) + $
          1.6D0 * cos((345.2533d0 +  3.0D0 * mv  -  4.0D0 * me )*dtor) + $
          1.0D0 * cos((318.15d0   +  3.0D0 * mv  -  5.0D0 * me )*dtor)
l = l + vencorr

;  Allow for the Mars perturbations using the mean anomaly of Mars MM

 mm = 319.529425d0  +  (( 19139.858500d0 * t)  mod  360.0d0 )
 marscorr = 2.0d0 * cos((343.8883d0 -  2.0d0 * mm  +  2.0d0 * me)*dtor ) + $
            1.8D0 * cos((200.4017d0 -  2.0d0 * mm  + me) * dtor)
 l = l + marscorr

; Allow for the Jupiter perturbations using the mean anomaly of
; Jupiter MJ

 mj = 225.328328d0  +  (( 3034.6920239d0 * t)  mod  360.0d0 )
 jupcorr = 7.2d0 * cos(( 179.5317d0 - mj + me )*dtor) + $
          2.6d0 * cos((263.2167d0  -  MJ ) *dtor) + $
          2.7d0 * cos(( 87.1450d0  -  2.0d0 * mj  +  2.0D0 * me ) *dtor) + $
          1.6d0 * cos((109.4933d0  -  2.0d0 * mj  +  me ) *dtor)
 l = l + jupcorr

; Allow for the Moons perturbations using the mean elongation of
; the Moon from the Sun D

 d = 350.7376814d0  + (( 445267.11422d0 * t)  mod  360.0d0 )
 mooncorr  = 6.5d0 * sin(d*dtor)
 l = l + mooncorr

; Allow for long period terms

 longterm  = + 6.4d0 * sin(( 231.19d0  +  20.20d0 * t )*dtor)
 l  =    l + longterm
 l  =  ( l + 2592000.0d0)  mod  1296000.0d0 
 longmed = l/3600.0d0

; Allow for Aberration

 l  =  l - 20.5d0

; Allow for Nutation using the longitude of the Moons mean node OMEGA

 omega = 259.183275d0 - (( 1934.142008d0 * t ) mod 360.0d0 )
 l  =  l - 17.2d0 * sin(omega*dtor)

; Form the True Obliquity

 oblt  = 23.452294d0 - 0.0130125d0*t + (9.2d0*cos(omega*dtor))/3600.0d0

; Form Right Ascension and Declination

 l = l/3600.0d0
 ra  = atan( sin(l*dtor) * cos(oblt*dtor) , cos(l*dtor) )

 neg = where(ra LT 0.0d0, Nneg) 
 if Nneg GT 0 then ra[neg] = ra[neg] + 2.0d*!DPI

 dec = asin(sin(l*dtor) * sin(oblt*dtor))
 
 if keyword_set(RADIAN) then begin
        oblt = oblt*dtor 
        longmed = longmed*dtor
 endif else begin
        ra = ra/dtor
        dec = dec/dtor
 endelse
 end
