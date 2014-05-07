PRO altaz2hadec, alt, az, lat, ha, dec
;+
;  NAME:
;    ALTAZ2HADEC
; PURPOSE:
;    Convert Horizon (Alt-Az) coordinates to Hour Angle and Declination.
; EXPLANATION::
;    Can deal with the NCP singularity.    Intended mainly to be used by
;    program hor2eq.pro
; CALLING SEQUENCE:
;   ALTAZ2HADEC, alt, az, lat, ha, dec
;
; INPUTS
;   alt - the local apparent altitude, in DEGREES, scalar or vector
;   az  - the local apparent azimuth, in DEGREES, scalar or vector,
;         measured EAST of NORTH!!!  If you have measured azimuth west-of-south
;        (like the book MEEUS does), convert it to east of north via:
;                       az = (az + 180) mod 360
;
;   lat -  the local geodetic latitude, in DEGREES, scalar or vector.
;
; OUTPUTS
;   ha  -  the local apparent hour angle, in DEGREES.  The hour angle is the 
;          time that right ascension of 0 hours crosses the local meridian.  
;          It is unambiguously defined.
;   dec -  the local apparent declination, in DEGREES.
;
; EXAMPLE:
;     Arcturus is observed at an apparent altitude of 59d,05m,10s and an 
;     azimuth (measured east of north) of 133d,18m,29s while at the 
;     latitude of +43.07833 degrees.
;     What are the local hour angle and declination of this object?
;
;     IDL> altaz2hadec, ten(59,05,10), ten(133,18,29), 43.07833, ha, dec
;     ===> Hour angle ha = 336.683 degrees
;          Declination, dec = 19.1824 degrees
;
;       The widely available XEPHEM code gets:
;                 Hour Angle = 336.683
;                 Declination = 19.1824
;
; REVISION HISTORY:
;    Written  Chris O'Dell Univ. of Wisconsin-Madison May 2002
;-

 if N_params() LT 4 then begin
     print,'Syntax - ALTAZ2HADEC, alt, az, lat, ha, dec'
     return
 endif
 d2r = !dpi/180.0d
 alt_r  = alt*d2r
 az_r = az*d2r
 lat_r = lat*d2r

;******************************************************************************
; find local HOUR ANGLE (in degrees, from 0. to 360.)
 ha = atan( -sin(az_r)*cos(alt_r), $
           -cos(az_r)*sin(lat_r)*cos(alt_r)+sin(alt_r)*cos(lat_r))
 ha = ha / d2r
 w = where(ha LT 0.)
 if w[0] ne -1 then ha[w] = ha[w] + 360.
 ha = ha mod 360.

; Find declination (positive if north of Celestial Equator, negative if south)
 sindec = sin(lat_r)*sin(alt_r) + cos(lat_r)*cos(alt_r)*cos(az_r)
 dec = asin(sindec)/d2r  ; convert dec to degrees


END
