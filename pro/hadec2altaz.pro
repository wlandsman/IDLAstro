PRO hadec2altaz, ha, dec, lat, alt, az, WS=WS

;+
;  NAME:
;     HADEC2ALTAZ
;  PURPOSE:
;      Converts Hour Angle and Declination to Horizon (alt-az) coordinates.
;  EXPLANATION:
;      Can deal with NCP/SCP singularity.    Intended mainly to be used by
;      program EQ2HOR
;
; CALLING SEQUENCE:
;      HADEC2ALTAZ, ha, dec, lat ,alt ,az [ /WS ]
;
; INPUTS
;     ha -  the local apparent hour angle, in DEGREES, scalar or vector
;     dec -  the local apparent declination, in DEGREES, scalar or vector
;     lat -  the local latitude, in DEGREES, scalar or vector
;
; OUTPUTS
;     alt - the local apparent altitude, in DEGREES.
;     az  - the local apparent azimuth, in DEGREES, all results in double
;           precision
; OPTIONAL KEYWORD INPUT:
;      /WS - Set this keyword for the output azimuth to be measured West from 
;            South.    The default is to measure azimuth East from North.
;
; EXAMPLE:
;     What were the apparent altitude and azimuth of the sun when it transited 
;     the local meridian at Pine Bluff Observatory (Lat=+43.07833 degrees) on 
;     April 21, 2002?   An object transits the local meridian at 0 hour angle.
;     Assume this will happen at roughly 1 PM local time (18:00 UTC).
;
;     IDL> jdcnv, 2002, 4, 21, 18., jd  ; get rough Julian date to determine 
;                                       ;Sun ra, dec.
;     IDL> sunpos, jd, ra, dec
;     IDL> hadec2altaz, 0., dec, 43.078333, alt, az
;
;       ===> Altitude alt = 58.90
;            Azimuth  az = 180.0

; REVISION HISTORY:
;      Written  Chris O'Dell Univ. of Wisconsin-Madison May 2002
;-

if N_params() LT 4 then begin
     print,'Syntax - HADEC2ALTAZ, ha, dec, lat ,alt ,az [ /WS ]'
     return
endif

d2r = !dpi/180.

sh = sin(ha*d2r) & ch = cos(ha*d2r)
sd = sin(dec*d2r) & cd = cos(dec*d2r)
sl = sin(lat*d2r) & cl = cos(lat*d2r)

x = - ch * cd * sl + sd * cl
y = - sh * cd
z = ch * cd * cl + sd * sl
r = sqrt(x^2 + y^2)
; now get Alt, Az

az = atan(y,x) /d2r
alt = atan(z,r) / d2r

; correct for negative AZ
w = where(az LT 0)
if w[0] ne -1 then az[w] = az[w] + 360.

; convert AZ to West from South, if desired
if keyword_set(WS) then az = (az + 180.) mod 360.


END