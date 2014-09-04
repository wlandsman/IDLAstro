PRO co_nutate, jd, ra, dec, d_ra, d_dec, eps=eps, d_psi=d_psi, d_eps=d_eps
;+
;  NAME:
;     CO_NUTATE
;  PURPOSE:
;     Calculate changes in RA and Dec due to nutation of the Earth's rotation
; EXPLANATION:
;     Calculates necessary changes to ra and dec due to
;     the nutation of the Earth's rotation axis, as described in Meeus, Chap 23.
;     Uses formulae from Astronomical Almanac, 1984, and does the calculations
;     in equatorial rectangular coordinates to avoid singularities at the
;     celestial poles.
;
; CALLING SEQUENCE:
;     CO_NUTATE, jd, ra, dec, d_ra, d_dec, [EPS=, D_PSI =, D_EPS = ]
; INPUTS
;    JD: Julian Date [scalar or vector]
;    RA, DEC : Arrays (or scalars) of the ra and dec's of interest
;
;   Note: if jd is a vector, ra and dec MUST be vectors of the same length.
;
; OUTPUTS:
;    d_ra, d_dec: the corrections to ra and dec due to nutation (must then
;                                be added to ra and dec to get corrected values).
; OPTIONAL OUTPUT KEYWORDS:
;    EPS: set this to a named variable that will contain the obliquity of the 
;             ecliptic.
;    D_PSI: set this to a named variable that will contain the nutation in the
;           longitude of the ecliptic
;    D_EPS: set this to a named variable that will contain the nutation in the
;                       obliquity of the ecliptic
; EXAMPLE:
;    (1) Example 23a in Meeus: On 2028 Nov 13.19 TD the mean position of Theta
;        Persei is 2h 46m 11.331s 49d 20' 54.54".    Determine the shift in 
;        position due to the Earth's nutation.
;    
;        IDL> jd = JULDAY(11,13,2028,.19*24)       ;Get Julian date
;        IDL> CO_NUTATE, jd,ten(2,46,11.331)*15.,ten(49,20,54.54),d_ra,d_dec    
;
;              ====> d_ra = 15.843"   d_dec = 6.217"
; PROCEDURES USED:
;    NUTATE 
; REVISION HISTORY:
;    Written  Chris O'Dell, 2002
;    Vector call to NUTATE   W. Landsman   June 2002
;    Fix when JD is 1 element vector, and RA,Dec are vectors WL  May 2013
;-

 if N_Params() LT 4  then begin
     print,'Syntax - CO_NUTATE, jd, ra, dec, d_ra, d_dec, '
     print,'   Output keywords:     [EPS=, D_PSI =, D_EPS = ]'
     return
 endif
 d2r = !dpi/180.
 d2as = !dpi/(180.d*3600.d)
 T = (jd -2451545.0)/36525.0 ; Julian centuries from J2000 of jd.

; must calculate obliquity of ecliptic
 nutate,jd,d_psi, d_eps 

 eps0 = 23.4392911*3600.d - 46.8150*T - 0.00059*T^2 + 0.001813*T^3
 eps = (eps0 + d_eps)/3600.*d2r ; true obliquity of the ecliptic in radians
 if N_elements(eps) EQ 1 then eps = eps[0]
 if N_elements(d_psi) Eq 1 then d_psi = d_psi[0]

;useful numbers
 ce = cos(eps)
 se = sin(eps)

; convert ra-dec to equatorial rectangular coordinates
 x = cos(ra*d2r) * cos(dec*d2r)
 y = sin(ra*d2r) * cos(dec*d2r)
 z = sin(dec*d2r)

; apply corrections to each rectangular coordinate
 x2 = x - (y*ce + z*se)*d_psi * d2as
 y2 = y + (x*ce*d_psi - z*d_eps) * d2as
 z2 = z + (x*se*d_psi + y*d_eps) * d2as

; convert back to equatorial spherical coordinates
 r = sqrt(x2^2 + y2^2 + z2^2)
 xyproj = sqrt(x2^2 + y2^2)

 ra2 = x2 * 0.d
 dec2= x2 * 0.d

 w1 = where( (xyproj eq 0) AND (z ne 0) )
 w2 = where(xyproj ne 0)

; Calculate Ra and Dec in RADIANS (later convert to DEGREES)
 if w1[0] ne -1 then begin
	; places where xyproj=0 (point at NCP or SCP)
	dec2[w1] = asin(z2[w1]/r[w1])
	ra2[w1] = 0.
 endif
 if w2[0] ne -1 then begin
	; places other than NCP or SCP
	ra2[w2] = atan(y2[w2],x2[w2])
	dec2[w2] = asin(z2[w2]/r[w2])
 endif

                  ; convert to DEGREES

 ra2 = ra2 /d2r
 dec2 = dec2 /d2r

 w = where(ra2 LT 0., Nw)
 if Nw GT 0 then ra2[w] = ra2[w] + 360.


; Return changes in ra and dec in arcseconds
 d_ra = (ra2 - ra) * 3600.
 d_dec = (dec2 - dec) * 3600.

END
