;+
; NAME:
;   HOR2EQ
;
; PURPOSE:
;    Converts local horizon coords (alt-az) of something to equatorial (ra-dec).
;
; EXPLANATION:
;     This is a nice code to calculate equatorial (ra,dec) coordinates from
;     horizon (alt,az) coords.    It is typically accurate to about 1 arcsecond
;     or better (I have checked the output against the publicly available XEPHEM
;     software). It performs precession, nutation, aberration, and refraction
;     corrections.  The perhaps best thing about it is that it can take arrays
;     as inputs, in all variables and keywords EXCEPT Lat, lon, and Altitude
;    (the code assumes these aren't changing), and uses vector arithmetic in
;     every calculation except when calculating the precession matrices.
;
; CALLING SEQUENCE:
;
;    HOR2EQ, alt, az, jd, ra, dec, [ha, LAT= , LON= , /WS, OBSNAME= , $
;                       /B1950 , PRECESS_= 0, NUTATE_= 0, REFRACT_= 0, $
;                       ABERRATION_= 0, ALTITUDE= , /VERBOSE, _EXTRA= ]
;
;
; INPUT VARIABLES
;       alt  : altitude (in degrees) [scalar or vector]
;       az   : azimuth angle (in degrees, measured EAST from NORTH, but see
;              keyword WS below.) [scalar or vector]
;       JD   : Julian Date [scalar or vector], double precision

;       Note: if RA and DEC are arrays, then alt and az will also be arrays.
;             If RA and DEC are arrays, JD may be a scalar OR an array of
;              the same dimensionality.
;
; OPTIONAL INPUT KEYWORDS:
;       lat   : north geodetic latitude of location in degrees
;       lon   : EAST longitude of location in degrees
;               (Specify west longitude with a negative sign.)
;       /WS   : Set this to get the azimuth measured westward from south
;               (not East of North).
;       obsname   : Set this to a valid observatory name to be used by the
;               astrolib OBSERVATORY procedure, which will return the latitude
;               and longitude to be used by this program.
;       /B1950  : Set this if your ra and dec are specified in B1950,
;               FK4 coordinates (instead of J2000, FK5)
;       precess_ : Set this to 1 to force precession [default], 0 for no
;                 precession.
;       nutate_  : Set this to 1 to force nutation [default], 0 for no nutation.
;       aberration_ : Set this to 1 to force aberration correction [default],
;                 0 for no correction.
;       refract_  : Set to 1 to force refraction correction [default], 0 for
;                   no correction.
;       altitude: The altitude of the observing location, in meters. [default=0].
;       /verbose: Set this for verbose output.  The default is verbose=0.
;   _extra: This is for setting TEMPERATURE or PRESSURE explicitly, which are
;           used by CO_REFRACT to calculate the refraction effect of the
;           atmosphere. If you don't set these, the program will make an
;           intelligent guess as to what they are (taking into account your
;            altitude).  See CO_REFRACT for more details.
;
; OUTPUT VARIABLES
;       ra   : Right Ascension of object  (J2000) in degrees (FK5); scalar or
;              vector.
;       dec  : Declination of object (J2000) in degrees (FK5), scalar or vector.
;       ha   : hour angle (in degrees) (optional)
;
; DEPENDENCIES:
;       NUTATE, PRECESS, ADSTRING(), SUNPOS, OBSERVATORY (from the astrolib)
;       CO_NUTATE, CO_ABERRATION, CO_REFRACT, HADEC2ALTAZ
;
; BASIC STEPS
;   Precess Ra-Dec to current equinox.
;   Nutation Correction to Ra-Dec
;   Aberration correction to Ra-Dec
;   Calculate Local Mean Sidereal Time
;   Calculate Local Apparent Sidereal Time
;   Calculate Hour Angle
;   Do Spherical Trig to find Apparent Alt-Az
;   Apply refraction correction to find observed Alt.
;
;CORRECTIONS I DO NOT MAKE:
;   *  Deflection of Light by the sun due to GR. (typically milliarcseconds,
;        can be arcseconds within one degree of the sun)
;   *  The Effect of Annual Parallax (typically < 1 arcsecond)
;   *  and more (see below)
;
; TO DO
;    * Better Refraction Correction.  Need to put in wavelength dependence,
;       and integrate through the atmosphere.
;    * Topocentric Parallax Correction (will take into account elevation of
;          the observatory)
;    * Proper Motion (but this will require crazy lookup tables or something).
;    * Difference between UTC and UT1 in determining LAST -- is this important?
;    * Effect of Annual Parallax (is this the same as topocentric Parallax?)
;    * Polar Motion
;    * Better connection to Julian Date Calculator.
;
; EXAMPLE:
;
;   You are at Kitt Peak National Observatory, looking at a star at azimuth
;   angle 264d 55m 06s and elevation 37d 54m 41s (in the visible).  Today is
;   Dec 25, 2041 and the local time is 10 PM precisely.  What is the ra and dec
;   (J2000) of the star you're looking at?   The temperature here is about 0
;   Celsius, and the pressure is 781 millibars.    The Julian date for this
;   time is 2466879.7083333
;
;  IDL> hor2eq, ten(37,54,41), ten(264,55,06), 2466879.7083333d, ra, dec, $
;           /verb, obs='kpno', pres=781.0, temp=273.0
;
; The program produces this output (because the VERBOSE keyword was set):
;
; Latitude = +31 57 48.0   Longitude = *** 36  0.0   ; longitude prints weirdly b/c of negative input to ADSTRING!!
; Julian Date =  2466879.708333
; Az, El =  17 39 40.4  +37 54 41.0   (Observer Coords)
; Az, El =  17 39 40.4  +37 53 39.6   (Apparent Coords)
; LMST = +03 53 54.1
; LAST = +03 53 53.6
; Hour Angle = +03 38 30.1  (hh:mm:ss)
; Ra, Dec:  00 15 23.5  +15 25  1.9   (Apparent Coords)
; Ra, Dec:  00 15 24.2  +15 25  0.1   (J2041.9841)
; Ra, Dec:  00 13 14.1  +15 11  0.3   (J2000)
;
; The star is therefore Algenib!  Compare the derived Ra, Dec with what XEPHEM
; got:
; Ra, Dec:      00 13 14.2  +15 11  1.0   (J2000)
;
; AUTHOR:
;   Chris O'Dell
;   Assistant Professor of Atmospheric Science
;   Colorado State University
;   Email: odell@atmos.colostate.edu
; REVISION HISTORY:
;     Made all integers type LONG  W. Landsman   September 2007
;     Fixed for case of scalar Julian date but vector positions W L June 2009
;-

pro hor2eq, alt, az, jd, ra, dec, ha, lat=lat, lon=lon, WS=WS, obsname=obsname,$
           B1950 = B1950, verbose=verbose, precess_=precess_, nutate_=nutate_, $
           refract_ = refract_, aberration_ = aberration_, altitude=altitude, $
           _extra = _extra

 On_error,2
 compile_opt idl2
 if N_params() LT 4 then begin
   print,'Syntax - HOR2EQ, alt, az, jd, ra, dec, [ha, LAT= , LON= , /WS, '
   print,'        OBSNAME= ,/B1950 , PRECESS_= 0, NUTATE_= 0, REFRACT_= 0, '
   print,'        ABERRATION_= 0, ALTITUDE= , /VERBOSE, TEMPERATURE=, PRESSURE='
   return
 endif
;*******************************************************************************
; INITIALIZE STUFF

; If no lat or lng entered, use Pine Bluff Observatory values
if n_elements(lat) eq 0 then lat = 43.0783d
; (btw, this is the declination of the zenith)
if n_elements(lon) eq 0 then lon = -89.865d

if keyword_set(obsname) then begin
        ;override lat,lon if observatory name has been specified
        Observatory, obsname, obs
        lat = obs.latitude
        lon = -1*obs.longitude ; minus sign is becase OBSERVATORY uses west
;                              ;longitude as positive.
        altitude = obs.altitude
endif

if n_elements(precess_) eq 0 then precess_ = 1
if n_elements(nutate_) eq 0 then nutate_ = 1
if n_elements(aberration_) eq 0 then aberration_ = 1
if n_elements(refract_) eq 0 then refract_ = 1
v = keyword_set(verbose)

; conversion factors
d2r = !dpi/180.
h2d = 15.

alt_ = alt   ;do this so we don't change ra, dec arrays.
az_ = az

if v then print, 'Latitude = ', adstring(lat), '   Longitude = ', adstring(lon)
if v then print, 'Julian Date = ', jd, format='(A,f15.6)'
if v then print,'Az, El = ', adstring(az_, alt_), '   (Observer Coords)'

;*******************************************************************************************
; Make Correction for ATMOSPHERIC REFRACTION
; (use this for visible and radio wavelengths; author is unsure about other wavelengths)
if refract_ then alt_ = co_refract(alt_, altitude=altitude, _extra=_extra)
if v then print,'Az, El = ', adstring(az_, alt_), '   (Apparent Coords)'

if keyword_set(WS) then az_ = az_ - 180.

co_nutate, jd, 45.,45., dra1, ddec1, eps=eps, d_psi=d_psi

;******************************************************************************
;Calculate LOCAL APPARENT SIDEREAL TIME
; first get local mean sidereal time (lmst)
; get LST (in hours) - note:this is indep of tzone since giving jd
ct2lst, lmst, lon, 0, jd
lmst = lmst*h2d ; convert LMST to degrees (btw, this is the RA of the zenith)
; calculate local APPARENT sidereal time (last)
last = lmst + d_psi *cos(eps)/3600. ; add correction in degrees
if v then print, 'LMST = ', adstring(lmst/15.)
if v then print, 'LAST = ', adstring(last/15.)

;****************************************************************************
; Now do the spherical trig to get APPARENT Hour Angle [degrees], and
; declination [degrees].
altaz2hadec, alt_, az_, lat, ha, dec

; Find Right Ascension (in degrees, from 0 to 360.)
 ra = (last - ha + 360.) mod 360.

if v then print, 'Hour Angle = ', adstring(ha/15.), '  (hh:mm:ss)'
if v then print, 'Ra, Dec: ', adstring(ra,dec), '   (Apparent Coords)'


;*****************************************************************************
; calculate NUTATION and ABERRATION Corrections to Ra-Dec
co_nutate, jd, ra, dec, dra1, ddec1, eps=eps, d_psi=d_psi
co_aberration, jd, ra, dec, dra2, ddec2, eps=eps

;******************************************************************************
; Make Nutation and Aberration Corrections (if wanted)
ra = ra - (dra1*nutate_ + dra2*aberration_)/3600.
dec = dec - (ddec1*nutate_ + ddec2*aberration_)/3600.
J_now = (JD - 2451545.)/365.25 + 2000.0 ; compute current equinox
Njd = N_elements(J_now)
Npos = N_elements(ra)
if (Njd EQ 1) and (Npos GT 1) then J_now = replicate(J_now, Npos) 
if v then print, 'Ra, Dec: ', adstring(ra,dec), '   (J'+ $
           strcompress(string(J_now),/rem)+')'

;*****************************************************************************
; PRECESS coordinates to current date
; (uses astro lib procedure PRECESS.pro)

if precess_ then begin
        if keyword_set(B1950) then begin
                for i=0, Npos-1 do begin
                        ra_i = ra[i] & dec_i = dec[i]
                        precess, ra_i, dec_i, J_now[i], 1950.0, /FK4
                        ra[i] = ra_i & dec[i] = dec_i
                endfor
        endif else begin
                for i=0, Npos-1 do begin
                        ra_i = ra[i] & dec_i = dec[i]
                        precess, ra_i, dec_i, J_now[i], 2000.0
                        ra[i] = ra_i & dec[i] = dec_i
                endfor
        endelse
endif
if keyword_set(B1950) then s_now='   (J1950)' else s_now='   (J2000)'
if v then print, 'Ra, Dec: ', adstring(ra,dec), s_now

Return
END
