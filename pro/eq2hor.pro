;+
; NAME:
;   EQ2HOR
;
; PURPOSE:
;    Convert celestial  (ra-dec) coords to local horizon coords (alt-az).
;
; CALLING SEQUENCE:
;
;    eq2hor, ra, dec, jd, alt, az, [ha, LAT= , LON= , /WS, OBSNAME= , $
;                       /B1950 , PRECESS_= 0, NUTATE_= 0, REFRACT_= 0, $
;                       ABERRATION_= 0, ALTITUDE= , /VERBOSE, _EXTRA= ]
;
; DESCRIPTION:
;  This  code calculates horizon (alt,az) coordinates from equatorial
;  (ra,dec) coords.   It is typically accurate to about 1 arcsecond or better (I
;  have checked the output against the publicly available XEPHEM software). It
;  performs precession, nutation, aberration, and refraction corrections.  The
;  perhaps best thing about it is that it can take arrays as inputs, in all
;  variables and keywords EXCEPT Lat, lon, and Altitude (the code assumes these
;  aren't changing), and uses vector arithmetic in every calculation except
;  when calculating the precession matrices.
;
; INPUT-OUTPUT VARIABLES:
;       RA   : Right Ascension of object  (J2000) in degrees (FK5); scalar or
;              vector.
;       Dec  : Declination of object (J2000) in degrees (FK5), scalar or vector.
; INPUT VARIABLES:
;       JD   : Julian Date [scalar or vector]
;
;       Note: if RA and DEC are arrays, then alt and az will also be arrays.
;             If RA and DEC are arrays, JD may be a scalar OR an array of the
;             same dimensionality.
;
; OPTIONAL INPUT KEYWORDS:
;       lat   : north geodetic latitude of location in degrees
;       lon   : EAST longitude of location in degrees (Specify west longitude
;               with a negative sign.)
;       /WS    : Set this to get the azimuth measured westward from south (not
;               East of North).
;       obsname: Set this to a valid observatory name to be used by the
;              astrolib OBSERVATORY procedure, which will return the latitude
;              and longitude to be used by this program.
;       /B1950 : Set this if your ra and dec are specified in B1950, FK4
;              coordinates (instead of J2000, FK5)
;       precess_ : Set this to 1 to force precession [default], 0 for no
;               precession correction
;       nutate_  : Set this to 1 to force nutation [default], 0 for no nutation.
;       aberration_ : Set this to 1 to force aberration correction [default],
;                     0 for no correction.
;       refract_ : Set to 1 to force refraction correction [default], 0 for no
;                     correction.
;       altitude: The altitude of the observing location, in meters. [default=0].
;       verbose: Set this for verbose output.  The default is verbose=0.
;       _extra: This is for setting TEMPERATURE or PRESSURE explicitly, which are
;               used by CO_REFRACT to calculate the refraction effect of the
;               atmosphere. If you don't set these, the program will make an
;               intelligent guess as to what they are (taking into account your
;               altitude).  See CO_REFRACT for more details.
;
; OUTPUT VARIABLES: (all double precision)
;       alt    : altitude, also known as elevation (in degrees)
;       az     : azimuth angle (in degrees, measured EAST from NORTH, but see
;                keyword WS above.)
;       ha     : hour angle (in degrees) (optional)
;
; DEPENDENCIES:
;       NUTATE, PRECESS, OBSERVATORY, SUNPOS, ADSTRING()
;       CO_NUTATE, CO_ABERRATION, CO_REFRACT, ALTAZ2HADEC, SETDEFAULTVALUE
;
; BASIC STEPS
;   Apply refraction correction to find apparent Alt.
;   Calculate Local Mean Sidereal Time
;   Calculate Local Apparent Sidereal Time
;   Do Spherical Trig to find apparent hour angle, declination.
;   Calculate Right Ascension from hour angle and local sidereal time.
;   Nutation Correction to Ra-Dec
;   Aberration correction to Ra-Dec
;       Precess Ra-Dec to current equinox.
;
;
;CORRECTIONS I DO NOT MAKE:
;   *  Deflection of Light by the sun due to GR. (typically milliarcseconds,
;        can be arseconds within one degree of the sun)
;   *  The Effect of Annual Parallax (typically < 1 arcsecond)
;   *  and more (see below)
;
; TO DO
;    * Better Refraction Correction.  Need to put in wavelength dependence,
;    and integrate through the atmosphere.
;        * Topocentric Parallax Correction (will take into account elevation of
;          the observatory)
;    * Proper Motion (but this will require crazy lookup tables or something).
;        * Difference between UTC and UT1 in determining LAST -- is this
;          important?
;        * Effect of Annual Parallax (is this the same as topocentric Parallax?)
;    * Polar Motion
;        * Better connection to Julian Date Calculator.
;
; EXAMPLE
;
;  Find the position of the open cluster NGC 2264 at the Effelsburg Radio
;  Telescope in Germany, on June 11, 2023, at local time 22:00 (METDST).
;  The inputs will then be:
;
;       Julian Date = 2460107.250
;       Latitude = 50d 31m 36s
;       Longitude = 06h 51m 18s
;       Altitude = 369 meters
;       RA (J2000) = 06h 40m 58.2s
;       Dec(J2000) = 09d 53m 44.0s
;
;  IDL> eq2hor, ten(6,40,58.2)*15., ten(9,53,44), 2460107.250d, alt, az, $
;               lat=ten(50,31,36), lon=ten(6,51,18), altitude=369.0, /verb, $
;                pres=980.0, temp=283.0
;
; The program produces this output (because the VERBOSE keyword was set)
;
; Latitude = +50 31 36.0   Longitude = +06 51 18.0
; ************************** 
;Julian Date =  2460107.250000
;LMST = +11 46 42.0
;LAST = +11 46 41.4
; 
;Ra, Dec:  06 40 58.2  +09 53 44   (J2000)
;Ra, Dec:  06 42 15.7  +09 52 19   (J2023.4422)
;Ra, Dec:  06 42 13.8  +09 52 27   (fully corrected)
;Hour Angle = +05 04 27.6  (hh:mm:ss)
;Az, El =  17 42 25.6  +16 25 10   (Apparent Coords)
;Az, El =  17 42 25.6  +16 28 23   (Observer Coords)
;
; Compare this with the result from XEPHEM:
; Az, El =  17h 42m 25.6s +16d 28m 21s
;
; This 1.8 arcsecond discrepancy in elevation arises primarily from slight
; differences in the way I calculate the refraction correction from XEPHEM, and
; is pretty typical.
;
; AUTHOR:
;   Chris O'Dell
;    Assistant Professor of Atmospheric Science
;    Colorado State University
;    Email: odell@atmos.colostate.edu
;   
;  Revision History: 
;    August 2012  Use Strict_Extra to flag spurious keywords W. Landsman
;    May 2013   Fix case of scalar JD but vector RA, Dec W. Landsman
;    Jun 2014   Fix case of vector JD but scalar RA, Dec W. Landsman
;    Sep 2017  Work with /verbose and vector JD W. Landsman
;-

pro eq2hor, ra, dec, jd, alt, az, ha, lat=lat, lon=lon, WS=WS, obsname=obsname,$
     B1950 = B1950, verbose=verbose, precess_=precess_, nutate_=nutate_, $
                refract_ = refract_, aberration_ = aberration_,  $
                altitude = altitude, _extra= _extra

 On_error,2
 compile_opt idl2
 
if N_params() LT 4 then begin
    print,'Syntax - EQ2HOR, ra, dec, jd, alt, az, [ha, LAT= , LON= , /WS, '
    print,'          OBSNAME= ,/B1950 , PRECESS_= 0, NUTATE_= 0, REFRACT_= 0 '
    print,'          ABERRATION_= 0, ALTITUDE= , /VERBOSE, TEMPERATURE=, ' +$
          'PRESSURE = ]'
     return
 endif

;*******************************************************************************
; INITIALIZE STUFF

; If no lat or lng entered, use Pine Bluff Observatory values!
;   (near Madison, Wisconsin, USA)
; * Feel free to change these to your favorite observatory *
v = keyword_set(verbose)
if keyword_set(obsname) then begin
        ;override lat,lon, altitude if observatory name has been specified
        observatory, obsname, obs
        lat = obs.latitude
        lon = -1*obs.longitude ; minus sign is because OBSERVATORY uses west
;                               longitude as positive.
        altitude = obs.altitude
endif
if ~v && ((N_elements(lat) EQ 0 ) || N_elements(lon) Eq 0) then $
   message,'Using latitude and longitude for Pine Bluff Observatory',/con
setdefaultvalue, lat,   43.0783d ; (this is the declination of the zenith)
setdefaultvalue, lon, -89.865d
setdefaultvalue, altitude, 0.                ; [meters]

setdefaultvalue, precess_, 1
setdefaultvalue, nutate_, 1
setdefaultvalue, aberration_, 1
setdefaultvalue, refract_ , 1


; conversion factors
d2r = !dpi/180.
h2r = !dpi/12.
h2d = 15.d

npos = N_elements(ra)
njd = N_elements(jd)

if ~((npos EQ njd) || (npos EQ 1) || (njd EQ 1)) then message,'Error - ' + $
   'Either JD or (ra,dec) must be scalars, or have the same # of elements'

if (npos EQ 1) && (njd GT 1) then begin  
    ra_ = replicate(double(ra[0]),njd)
    dec_ = replicate(double(dec[0]),njd)
endif else begin 
    ra_ = ra
    dec_ = dec
endelse    

if keyword_set(B1950) then begin 
     tstart = 1950.0
     s_now='   (B1950)' 
endif else begin 
     tstart = 2000.0
     s_now='   (J2000)'
endelse 

;******************************************************************************
; PRECESS coordinates to current date
; (uses astro lib procedure PRECESS.pro)
J_now = (JD - 2451545.)/365.25 + 2000.0 ; compute current equinox
if precess_ then begin
   if njd GT 1 then begin
       for i=0,n_elements(jd)-1 do begin
            tmpra = ra_[i]  & tmpdec = dec_[i]
            precess, tmpra, tmpdec, tstart, J_now[i], FK4 = keyword_set(B1950)
	    ra_[i] = tmpra & dec_[i] = tmpdec
        endfor
    endif else $
            precess, ra_, dec_, tstart, J_now, FK4 = keyword_set(B1950)
 endif
if v then begin      
   rap = ra_
   decp = dec_
endif
;******************************************************************************
; calculate NUTATION and ABERRATION Corrections to Ra-Dec

co_nutate, jd, ra_, dec_, dra1, ddec1, eps=eps, d_psi=d_psi
co_aberration, jd, ra_, dec_, dra2, ddec2, eps=eps

; make nutation and aberration corrections
ra_ +=  (dra1*nutate_ + dra2*aberration_)/3600.
dec_ +=  (ddec1*nutate_ + ddec2*aberration_)/3600.

;**************************************************************************************
;Calculate LOCAL MEAN SIDEREAL TIME
ct2lst, lmst, lon, 0, jd  ; get LST (in hours) - note:this is independent of
                           ;time zone  since giving jd
lmst = lmst*h2d ; convert LMST to degrees (btw, this is the RA of the zenith)
; calculate local APPARENT sidereal time
LAST = lmst + d_psi *cos(eps)/3600. ; add correction in degrees

;******************************************************************************
; Find hour angle (in DEGREES)
ha = last - ra_
w = where(ha LT 0, Nw)
if Nw GT 0 then ha[w] = ha[w] + 360.
ha = ha mod 360.

;******************************************************************************
; Now do the spherical trig to get APPARENT alt,az.
hadec2altaz, ha, dec_, lat, alt, az, WS=WS

;*******************************************************************************************
; Make Correction for ATMOSPHERIC REFRACTION
; (use this for visible and radio wavelengths; author is unsure about other wavelengths.
;  See the comments in CO_REFRACT.pro for more details.)
if v then alt_app = alt
if refract_ then alt = $
      co_refract(alt, altitude=altitude, _strict_extra=_extra, /to_observed)
if v then begin
     bothvector = (njd GT 1) && (npos EQ njd) 
     print, 'Latitude = ', adstring(lat), '   Longitude = ', adstring(lon)
     for j=0,njd-1 do begin 
	  print,' ************************** '

        print, 'Julian Date = ', jd[j], format='(A,f15.6)'
        print, 'LMST = ', adstring(lmst[j]/15.)
        print, 'LAST = ', adstring(last[j]/15.)
	print,' '
          for i=0,npos-1 do begin
               if bothvector then i = j
               print, 'Ra, Dec: ', adstring(ra[i],dec[i]), s_now
               print, 'Ra, Dec: ', adstring(rap[i],decp[i]), '   (J' + $
                       strcompress(string(J_now[i]),/rem)+')'
		      
               print, 'Ra, Dec: ', adstring(ra_[i],dec_[i]), $
	               '   (fully corrected)'
               print, 'Hour Angle = ', adstring(ha[i]/15.), '  (hh:mm:ss)'

	       print,'Az, El = ', adstring(az[i],alt_app[i]), '   (Apparent Coords)'       
               print,'Az, El = ', adstring(az[i],alt[i]), '   (Observer Coords)'
               print,' '
               if bothvector then break
	  endfor 
	  endfor
  endif   
  return  
end
