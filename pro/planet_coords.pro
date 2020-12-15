pro planet_coords, date, ra, dec, planet=planet, jd = jd, jpl = jpl
;+
; NAME:
;    PLANET_COORDS
; PURPOSE:  
;    Find low or high precision RA and DEC for the planets given a date
;
; EXPLANATION:
;    For low precision this routine uses HELIO to get the heliocentric ecliptic
;    coordinates of the planets at the given date, then converts these to 
;    geocentric ecliptic coordinates ala "Astronomical Algorithms" by Jean 
;    Meeus (1991, p 209). These are then converted to RA and Dec using EULER.
;    The accuracy between the years 1800 and 2050 is better than 1 arcminute 
;    for  the terrestial planets, but reaches 10 arcminutes for Saturn.    
;    Before 1850 or after 2050 the accuracy can get much worse.   
;
;    For high precision use the /JPL option ito use the full JPL ephemeris.
; CALLING SEQUENCE:
;    PLANET_COORDS, DATE, RA, DEC, [ PLANET = , /JD, /JPL]
;
; INPUTS:
;       DATE - If /JD is not set, then date is a 3-6 element vector containing
;              year,month (1-12), day, and optionally hour, minute, & second.
;              If /JD is set then DATE is a Julian date.   An advantage of the
;              /JD option is that it allows the use of vector dates.
; OUTPUTS:
;       RA - right ascension of planet(s), J2000 degrees, double precision
;       DEC - declination of   planet(s), J2000 degrees, double precision
;
; OPTIONAL INPUT KEYWORD:
;       PLANET - scalar string giving name of a planet, e.g. 'venus'. Default 
;               is to compute coords for all of them (except Earth).
;       /JD - If set, then the date parameter should be supplied as Julian date
;       JPL - if /JPL set, then PLANET_COORDS will call the procedure 
;             JPLEPHINTERP to compute positions using the full JPL ephemeris.   
;             The JPL ephemeris FITS file JPLEPH.405 must exist in either the 
;             current directory, or in the directory specified by the 
;             environment variable ASTRO_DATA.   Alternatively, the JPL keyword
;             can be set to the full path and name of the ephemeris file.
;             A copy of the JPL ephemeris FITS file JPLEPH.405 is available in
;                 http://idlastro.gsfc.nasa.gov/ftp/data/         
; EXAMPLES:
;    (1)  Find the RA, Dec of Venus on 2018 Dec 20
;          IDL> planet_coords, [2018,12,20], ra,dec    ;Compute for all planets
;          IDL> print,adstring(ra[1],dec[1],1)         ;Venus is second planet
;     ====> RA = 14 42 41.17  Dec = -12 30 53.5
;    This position is 61" from the full JPL ephemeris position of
;          RA = 14 42 45.32       -12 30 50.6
;
;    (2) Return the current RA and Dec of all 8 planets using JPL ephemeris
;          IDL> get_juldate, jd                 ;Get current Julian Date
;          IDL> planet_coords,jd,ra,dec,/jd,/jpl  ;Find positions of all planets
;          IDL> forprint,adstring(ra,dec,0)     ;Display positions   
;
;    (3) Plot the declination of Mars for every day in the year 2001
;          IDL> jdcnv,2001,1,1,0,jd      ;Get Julian date of midnight on Jan 1 
;               Now get Mars RA,Dec for 365 consecutive days
;          IDL> planet_coords,jd+indgen(365),ra,dec,/jd, planet = 'mars'     
;          IDL> plot,indgen(365)+1,dec
; NOTES:
;          HELIO is based on the two-body problem and neglects interactions 
;           between the planets.   This is why the worst results are for
;           Saturn.   Use the /JPL option or the online ephemeris generator 
;           http://ssd.jpl.nasa.gov/horizons.cgi for more accuracy. 
;
;           The procedure returns astrometric coordinates, i.e. no correction
;           for aberration.   A correction for light travel time is applied
;           when /JPL is set, but not for the default low-precision calculation.
; PROCEDURES USED:
;        JULDATE 
;        EULER, HELIO  - if /JPL is not set
;        JPLEPHREAD, JPLEPHINTERP - if /JPL is set
; REVISION HISTORY:
;        Written P.Plait & W. Landsman     August 2000
;        Fixed Julian date conversion   W. Landsman August 2000
;        Added /JPL keyword  W. Landsman   July 2001
;        Allow vector Julian dates with JPL ephemeris W. Landsman December 2002
;-   
; On_error,2
 if N_params() LT 1 then begin
     print,'Syntax - PLANET_COORDS, date, ra,dec, [PLANET =, /JD , JPL= ]'
     print,'      date - either 3-6 element date or Julian date (if /JD is set)'
     print,'      ra,dec - output ra and dec in degrees'
     print,'      PLANET - name of planet (optional)'
     return
 endif

 radeg = 180.0d/!DPI
 c = 2.99792458d5

;convert input date to real JD
  
  if keyword_set(jd) then begin 
       jj = date
       if N_elements(jj) GT 0 then if N_elements(planet) GT 1 then $
       message,'ERROR - A planet name must be supplied for vector dates'
  endif else begin 
           juldate,date,jj
            jj = jj + 2400000.0d
  endelse 

;make output arrays to include each planet
; note that we need Earth to convert from heliocentric
; ecliptic coordinates to geocentric and then to RA and DEC

   if keyword_set(planet) then begin
       planetlist = ['MERCURY','VENUS','MARS', $
        'JUPITER','SATURN','URANUS','NEPTUNE','PLUTO']
        index = 1+ where(planetlist eq strupcase(strtrim(planet,2)), Nfound) 
        if index[0] GE 3 then index = index + 1
       if Nfound EQ 0 then message,'Unrecognized planet of ' + planet
   endif else index = [1,2,4,5,6,7,8,9]

   if keyword_set(JPL) then begin
       if size(jpl,/TNAME) EQ 'STRING' then jplfile = jpl else $
            jplfile = find_with_def('JPLEPH.405','ASTRO_DATA')

        if jplfile EQ '' then message,'ERROR - Cannot find JPL ephemeris file' 
;Read ephemeris FITS file
        JPLEPHREAD,jplfile, pinfo, pdata, [long(min(jj)-1), long(max(jj)+1)]
        np = N_elements(index)
        njd = n_elements(jj)
        ra = dblarr(njd,np)   & dec = dblarr(njd,np)

        for i=0, Np-1 do begin
        JPLEPHINTERP, pinfo, pdata, jj, x,y,z, $
                       objectname=index[i],center='EARTH'
;  Compute distance to planet(s) and adjust Julian date for light travel time
; and recompute planet positions
       dis = sqrt(x^2 + y^2 + z^2)
       jj1 = jj - dis/c/86400.0d

; Compute position of Earth at current time, but position of planet at time
; light started traveling 
       JPLEPHINTERP, pinfo, pdata, jj, xe,ye,ze, /EARTH
       JPLEPHINTERP, pinfo, pdata, jj1, x,y,z, objectname=index[i]
       x = x-xe & y = y-ye & z = z-ze
         ra[0,i] = atan(y,x) * radeg
       g = where(ra LT 0, Ng)
       if Ng GT 0 then ra[g] = ra[g] + 360.0d
       dec[0,i]   = atan(z,sqrt(x*x + y*y)) * radeg
       endfor
       ra = reform(ra) & dec = reform(dec)
       return
   endif

   helio,jj,index,rad,lon,lat,/radian

; extract Earth-Moon barycenter info

   helio,jj,3,rade,lone,late,/radian

;get rectangular coords of planets

   x = rad * cos(lat) * cos(lon) - rade * cos(late) * cos(lone)
   y = rad * cos(lat) * sin(lon) - rade * cos(late) * sin(lone)
   z = rad * sin(lat)            - rade * sin(late)

;get geocentric longitude lambda and geo latitude, beta

   lambda = atan(y,x) * radeg
   beta   = atan(z,sqrt(x*x + y*y)) * radeg

;convert to Ra and Dec

   euler, lambda, beta, ra, dec, 4

   return
   end
