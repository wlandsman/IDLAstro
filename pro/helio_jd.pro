function helio_jd,date,ra,dec, B1950 = B1950, TIME_DIFF = time_diff
;+
; NAME:
;      HELIO_JD
; PURPOSE:
;      Convert geocentric (reduced) Julian date to heliocentric Julian date
; EXPLANATION:
;      This procedure correct for the extra light travel time between the Earth 
;      and the Sun.
;
;       An online calculator for this quantity is available at 
;       http://www.physics.sfasu.edu/astro/javascript/hjd.html
;
;       Users requiring more precise calculations and documentation should 
;       look at the IDL code available at 
;       http://astroutils.astronomy.ohio-state.edu/time/
; CALLING SEQUENCE:
;       jdhelio = HELIO_JD( date, ra, dec, /B1950, /TIME_DIFF)
;
; INPUTS
;       date - reduced Julian date (= JD - 2400000), scalar or vector, MUST
;               be double precision
;       ra,dec - scalars giving right ascension and declination in DEGREES
;               Equinox is J2000 unless the /B1950 keyword is set
;
; OUTPUTS:
;       jdhelio - heliocentric reduced Julian date.  If /TIME_DIFF is set, then
;                 HELIO_JD() instead returns the time difference in seconds
;                 between the geocentric and heliocentric Julian date.
;                 
; OPTIONAL INPUT KEYWORDS 
;       /B1950 - if set, then input coordinates are assumed to be in equinox 
;                B1950 coordinates.
;       /TIME_DIFF - if set, then HELIO_JD() returns the time difference
;                (heliocentric JD - geocentric JD ) in seconds 
;
; EXAMPLE:
;       What is the heliocentric Julian date of an observation of V402 Cygni
;       (J2000: RA = 20 9 7.8, Dec = 37 09 07) taken June 15, 1973 at 11:40 UT?
;
;       IDL> juldate, [1973,6,15,11,40], jd      ;Get geocentric Julian date
;       IDL> hjd = helio_jd( jd, ten(20,9,7.8)*15., ten(37,9,7) )  
;                                                            
;       ==> hjd = 41848.9881
;
; Wayne Warren (Raytheon ITSS) has compared the results of HELIO_JD with the
; FORTRAN subroutines in the STARLINK SLALIB library (see 
; http://star-www.rl.ac.uk/).    
;                                                  Time Diff (sec)
;      Date               RA(2000)   Dec(2000)  STARLINK      IDL
;
; 1999-10-29T00:00:00.0  21 08 25.  -67 22 00.  -59.0        -59.0
; 1999-10-29T00:00:00.0  02 56 33.4 +00 26 55.  474.1        474.1
; 1940-12-11T06:55:00.0  07 34 41.9 -00 30 42.  366.3        370.2
; 1992-02-29T03:15:56.2  12 56 27.4 +42 10 17.  350.8        350.9
; 2000-03-01T10:26:31.8  14 28 36.7 -20 42 11.  243.7        243.7
; 2100-02-26T09:18:24.2  08 26 51.7 +85 47 28.  104.0        108.8
; PROCEDURES CALLED:
;       bprecess, xyz, zparcheck
;
; REVISION HISTORY:
;       Algorithm from the book Astronomical Photometry by Henden, p. 114
;       Written,   W. Landsman       STX     June, 1989 
;       Make J2000 default equinox, add B1950, /TIME_DIFF keywords, compute
;       variation of the obliquity      W. Landsman   November 1999
;-
 On_error,2
 If N_params() LT 3 then begin
    print,'Syntax -   jdhelio = HELIO_JD( date, ra, dec, /B1950, /TIME_DIFF)'
    print,'      date - reduced Julian date (= JD - 2400000)'
    print,'      Ra and Dec must be in degrees'
 endif

;Because XYZ uses default B1950 coordinates, we'll convert everything to B1950

 if not keyword_set(B1950) then bprecess,ra,dec,ra1,dec1 else begin
        ra1 = ra
        dec1 = dec
 endelse
 
 radeg = 180.0d/!DPI   
 zparcheck,'HELIO_JD',date,1,[3,4,5],[0,1],'Reduced Julian Date'

 delta_t = (double(date) - 33282.42345905d)/36525.0d
 epsilon_sec = poly( delta_t, [44.836d, -46.8495, -0.00429, 0.00181])
 epsilon = (23.433333d0 + epsilon_sec/3600.0d)/radeg
 ra1 = ra1/radeg
 dec1 = dec1/radeg

 xyz, date, x, y, z

;Find extra distance light must travel in AU, multiply by 1.49598e13 cm/AU,
;and divide by the speed of light, and multiply by 86400 second/year

 time = -499.00522d*( cos(dec1)*cos(ra1)*x + $
                 (tan(epsilon)*sin(dec1) + cos(dec1)*sin(ra1))*y)

 if keyword_set(TIME_DIFF) then return, time else $
           
       return, double(date) + time/86400.0d

 end
