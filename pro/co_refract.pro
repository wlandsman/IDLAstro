;+
; NAME:
;   CO_REFRACT()      
;
; PURPOSE:
;   Calculate correction to altitude due to atmospheric refraction.
;
; DESCRIPTION:
;   CO_REFRACT can calculate both apparent altitude from observed altitude and 
;   vice-versa.
;
; CALLING SEQUENCE:
;   new_alt  = CO_REFRACT(old_alt, [ ALTITUDE= , PRESSURE= , $
;                                  TEMPERATURE= , /TO_OBSERVED , EPSILON= ])
;
; INPUT:
;   old_alt - Observed (apparent) altitude, in DEGREES.  (apparent if keyword 
;             /TO_OBSERVED set).    May be scalar or vector.
;
; OUTPUT: 
;     Function returns apparent (observed) altitude, in DEGREES. (observed if 
;         keyword /TO_OBSERVED set).    Will be of same type as input 
;         altitude(s).
;
; OPTIONAL KEYWORD INPUTS:
;      ALTITUDE :  The height of the observing location, in meters.  This is 
;             only used to determine an approximate temperature and pressure, 
;             if these are not specified separately. [default=0, i.e. sea level]
;      PRESSURE :  The pressure at the observing location, in millibars.
;      TEMPERATURE:    The temperature at the observing location, in Kelvin.
;      EPSILON:  When keyword /TO_OBSERVED has been set, this is the accuracy 
;               to  obtain via the iteration, in arcseconds [default = 0.25 
;                arcseconds].
;      /TO_OBSERVED:  Set this keyword to go from Apparent->Observed altitude, 
;                 using the iterative technique.
;
;       Note, if altitude is set, but temperature or pressure are not, the 
;       program will make an intelligent guess for the temperature and pressure.
;
; DESCRIPTION:
;
;   Because the index of refraction of air is not precisely 1.0, the atmosphere
;   bends all incoming light, making a star or other celestial object appear at
;   a slightly different altitude (or elevation) than it really is.  It is 
;   important to understand the following definitions:
;
;   Observed Altitude:  The altitude that a star is SEEN to BE, with a telescope.
;                       This is where it appears in the sky.  This is always 
;                       GREATER than the apparent altitude.
;
;   Apparent Altitude:  The altitude that a star would be at, if *there were no
;                     atmosphere* (sometimes called "true" altitude). This is 
;                     usually calculated from an object's celestial coordinates.
;                     Apparent altitude is always LOWER than the observed 
;                     altitude.
;
;   Thus, for example, the Sun's apparent altitude when you see it right on the
;   horizon is actually -34 arcminutes.
;
;   This program uses couple simple formulae to estimate the effect for most 
;   optical and radio wavelengths.  Typically, you know your observed altitude 
;   (from an observation), and want the apparent altitude.  To go the other way,
;   this program uses an iterative approach.
;
; EXAMPLE:
;    The lower limb of the Sun is observed to have altitude of 0d 30'.   
;    Calculate the the true (=apparent) altitude of the Sun's lower limb using 
;    mean  conditions of air pressure and temperature
;
;    IDL> print, co_refract(0.5)     ===>  0.025degrees (1.55')
; WAVELENGTH DEPENDENCE:
;    This correction is 0 at zenith, about 1 arcminute at 45 degrees, and 34 
;    arcminutes at the horizon FOR OPTICAL WAVELENGTHS.  The correction is 
;    NON-NEGLIGIBLE at all wavelengths, but is not very easily calculable.  
;    These formulae assume a wavelength of 550 nm, and will be accurate to 
;    about 4 arcseconds for all visible wavelengths, for elevations of 10 
;    degrees and higher.    Amazingly, they are also ACCURATE FOR RADIO 
;    FREQUENCIES LESS THAN ~ 100 GHz.
;
;    It is important to understand that these formulae really can't do better 
;    than about 30 arcseconds of accuracy very close to the horizon, as 
;    variable atmospheric effects become very important.
;
; REFERENCES:
;    1.  Meeus, Astronomical Algorithms, Chapter 15.
;    2.  Explanatory Supplement to the Astronomical Almanac, 1992.
;    3.  Methods of Experimental Physics, Vol 12 Part B, Astrophysics, 
;        Radio Telescopes, Chapter 2.5, "Refraction Effects in the Neutral 
;        Atmosphere", by R.K. Crane.
;
;
; DEPENDENCIES:
;    CO_REFRACT_FORWARD (contained in this file and automatically compiled).
;
; AUTHOR:
;   Chris O'Dell  
;     Assistant Professor of Atmospheric Science
;      Colorado State University
;   Email: odell@atmos.colostate.edu
;
; REVISION HISTORY:
;    version 1 (May 31, 2002)
;    Update iteration formula,   W. Landsman    June 2002
;    Corrected slight bug associated with scalar vs. vector temperature and 
;               pressure inputs. 6/10/2002
;    Fixed problem with vector input when /TO_OBSERVED set W. Landsman Dec 2005
;    Allow arrays with more than 32767 elements W.Landsman/C.Dickinson Feb 2010
;-
function co_refract_forward, a, P=P, T=T

; INPUTS
;    a = The observed (apparent) altitude, in DEGREES.
;        May be scalar or vector.
;
; INPUT KEYWORDS
;    P:  Pressure [in millibars]. Default is 1010 millibars. [scalar or vector]
;    T:  Ground Temp [in Celsius].  Default is 0 Celsius. [scalar or vector]

compile_opt idl2
d2r = !dpi/180.
if n_elements(P) eq 0 then P = 1010.
if n_elements(T) eq 0 then T = 283.

; you have observed the altitude a, and would like to know what the "apparent" 
; altitude is (the one behind the atmosphere).
w = where(a LT 15.)
R = 0.0166667/tan((a + 7.31/(a+4.4))*d2r)

;R = 1.02/tan((a + 10.3/(a+5.11))*d2r)/60. 
; this formula goes the other direction!

if w[0] ne -1 then R[w] = 3.569*(0.1594 + .0196*a[w] + $
      .00002*a[w]^2)/(1.+.505*a[w]+.0845*a[w]^2)
tpcor = P/1010. * 283/T
R = tpcor * R

return, R

END

function co_refract, a, altitude=altitude, pressure=pressure,  $
            temperature=temperature, To_observed=To_observed, epsilon=epsilon

; This is the main window.  Calls co_refract_forward either iteratively or a 
; single time depending on the direction we are going for refraction.

compile_opt idl2
o = keyword_set(To_observed)
alpha = 0.0065 ; temp lapse rate [deg C per meter]

if n_elements(altitude) eq 0 then altitude = 0.
if n_elements(temperature) eq 0 then begin
        if altitude GT 11000 then temperature = 211.5 $
                             else temperature = 283.0 - alpha*altitude
endif
; estimate Pressure based on altitude, using U.S. Standard Atmosphere formula.
if n_elements(pressure) eq 0 then $ 
              pressure = 1010.*(1-6.5/288000*altitude)^5.255
if n_elements(epsilon) eq 0 then  $
     epsilon = 0.25 ; accuracy of iteration for observed=1 case, in arcseconds

if not o then begin
        aout = a - co_refract_forward(a,P=pressure,T=temperature)
endif else begin
        aout = a*0.
        na = n_elements(a)
; if there are multiple elevations but only one temp and pressure entered, 
; expand those to be arrays of the same size.
	P = pressure + a*0. & T = temperature + a*0.
        for i=0L,na-1 do begin
                ;calculate initial refraction guess
                dr = co_refract_forward(a[i],P=P[i],T=T[i])
            cur = a[i] + dr ; guess of observed location

                repeat begin
                  last = cur
                  dr = co_refract_forward(cur,P=P[i],T=T[i])
                  cur= a[i] + dr
                endrep until (abs(last-cur)*3600. LT epsilon) || $
                             ~finite(last - cur)
                aout[i] = cur
        endfor
endelse

if N_elements(aout) GT 1 then return, reform(aout) else return, aout

END
