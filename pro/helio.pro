PRO HELIO, JD, LIST, HRAD, HLONG, HLAT, RADIAN = radian
;+
; NAME: 
;      HELIO
; PURPOSE: 
;      Compute (low-precision) heliocentric coordinates for the planets.
; EXPLANATION:
;      The mean orbital elements for epoch J2000 are used.   These are derived
;      from a 250 yr least squares fit of the DE 200 planetary ephemeris to a 
;      Keplerian orbit where each element is allowed to vary linearly with 
;      time.  For dates between 1800 and 2050, this solution fits the 
;      terrestrial planet orbits to ~25" or better, but achieves only ~600" 
;      for Saturn.   
;
;      Use PLANET_COORDS (which calls HELIO) to get celestial (RA, Dec)
;      coordinates of the planets
; CALLING SEQUENCE: 
;       HELIO, JD, LIST, HRAD, HLONG, HLAT, [/RADIAN]
; INPUTS:
;       JD = Julian date, double precision scalar or vector
;       LIST = List of planets array.  May be a single number.
;               1 = merc, 2 = venus, ... 9 = pluto.
;
; OUTPUTS:
;       HRAD = array of Heliocentric radii (A.U).
;       HLONG = array of Heliocentric (ecliptic) longitudes (degrees).
;       HLAT = array of Heliocentric latitudes (degrees).
;             These output parameters will be dimensioned Nplanet by Ndate,
;             where Nplanet is the number of elements of list, and Ndate is 
;             the number of elements of JD.
;
; OPTIONAL INPUT KEYWORD:
;       /RADIAN - If set, then the output longitude and latitude are given in 
;                 radians.         
; EXAMPLE:
;       (1) Find the current heliocentric positions of all the planets
;
;        IDL> GET_JULDATE, jd      ;Get current Julian date
;        IDL> HELIO,jd,indgen(9)+1,hrad,hlong,hlat  ;Get radius, long, and lat
;
;       (2) Find heliocentric position of Mars on August 23, 2018 
;         IDL> JDCNV, 2018,08,23,0,jd
;         IDL> HELIO,JD,4,HRAD,HLONG,HLAT
;                  ===> hrad = 1.6407 AU hlong = 124.3197 hlat = 1.7853
;         For comparison, the JPL ephemeris gives
;                       hrad = 1.6407 AU hlong = 124.2985 hlat = 1.7845
;       (3) Find the heliocentric positions of Mars and Venus for every day in
;           November 2018
;        IDL> JDCNV, 2018, 11, 1, 0, jd    ;Julian date of November 1, 2000
;        IDL> helio, jd+indgen(30), [4,2], hrad,hlong,hlat   ;Mars=4, Venus=2 
;                   hrad, hlong, and hlat will be dimensioned [2,30]
;                   first column contains Mars data, second column Venus
; COMMON BLOCKS: 
;       None 
; ROUTINES USED: 
;       CIRRANGE - force angle between 0 and 2*!PI
; NOTES:
;       (1) The calling sequence for this procedure was changed in August 2000
;       (2) This program is based on the two-body model and thus neglects 
;           interactions between the planets.   This is why the worst results
;           are for Saturn.  Use the procedure JPLEPHINTERP for more accurate
;           positions using the JPL ephemeris.   Also see JPL Horizons
;           ( https://ssd.jpl.nasa.gov/horizons.cgi ) for a more accurate ephemeris 
;           generator online.     
;       (3) The coordinates are given for equinox 2000 and *not* the equinox
;           of the supplied date(s)
; MODIFICATION HISTORY: 
;       R. Sterner.  20 Aug, 1986.
;       Code cleaned up a bit      W. Landsman             December 1992
;       Major rewrite, use modern orbital elements, vectorize, more accurate
;         solution to Kepler's equation          W. Landsman August 2000
;       Wasn't working for planet vectors        W. Landsman August 2000
;       Work for more than 32767 positions       S. Leach Jan 2009
;       Use data from https://ssd.jpl.nasa.gov/txt/p_elem_t1.txt   W. Landsman Jun 2017
;-
 On_error,2
 compile_opt idl2

   if N_params() LT 3 then begin
     print,'Syntax - Helio, jd, list, hrad, hlong, hlat, [/RADIAN]'
     print,'     jd - Scalar or vector Julian date'
     print,'     list - scalar or vector of planet numbers [1-9]'
     print, $
     '     hrad, hlong, hlat - output heliocentric distance, longitude latitude'    
     return
  endif

; Mean orbital elements taken from https://ssd.jpl.nasa.gov/txt/p_elem_t1.txt
; (1) semi-major axis in AU, (2) eccentricity, (3) inclination (degrees),
; (4) mean longitude (degrees), (5) longitude of perihelion (degrees)
; and (6) longitude of the ascending node (degrees)

;Mercury   
PD = [ [0.38709927d, 0.20563593d, 7.00497902d, 252.25032350d, 77.45779628d , 48.33076593d], $
;Venus     
[ 0.72333566d, 0.00677672d, 3.39467605d, 181.97909950d, 131.60246718d, 76.67984255d], $
;EMBary   
[ 1.00000261d, 0.01671123d, -0.00001531d, 100.46457166d, 102.93768193d,  0.0], $
;Mars 
[ 1.52371034d, 0.09339410d, 1.84969142d, -4.55343205d, -23.94362959d, 49.55953891d], $
;Jupiter
[ 5.20288700d, 0.04838624d, 1.30439695d, 34.39644051d, 14.72847983d, 100.47390909d], $
;Saturn
[ 9.53667594d, 0.05386179d, 2.48599187d, 49.95424423d, 92.59887831d, 113.66242448d], $
;Uranus
[ 19.18916464d, 0.04725744d, 0.77263783d, 313.23810451d, 170.95427630d, 74.01692503d], $
;Neptune
[ 30.06992276d, 0.00859048d, 1.77004347d,  -55.12002969d, 44.96476227d, 131.78422574d], $
;Pluto
[ 39.48211675d, 0.24882730d, 17.14001206d, 238.92903833d, 224.06891629d, 110.30393684d]]
        

 DPD = [[ 0.00000037d, 0.00001906d, -0.00594749d, 149472.67411175d, 0.16047689d, -0.12534081 ], $
         [0.00000390d, -0.00004107d, -0.00078890d,  58517.81538729d, 0.00268329d, -0.27769418 ], $
          [0.00000562d, -0.00004392d, -0.01294668d,  35999.37244981d, 0.32327364d,      0.0d], $
        [ 0.00001847d,  0.00007882d, -0.00813131d, 19140.30268499d,  0.44441088d, -0.29257343d], $
        [ -0.00011607d, -0.00013253d, -0.00183714d, 3034.74612775d, 0.21252668d, 0.20469106d],$
        [  -0.00125060d, -0.00050991d, 0.00193609d, 1222.49362201d, -0.41897216d, -0.28867794d], $
        [  -0.00196176d, -0.00004397d, -0.00242939d, 428.48202785d,  0.40805281d, 0.04240589d], $
        [   0.00026291d,  0.00005105d,  0.00035372d, 218.45945325d, -0.32241464d, -0.00508664d], $
        [ -0.00031596d,   0.00005170d,  0.00004818d, 145.20780515d, -0.04062942d,  -0.01183482d]]



 JD0 = 2451545.0d    ;Julian Date for Epoch 2000.0
 radeg = 180/!DPI

;-----------------  Days since Epoch  ---------------

  T = (JD - JD0)/36525.0d          ;Time in centuries since 2000.0
 

        ip = list-1
        ntime = N_elements(t)
        nplanet = N_elements(list)
        hrad = fltarr(nplanet,ntime) & hlong = hrad & hlat = hrad

;-----------------  Loop over dates  --------------
 
        for i =0L,ntime-1L do begin         ;SML made longword
 
        pd1 = pd[*,ip] + dpd[*,ip]*T[i]
        
        a = pd1[0,*]                            ;semi-major axis
        eccen = pd1[1,*]                        ;eccentricity
        inc = pd1[2,*]/RADEG          ;inclination in radians
        L =  pd1[3,*]/RADEG                     ;mean longitude
        pi = pd1[4,*]/RADEG                  ;longitude of the perihelion
        omega = pd1[5,*]/RADEG               ;longitude of the ascending node
    
        
        m = L - pi
        cirrange,m,/RADIAN
        e1 = m + (m + eccen*sin(m) - m)/(1 - eccen*cos(m) )
        e = e1 + (m + eccen*sin(e1) - e1)/(1 - eccen*cos(e1) )
        maxdif = max(abs(e-e1))
        niter = 0
        while (maxdif GT 1e-7) && (niter lt 10) do begin        
             e1 = e
             e = e1 + (m + eccen*sin(e1) - e1)/(1 - eccen*cos(e1) )
             maxdif = max(abs(e-e1))
             niter++
        endwhile       
        
      
        nu = 2*atan( sqrt( (1+eccen)/(1-eccen) )* tan(E/2))   ;true anomaly 

        hrad[0,i] = reform( a*(1 - eccen*cos(e) ) )
        hlong[0,i] = reform (nu + pi)                 
         hlat[0,i] = reform( asin(sin(hlong[*,i] - omega)*sin(inc) ) )
 endfor 

       cirrange,hlong,/RADIAN
       if ~keyword_set(RADIAN) then begin 
           hlong = hlong*RADEG
           hlat = hlat*RADEG
       endif
       if N_elements(hrad) GT 1 then begin
             hrad = reform(hrad,/over)
             hlong = reform(hlong,/over)
             hlat = reform(hlat,/over)
       endif else begin
             if N_elements(size(jd)) EQ 3 then begin      ;scalar?
                     hrad = hrad[0]
                     hlong = hlong[0]
                     hlat = hlat[0]
              endif
       endelse 

   return
   end
