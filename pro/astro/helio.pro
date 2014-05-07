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
;       (2) Find heliocentric position of Mars on August 23, 2000 
;         IDL> JDCNV, 2000,08,23,0,jd
;         IDL> HELIO,JD,2,HRAD,HLONG,HLAT
;                  ===> hrad = 1.6407 AU hlong = 124.3197 hlat = 1.7853
;         For comparison, the JPL ephemeris gives
;                       hrad = 1.6407 AU hlong = 124.2985 hlat = 1.7845
;       (3) Find the heliocentric positions of Mars and Venus for every day in
;           November 2000
;        IDL> JDCNV, 2000, 11, 1, 0, jd    ;Julian date of November 1, 2000
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
;           are for Saturn.  Use the procedure JPLEPHINTERp for more accurate
;           positions using the JPL ephemeris.   Also see 
;           http://ssd.jpl.nasa.gov/cgi-bin/eph for a more accurate ephemeris 
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

; Mean orbital elements taken from http://ssd.jpl.nasa.gov/elem_planets.html
; (1) semi-major axis in AU, (2) eccentricity, (3) inclination (degrees),
; (4) longitude of the ascending node (degrees), (5) longitude of perihelion
; (degrees) and (6) mean longitude (degrees)
;Mercury 
PD = [ [ 0.38709893d, 0.20563069, 7.00487,  48.33167,  77.45645, 252.25084 ], $
;Venus  
     [ 0.72333199d, 0.00677323, 3.39471,  76.68069, 131.53298, 181.97973 ], $ 
;Earth
     [ 1.00000011d, 0.01671022, 0.00005, -11.26064, 102.94719, 100.46435], $
;Mars 
     [ 1.52366231d, 0.09341233, 1.85061,  49.57854, 336.04084, 355.45332], $
;Jupiter
     [ 5.20336301d, 0.04839266, 1.30530, 100.55615,  14.75385,  34.40438], $ 
;Saturn
     [ 9.53707032d, 0.05415060, 2.48446, 113.71504,  92.43194,  49.94432], $
;Uranus
     [19.19126393d, 0.04716771, 0.76986,  74.22988, 170.96424, 313.23218], $ 
;Neptune
     [30.06896348d, 0.00858587, 1.76917, 131.72169,  44.97135, 304.88003], $
;Pluto
     [39.48168677d, 0.24880766,17.14175, 110.30347, 224.06676, 238.92881] ]

; DPD gives the time rate of change of the above quantities ("/century)

DPD = [  [0.00000066d, 0.00002527, -23.51, -446.30, 573.57, 538101628.29 ], $
 [ 0.00000092d, -0.00004938, -2.86, -996.89, -108.80, 210664136.06], $
 [-0.00000005d, -0.00003804, -46.94, -18228.25, 1198.28, 129597740.63], $ 
 [-0.00007221d, 0.00011902, -25.47, -1020.19, 1560.78, 68905103.78 ], $
 [0.00060737d, -0.00012880, -4.15, 1217.17, 839.93, 10925078.35 ], $
 [-0.00301530d, -0.00036762, 6.11, -1591.05, -1948.89, 4401052.95],  $
 [0.00152025d, -0.00019150, -2.09, -1681.40, 1312.56, 1542547.79 ], $
 [-0.00125196d, 0.0000251, -3.64, -151.25, -844.43, 786449.21 ], $
 [-0.00076912d, 0.00006465, 11.07, -37.33, -132.25, 522747.90] ] 

 JD0 = 2451545.0d    ;Julian Date for Epoch 2000.0
 radeg = 180/!DPI

;-----------------  Days since Epoch  ---------------

  T = (JD - JD0)/36525.0d          ;Time in centuries since 2000.0
 

        ip = list-1
        dpd[2:5,ip] = dpd[2:5,ip]/3600.0d       ;Convert arc seconds to degrees
        ntime = N_elements(t)
        nplanet = N_elements(list)
        hrad = fltarr(nplanet,ntime) & hlong = hrad & hlat = hrad

;-----------------  Loop over dates  --------------
 
        for i =0L,ntime-1L do begin         ;SML made longword
 
        pd1 = pd[*,ip] + dpd[*,ip]*T[i]
        
        a = pd1[0,*]                            ;semi-major axis
        eccen = pd1[1,*]                        ;eccentricity
        n = 0.9856076686/a/sqrt(a)/RADEG      ;mean motion, in radians/day
        L =  pd1[5,*]/RADEG                     ;mean longitude
        pi = pd1[4,*]/RADEG                  ;longitude of the perihelion
        omega = pd1[3,*]/RADEG               ;longitude of the ascending node
        inc = pd1[2,*]/RADEG          ;inclination in radians
        
        m = L - pi
        cirrange,m,/RADIAN
        e1 = m + (m + eccen*sin(m) - m)/(1 - eccen*cos(m) )
        e = e1 + (m + eccen*sin(e1) - e1)/(1 - eccen*cos(e1) )
        maxdif = max(abs(e-e1))
        niter = 0
        while (maxdif GT 1e-5) and (niter lt 10) do begin        
             e1 = e
             e = e1 + (m + eccen*sin(e1) - e1)/(1 - eccen*cos(e1) )
             maxdif = max(abs(e-e1))
             niter = niter+1
        endwhile       
        
      
        nu = 2*atan( sqrt( (1+eccen)/(1-eccen) )* tan(E/2))   ;true anomaly 

        hrad[0,i] = reform( a*(1 - eccen*cos(e) ) )
        hlong[0,i] = reform (nu + pi)                 
         hlat[0,i] = reform( asin(sin(hlong[*,i] - omega)*sin(inc) ) )
 endfor 

       cirrange,hlong,/RADIAN
       if not keyword_set(RADIAN) then begin 
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
