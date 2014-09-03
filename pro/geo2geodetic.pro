;+
; NAME:
;       GEO2GEODETIC
;
; PURPOSE:
;       Convert from geographic/planetographic to geodetic coordinates
; EXPLANATION:
;       Converts from geographic (latitude, longitude, altitude) to geodetic
;       (latitude, longitude, altitude).  In geographic coordinates, the 
;           Earth is assumed a perfect sphere with a radius equal to its equatorial 
;               radius. The geodetic (or ellipsoidal) coordinate system takes into 
;               account the Earth's oblateness.
;
;       Geographic and geodetic longitudes are identical.
;               Geodetic latitude is the angle between local zenith and the equatorial plane.
;               Geographic and geodetic altitudes are both the closest distance between 
;               the satellite and the ground.
;
;       The PLANET keyword allows a similar transformation for the other 
;       planets  (planetographic to planetodetic coordinates). 
;
;       The EQUATORIAL_RADIUS and POLAR_RADIUS keywords allow the 
;       transformation for any ellipsoid.
;
;       Latitudes and longitudes are expressed in degrees, altitudes in km.
;
;       REF: Stephen P.  Keeler and Yves Nievergelt, "Computing geodetic
;       coordinates", SIAM Rev. Vol. 40, No. 2, pp. 300-309, June 1998
;
;       Planetary constants from "Allen's Astrophysical Quantities", 
;       Fourth Ed., (2000)
;
; CALLING SEQUENCE:
;       ecoord=geo2geodetic(gcoord,[ PLANET=,EQUATORIAL_RADIUS=, POLAR_RADIUS=])
;
; INPUT:
;       gcoord = a 3-element array of geographic [latitude,longitude,altitude],
;                or an array [3,n] of n such coordinates.
;
;
; OPTIONAL KEYWORD INPUT:
;       PLANET = keyword specifying planet (default is Earth).   The planet
;                may be specified either as an integer (1-9) or as one of the
;                (case-independent) strings 'mercury','venus','earth','mars',
;                'jupiter','saturn','uranus','neptune', or 'pluto'
;               
;       EQUATORIAL_RADIUS : Self-explanatory. In km. If not set, PLANET's 
;                value is used.
;       POLAR_RADIUS : Self-explanatory. In km. If not set, PLANET's value is 
;                used.
;
; OUTPUT:
;      a 3-element array of geodetic/planetodetic [latitude,longitude,altitude],
;        or an array [3,n] of n such coordinates, double precision.
;
; COMMON BLOCKS:
;       None
;
; RESTRICTIONS:
;
;       Whereas the conversion from geodetic to geographic coordinates is given
;       by an exact, analytical formula, the conversion from geographic to
;       geodetic isn't. Approximative iterations (as used here) exist, but tend 
;       to become less good with increasing eccentricity and altitude.
;       The formula used in this routine should give correct results within
;       six digits for all spatial locations, for an ellipsoid (planet) with
;       an eccentricity similar to or less than Earth's.
;       More accurate results can be obtained via calculus, needing a 
;       non-determined amount of iterations.
;       In any case, 
;          IDL> PRINT,geodetic2geo(geo2geodetic(gcoord)) - gcoord
;       is a pretty good way to evaluate the accuracy of geo2geodetic.pro.
;
; EXAMPLES:
;
;       Locate the geographic North pole, altitude 0., in geodetic coordinates
;       IDL> geo=[90.d0,0.d0,0.d0]  
;       IDL> geod=geo2geodetic(geo); convert to equivalent geodetic coordinates
;       IDL> PRINT,geod
;       90.000000       0.0000000       21.385000
;
;       As above, but for the case of Mars
;       IDL> geod=geo2geodetic(geo,PLANET='Mars')
;       IDL> PRINT,geod
;       90.000000       0.0000000       18.235500
;
; MODIFICATION HISTORY:
;       Written by Pascal Saint-Hilaire (shilaire@astro.phys.ethz.ch), May 2002
;       Generalized for all solar system planets by Robert L. Marcialis
;               (umpire@lpl.arizona.edu), May 2002
;       Modified 2002/05/18, PSH: added keywords EQUATORIAL_RADIUS and 
;               POLAR_RADIUS
;-

;================================================================================
FUNCTION geo2geodetic,gcoord,PLANET=planet, $
        EQUATORIAL_RADIUS=equatorial_radius, POLAR_RADIUS=polar_radius

 sz_gcoord = size(gcoord,/DIMEN)
 if sz_gcoord[0] LT 3 then message, $
    'ERROR - 3 coordinates (latitude,longitude,altitude) must be specified'

 if N_elements(PLANET) GT 0  then begin
        if size(planet,/tname) EQ 'STRING' then begin 
        choose_planet=['mercury','venus','earth','mars','jupiter','saturn', $
                       'uranus','neptune','pluto']
        index=where(choose_planet eq strlowcase(planet))
        index=index[0]  ; make it a scalar
        if index eq -1 then index = 2   ; default is Earth
        endif else index = planet-1
 endif else index=2 

        Requator = [2439.7d0,6051.8d0,6378.137D, 3397.62d0,  71492d0, $
                 60268.d0,      25559.d0,    24764.d0,    1195.d0]
        Rpole = [2439.7d0, 6051.8d0, 6356.752d0, 3379.3845d0, 67136.5562d0, $
                 54890.7686d0, 24986.1354d0, 24347.6551d0, 1195.d0]
        Re = Requator[index]            ; equatorial radius
        Rp = Rpole[index]                    ; polar radius
        
        IF KEYWORD_SET(EQUATORIAL_RADIUS) THEN Re=DOUBLE(equatorial_radius[0])
        IF KEYWORD_SET(POLAR_RADIUS) THEN Rp=DOUBLE(polar_radius[0])
                
                e = sqrt(Re^2 - Rp^2)/Re
                ;f=1/298.257D   ; flattening = (Re-Rp)/Re  [not needed, here]

        glat=DOUBLE(gcoord[0,*])*!DPI/180.
        glon=DOUBLE(gcoord[1,*])
        galt=DOUBLE(gcoord[2,*])

        x= (Re+galt) * cos(glat) * cos(glon)
        y= (Re+galt) * cos(glat) * sin(glon)
        z= (Re+galt) * sin(glat)
        r=sqrt(x^2+y^2)

                s=(r^2 + z ^2)^0.5 * (1 - Re*((1-e^2)/((1-e^2)*r^2 + z^2))^0.5)
                t0=1+s*(1- (e*z)^2/(r^2 + z^2) )^0.5 /Re
                dzeta1=z * t0
                xi1=r*(t0 - e^2)
                rho1= (xi1^2 + dzeta1^2)^0.5
                c1=xi1/rho1
                s1=dzeta1/rho1
                b1=Re/(1- (e*s1)^2)^0.5
                u1= b1*c1
                w1= b1*s1*(1- e^2)
                ealt= ((r - u1)^2 + (z - w1)^2)^0.5
                elat= atan(s1,c1)

        elat=elat*180./!DPI
        elon=glon

        RETURN,[elat,elon,ealt]
END
;===============================================================================
