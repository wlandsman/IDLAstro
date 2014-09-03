;+
; NAME:
;       GEODETIC2GEO
;
; PURPOSE:
;       Convert from geodetic (or planetodetic) to geographic coordinates
; EXPLANATION:
;       Converts from geodetic (latitude, longitude, altitude) to geographic
;       (latitude, longitude, altitude).  In geographic coordinates, the 
;       Earth is assumed a perfect sphere with a radius equal to its equatorial 
;       radius. The geodetic (or ellipsoidal) coordinate system takes into 
;       account the Earth's oblateness.
;
;       Geographic and geodetic longitudes are identical.
;       Geodetic latitude is the angle between local zenith and the equatorial 
;       plane.   Geographic and geodetic altitudes are both the closest distance
;       between the satellite and the ground.
;
;       The PLANET keyword allows a similar transformation for the other 
;       planets  (planetodetic to planetographic coordinates). 
;
;       The EQUATORIAL_RADIUS and POLAR_RADIUS keywords allow the 
;       transformation for any ellipsoid.
;
;       Latitudes and longitudes are expressed in degrees, altitudes in km.
;
;       REF: Stephen P.  Keeler and Yves Nievergelt, "Computing geodetic
;       coordinates", SIAM Rev. Vol. 40, No. 2, pp. 300-309, June 1998
;       Planetary constants from "Allen's Astrophysical Quantities", 
;       Fourth Ed., (2000)
;
; CALLING SEQUENCE:
;       gcoord = geodetic2geo(ecoord, [ PLANET= ] )
;
; INPUT:
;       ecoord = a 3-element array of geodetic [latitude,longitude,altitude],
;                or an array [3,n] of n such coordinates.
;
; OPTIONAL KEYWORD INPUT:
;       PLANET = keyword specifying planet (default is Earth).   The planet
;                may be specified either as an integer (1-9) or as one of the
;                (case-independent) strings 'mercury','venus','earth','mars',
;                'jupiter','saturn','uranus','neptune', or 'pluto'
;
;       EQUATORIAL_RADIUS : Self-explanatory. In km. If not set, PLANET's value
;                is used.   Numeric scalar
;       POLAR_RADIUS : Self-explanatory. In km. If not set, PLANET's value is 
;                 used.   Numeric scalar
;
; OUTPUT:
;       a 3-element array of geographic [latitude,longitude,altitude], or an
;         array [3,n] of n such coordinates, double precision
;
;       The geographic and geodetic longitudes will be identical.
; COMMON BLOCKS:
;       None
;
; EXAMPLES:
;
;       IDL> geod=[90,0,0]  ; North pole, altitude 0., in geodetic coordinates
;       IDL> geo=geodetic2geo(geod)
;       IDL> PRINT,geo
;       90.000000       0.0000000      -21.385000
;
;       As above, but the equivalent planetographic coordinates for Mars
;       IDL> geod=geodetic2geo(geod,PLANET='Mars'); 
;       IDL> PRINT,geod
;       90.000000       0.0000000      -18.235500
;
; MODIFICATION HISTORY:
;       Written by Pascal Saint-Hilaire (shilaire@astro.phys.ethz.ch),
;                  May 2002
;
;       Generalized for all solar system planets by Robert L. Marcialis
;               (umpire@lpl.arizona.edu), May 2002
;
;       Modified 2002/05/18, PSH: added keywords EQUATORIAL_RADIUS and 
;                POLAR_RADIUS
;
;-
;===================================================================================
FUNCTION geodetic2geo,ecoord,PLANET=planet,     $
        EQUATORIAL_RADIUS=equatorial_radius, POLAR_RADIUS=polar_radius

 sz_ecoord = size(ecoord,/DIMEN)
 if sz_ecoord[0] LT 3 then message, $
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
                ;f=1/298.257D   ; flattening = (Re-Rp)/Re
        Re = Requator(index)            ; equatorial radius
        Rp = Rpole(index)                       ; polar radius

        IF KEYWORD_SET(EQUATORIAL_RADIUS) THEN Re=DOUBLE(equatorial_radius[0])
        IF KEYWORD_SET(POLAR_RADIUS) THEN Rp=DOUBLE(polar_radius[0])

        e = sqrt(Re^2 - Rp^2)/Re
        elat = DOUBLE(ecoord[0,*])*!DPI/180.
        elon = DOUBLE(ecoord[1,*])
        ealt = DOUBLE(ecoord[2,*])

        beta=sqrt(1-(e*sin(elat))^2)
        r=(Re/beta + ealt)*cos(elat)
        z=(Re*(1-e^2)/beta + ealt)*sin(elat)

        glat=atan(z,r)*180./!DPI
        glon=elon
        galt=sqrt(r^2+z^2) - Re

        RETURN,[glat,glon,galt]
END
;===================================================================================
