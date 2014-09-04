;+
; NAME:
;     ECI2GEO
;
; PURPOSE:
;     Convert Earth-centered inertial coordinates to geographic spherical coords
; EXPLANATION:
;     Converts from ECI (Earth-Centered Inertial) (X,Y,Z) rectangular 
;     coordinates to geographic spherical coordinates (latitude, longitude, 
;     altitude).    JD time is also needed as input.
;
;     ECI coordinates are in km from Earth center at the supplied time (True of
;     Date).     Geographic coordinates are in degrees/degrees/km
;     Geographic coordinates assume the Earth is a perfect sphere, with radius 
;     equal to its equatorial radius.
;
; CALLING SEQUENCE:
;     gcoord=eci2geo(ECI_XYZ,JDtime)
;
; INPUT:
;       ECI_XYZ : the ECI [X,Y,Z] coordinates (in km), can be an array [3,n] 
;                 of n such coordinates.    These should be at the supplied 
;                 Julian Date (TOD - true of date).
;       JDtime: the Julian Day time, double precision. Can be a 1-D array of n 
;                 such times.
;
; KEYWORD INPUTS:
;       None
;
; OUTPUT:
;       a 3-element array of geographic [latitude,longitude,altitude], or an 
;         array [3,n] of n such coordinates, double precision  
;
; COMMON BLOCKS:
;       None
;
; PROCEDURES USED:
;       CT2LST - Convert Local Civil Time to Local Mean Sidereal Time
;
; EXAMPLE:
;       IDL> gcoord=eci2geo([6378.137+600,0,0], 2452343.38982663D)
;       IDL> print,gcoord
;       0.0000000       232.27096       600.00000
;
;       (The above is the geographic direction of the vernal point on 
;       2002/03/09 21:21:21.021, in geographic coordinates. The chosen 
;       altitude was 600 km.)
;
;       gcoord can be further transformed into geodetic coordinates (using 
;       geo2geodetic.pro) or into geomagnetic coordinates (using geo2mag.pro)
;
; MODIFICATION HISTORY:
;       Written by Pascal Saint-Hilaire (Saint-Hilaire@astro.phys.ethz.ch) on 
;              2001/05/13
;       Modified on 2002/05/13, PSH : vectorization + use of JD times  
;       Document use of TOD epoch R. Redmon  April 2014 NOAA/NGDC     
;-

;=============================================================================
FUNCTION eci2geo,ECI_XYZ,JDtim

        Re=6378.137     ; Earth's equatorial radius, in km
        coord=DOUBLE(ECI_XYZ)
        JDtime= DOUBLE(JDtim)

        theta=atan(coord[1,*],coord[0,*])       ; azimuth       
        ct2lst,gst,0,0,JDtime
        angle_sid=gst*2.*!DPI/24.        ; sidereal angle
        lon= (theta - angle_sid ) MOD (2* !DPI)                  ;longitude      
        r=sqrt(coord[0,*]^2+coord[1,*]^2)
        lat=atan(coord[2,*],r)                                  ; latitude
        alt=r/cos(lat) - Re                                     ; altitude 

        lat=lat*180./(!DPI)      ; to convert from radians into degrees...
        lon=lon*180./(!DPI)
        ss=WHERE(lon LT 0.) 
        IF ss[0] NE -1 THEN lon[ss]=lon[ss]+360.
        
        RETURN,[lat,lon,alt]
END
;====================================================================================
