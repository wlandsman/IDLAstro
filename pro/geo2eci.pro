;+
; NAME:
;     GEO2ECI
;
; PURPOSE:
;     Convert geographic spherical coordinates to Earth-centered inertial coords
;
; EXPLANATION:
;     Converts from geographic spherical coordinates [latitude, longitude, 
;     altitude] to ECI (Earth-Centered Inertial) [X,Y,Z] rectangular 
;     coordinates.    JD time is also needed.
;
;     Geographic coordinates are in degrees/degrees/km
;     Geographic coordinates assume the Earth is a perfect sphere, with radius 
;       equal to its equatorial radius.
;     ECI coordinates are in km from Earth center at epoch TOD (True of Date)
;
; CALLING SEQUENCE:
;       ECIcoord=geo2eci(gcoord,JDtime)
;
; INPUT:
;       gcoord: geographic [latitude,longitude,altitude], or a an array [3,n] 
;                of n such coordinates
;       JDtime: Julian Day time, double precision. Can be a 1-D array of n 
;               such times.
;
; KEYWORD INPUTS:
;       None
;
; OUTPUT:
;       a 3-element array of ECI [X,Y,Z] coordinates, or an array [3,n] of 
;                n such coordinates, double precision.    The TOD epoch is the 
;                supplied JDtime.   
;       
; COMMON BLOCKS:
;       None
;
; PROCEDURES USED:
;       CT2LST - Convert Local Civil Time to Local Mean Sidereal Time
;
; EXAMPLES:
;
;       IDL> ECIcoord=geo2eci([0,0,0], 2452343.38982663D)
;       IDL> print,ECIcoord
;      -3902.9606       5044.5548       0.0000000
;
;       (The above is the ECI coordinates of the intersection of the equator and
;       Greenwich's meridian on 2002/03/09 21:21:21.021)
;
; MODIFICATION HISTORY:
;       Written by Pascal Saint-Hilaire (shilaire@astro.phys.ethz.ch) 
;             on 2002/05/14
;       Update documentation to specify epoch is TOD.  
;                R. Redmon  NOAA/NGDC   April 2014
;               
;-

;====================================================================================
FUNCTION geo2eci,incoord,JDtim

        Re=6378.137     ; Earth's equatorial radius, in km

        lat = DOUBLE(incoord[0,*])*!DPI/180.
        lon = DOUBLE(incoord[1,*])*!DPI/180.
        alt = DOUBLE(incoord[2,*])
        JDtime= DOUBLE(JDtim)
        
        ct2lst,gst,0,0,JDtime
        angle_sid=gst*2.*!DPI/24.       ; sidereal angle

        theta=lon+angle_sid             ; azimuth
        r=(alt+Re)*cos(lat)
        X=r*cos(theta)
        Y=r*sin(theta)
        Z=(alt+Re)*sin(lat)
                
        RETURN,[X,Y,Z]
END
;====================================================================================
