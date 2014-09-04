;+
; NAME:
;      MAG2GEO()
;
; PURPOSE:
;     Convert from geomagnetic to geographic coordinates
;
; EXPLANATION:
; 
;     Converts from GEOMAGNETIC (latitude,longitude) to GEOGRAPHIC (latitude,
;    longitude).    (altitude remains the same)
;
; CALLING SEQUENCE:
;       gcoord=mag2geo(mcoord)
;
; INPUT:
;       mcoord = a 2-element array of magnetic [latitude,longitude], or an 
;                array [2,n] of n such coordinates.
;
; KEYWORD INPUTS:
;               None
;
; OUTPUT:
;       a 2-element array of geographic [latitude,longitude], or an array [2,n]
;            of n such coordinates                   
;
; COMMON BLOCKS:
;               None
;
; EXAMPLES:
;       IDL> gcoord=mag2geo([90,0])       ; coordinates of magnetic south pole
;       IDL> print,gcoord
;       79.300000      -71.409990
;
; MODIFICATION HISTORY:
;       Written by Pascal Saint-Hilaire (Saint-Hilaire@astro.phys.ethz.ch), 
;        May 2002
;-
;====================================================================================
FUNCTION mag2geo,incoord

        ; SOME 'constants'...
        Dlong=288.59D   ; longitude (in degrees) of Earth's magnetic south pole
                        ; (which is near the geographic north pole!) (1995)
        Dlat=79.30D     ; latitude (in degrees) of same (1995)
        R = 1D          ; distance from planet center (value unimportant -- 
                ;just need a length for conversion to rectangular coordinates)

        ; convert first to radians
        Dlong=Dlong*!DPI/180.
        Dlat=Dlat*!DPI/180.

        mlat=DOUBLE(incoord[0,*])*!DPI/180.
        mlon=DOUBLE(incoord[1,*])*!DPI/180.
        malt=mlat * 0. + R
        
        coord=[mlat,mlon,malt]

        ;convert to rectangular coordinates
        ;       X-axis: defined by the vector going from Earth's center towards
        ;            the intersection of the equator and Greenwich's meridian.
        ;       Z-axis: axis of the geographic poles
        ;       Y-axis: defined by Y=Z^X
        x=coord[2,*]*cos(coord[0,*])*cos(coord[1,*])
        y=coord[2,*]*cos(coord[0,*])*sin(coord[1,*])
        z=coord[2,*]*sin(coord[0,*])

        ;First rotation : in the plane of the current meridian from magnetic 
        ;pole to geographic pole.
        togeolat=dblarr(3,3)
        togeolat[0,0]=cos(!DPI/2-Dlat)
        togeolat[0,2]=sin(!DPI/2-Dlat)
        togeolat[2,0]=-sin(!DPI/2-Dlat)
        togeolat[2,2]=cos(!DPI/2-Dlat)
        togeolat[1,1]=1.
        out= togeolat # [x,y,z]

        ;Second rotation matrix : rotation around plane of the equator, from
        ;the meridian containing the magnetic poles to the Greenwich meridian.
        maglong2geolong=dblarr(3,3)
        maglong2geolong[0,0]=cos(Dlong)
        maglong2geolong[0,1]=-sin(Dlong)
        maglong2geolong[1,0]=sin(Dlong)
        maglong2geolong[1,1]=cos(Dlong)
        maglong2geolong[2,2]=1.
        out=maglong2geolong # out

        ;convert back to latitude, longitude and altitude
        glat=atan(out[2,*],sqrt(out[0,*]^2+out[1,*]^2))
        glat=glat*180./!DPI
        glon=atan(out[1,*],out[0,*])
        glon=glon*180./!DPI
        ;galt=sqrt(out[0,*]^2+out[1,*]^2+out[2,*]^2)-R  ; I don't care about that one...just put it there for completeness' sake 

        RETURN,[glat,glon]
END
;====================================================================================
