;+
; NAME:
;       GEO2MAG()
;
; PURPOSE:
;       Convert from geographic to geomagnetic coordinates
; EXPLANATION:
;       Converts from GEOGRAPHIC (latitude,longitude) to GEOMAGNETIC (latitude, 
;       longitude).   (Altitude remains the same)
;
;       Latitudes and longitudes are expressed in degrees.
;
; CALLING SEQUENCE:
;       mcoord=geo2mag(gcoord)
;
; INPUT:
;       gcoord = a 2-element array of geographic [latitude,longitude], or an 
;                array [2,n] of n such coordinates.
;
; KEYWORD INPUTS:
;       None
;
; OUTPUT:
;       a 2-element array of magnetic [latitude,longitude], or an array [2,n] 
;         of n such coordinates                     
;
; COMMON BLOCKS:
;       None
;
; EXAMPLES:
;       geographic coordinates of magnetic south pole
;
;       IDL> mcoord=geo2mag([79.3,288.59])      
;       IDL> print,mcoord
;       89.999992      -173.02325
;
; MODIFICATION HISTORY:
;       Written by Pascal Saint-Hilaire (Saint-Hilaire@astro.phys.ethz.ch), 
;            May 2002
;               
;-

;====================================================================================
FUNCTION geo2mag,incoord

        ; SOME 'constants'...
        Dlong=288.59D   ; longitude (in degrees) of Earth's magnetic south pole
                        ;(which is near the geographic north pole!) (1995)
        Dlat=79.30D     ; latitude (in degrees) of same (1995)
        R = 1D          ; distance from planet center (value unimportant -- 
                 ;just need a length for conversion to rectangular coordinates)

        ; convert first to radians
        Dlong=Dlong*!DPI/180.
        Dlat=Dlat*!DPI/180.

        glat=DOUBLE(incoord[0,*])*!DPI/180.
        glon=DOUBLE(incoord[1,*])*!DPI/180.
        galt=glat * 0. + R
        
        coord=[glat,glon,galt]

        ;convert to rectangular coordinates
        ;       X-axis: defined by the vector going from Earth's center towards
        ;            the intersection of the equator and Greenwitch's meridian.
        ;       Z-axis: axis of the geographic poles
        ;       Y-axis: defined by Y=Z^X
        x=coord[2,*]*cos(coord[0,*])*cos(coord[1,*])
        y=coord[2,*]*cos(coord[0,*])*sin(coord[1,*])
        z=coord[2,*]*sin(coord[0,*])

        ;Compute 1st rotation matrix : rotation around plane of the equator,
        ;from the Greenwich meridian to the meridian containing the magnetic
        ;dipole pole.
        geolong2maglong=dblarr(3,3)
        geolong2maglong[0,0]=cos(Dlong)
        geolong2maglong[0,1]=sin(Dlong)
        geolong2maglong[1,0]=-sin(Dlong)
        geolong2maglong[1,1]=cos(Dlong)
        geolong2maglong[2,2]=1.
        out=geolong2maglong # [x,y,z]

        ;Second rotation : in the plane of the current meridian from geographic
        ;                  pole to magnetic dipole pole.
        tomaglat=dblarr(3,3)
        tomaglat[0,0]=cos(!DPI/2-Dlat)
        tomaglat[0,2]=-sin(!DPI/2-Dlat)
        tomaglat[2,0]=sin(!DPI/2-Dlat)
        tomaglat[2,2]=cos(!DPI/2-Dlat)
        tomaglat[1,1]=1.
        out= tomaglat # out

        ;convert back to latitude, longitude and altitude
        mlat=atan(out[2,*],sqrt(out[0,*]^2+out[1,*]^2))
        mlat=mlat*180./!DPI
        mlon=atan(out[1,*],out[0,*])
        mlon=mlon*180./!DPI
        ;malt=sqrt(out[0,*]^2+out[1,*]^2+out[2,*]^2)-R 
;  I don't care about that one...just put it there for completeness' sake

        RETURN,[mlat,mlon]
END
;===============================================================================
