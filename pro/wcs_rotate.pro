;+
; NAME:
;       WCS_ROTATE 
;
; PURPOSE:
;       Rotate between standard (e.g. celestial) and native coordinates
; EXPLANATION:
;       Computes a spherical coordinate rotation between native coordinates 
;       and  standard celestial coordinate system (celestial, Galactic, or
;       ecliptic).   Applies the equations in Appendix B of the paper 
;       "Representation of Celestial Coordinates in FITS" by Calabretta 
;       Greisen (2002, A&A, 395, 1077).    Also see 
;       http://fits.gsfc.nasa.gov/fits_wcs.html
;
; CATEGORY:
;       Mapping and Auxiliary FITS Routine
;
; CALLING SEQUENCE:
;       WCS_ROTATE, longitude, latitude, phi, theta, crval, theta0 = 
;               [LONGPOLE = , LATPOLE = , PV1 = , /REVERSE, /ORIGIN ]
;
; INPUT PARAMETERS:
;       crval - 2 element vector containing standard system coordinates (the 
;               longitude and latitude) of the reference point
;
; INPUT OR OUTPUT PARAMETERS
;       longitude - longitude of data, scalar or vector, in degrees, in the
;               standard celestial coordinate system
;       latitude - latitude of data, same number of elements as longitude, 
;               in degrees
;       theta - latitude of data in the native system, in degrees, scalar or
;               vector
;
;       If the keyword(REVERSE) is set then phi and theta are input parameters
;       and longitude and latitude are computed.    Otherwise, longitude and
;       latitude are input parameters and phi and theta are computed.
;
; OPTIONAL KEYWORD INPUT PARAMETERS:
;
;       THETA0   - Native latitude of the reference point (required unless PV1 set)
;       PV1      - Vector giving parameters of user-defined fiducial point
;       LONGPOLE - native longitude of standard system's North Pole
;       LATPOLE -  native latitude of the standard system's North Pole
;       /REVERSE - if set then phi and theta are input parameters and longitude
;                  and latitude are computed.    By default, longitude and
;                  latitude are input parameters and phi and theta are computed.
;
;      /ORIGIN     This keyword is obsolete and is no longer used. Replaced by
;                  explicitly specifying theta0 and/or PV1
;
; REVISION HISTORY:
;       Written    W. Landsman               December, 1994
;       Fixed error in finding North Pole if /ORIGIN and LONGPOLE NE 180
;       Xiaoyi Wu and W. Landsman,   March, 1996
;       Fixed implementation of March 96 error, J. Thieler,  April 1996
;       Updated to IDL V5.0   W. Landsman    December 1997
;       Fixed determination of alpha_p if /ORIGIN and LONGPOLE EQ 180
;               W. Landsman    May 1998
;       Ensure argument of ASIN() is -1<x<-1 after roundoff 
;               W. Landsman/R. Arendt  June 2002
;       Call WCS_GETPOLE, accept LATPOLE keyword, update cylindrical coords
;               W. Landsman  June 2003 
;       Don't attempt to rotate NaN values   W. Landsman  May 2004
;       at some unknown time theta0 introduced
;       Traps put in to detect no rotation and avoid rounding errors for
;       common special cases. PV1 introduced. Comments updated & corrected.
;                                            J. P. Leahy July 2013.
;       Avoid roundoff error when longitude = +/- 180 W. Landsman Dec 2013 
;       
;-

pro wcs_rotate, longitude, latitude, phi, theta, crval, LONGPOLE = longpole, $
          LATPOLE = latpole, REVERSE=reverse, ORIGIN = origin, $
          PV1 = PV1, THETA0 = theta0

 COMPILE_OPT idl2, hidden
 
; check to see that enough parameters (at least 4) were sent
 if (N_params() lt 5 || N_Elements(theta0) ne 1) then begin
    print,'Syntax - WCS_ROTATE, longitude, latitude, phi, theta, crval, PV1 = '
    print,'               THETA0 = <scalar>, LATPOLE =,  LONGPOLE = , /REVERSE' 
    return
 endif 

 ; DEFINE ANGLE CONSTANTS 
 pi = !DPI
 pi2 = pi/2.d0
 radeg = 1.8d2/pi
 twopi = !dpi+!dpi

 if keyword_set( REVERSE) then begin
        if min([ N_elements(phi), N_elements(theta) ]) EQ 0 then          $
        message, 'ERROR - Native Coordinates (phi,theta) not defined'    
 endif else begin
        if min([ N_elements(longitude), N_elements(latitude) ]) EQ 0 then $ 
        message, 'ERROR - Celestial Coordinates (long,lat) not defined' 
 endelse

 IF N_ELEMENTS(pv1) GT 0 THEN BEGIN ; User-specified fiducial point
    IF N_ELEMENTS(pv1) NE 5 THEN $
        MESSAGE, 'ERROR:- PV1 array should contain five values if specified'
    phi0 = pv1[1]
    theta0 = pv1[2]
    longpole = pv1[3]
    latpole = pv1[4]
 ENDIF ELSE BEGIN
    IF N_elements(theta0) NE 1 THEN $
        MESSAGE, 'ERROR: Either PV1 or THETA0 must be set'
    IF N_elements(longpole) eq 0 THEN longpole = crval[1] ge theta0 ? 0d0 : 1.8d2
    IF N_elements(latpole)  eq 0 THEN latpole  = 90d0
    phi0 = 0
 ENDELSE
; Longpole is the longitude in the native system of the North Pole in the
; standard system.

 phi_p = double(longpole)/radeg
 IF longpole EQ 180d0 THEN BEGIN ; Check for special case to avoid roundoff
     sp =  0d0
     cp = -1d0
 ENDIF ELSE BEGIN
     sp = sin(phi_p)
     cp = cos(phi_p)
 ENDELSE

; CRVAL give the celestial coordinates of the fiducial point (phi0, theta0)
; in the native system.   This must be converted (using Eqs 8-10 in Greisen & 
; Calabretta with theta0 = 0) to give the coordinates of the North pole 
; (alpha_p, delta_p)
 if theta0 EQ 90 then begin
    ; Easy case: fiducial point is native pole:
     alpha_p = double(crval[0]) / radeg
     delta_p = double(crval[1]) / radeg
     at_pole = crval[1] EQ 90d0
 endif else WCS_GETPOLE, crval, longpole-phi0, theta0, alpha_p, delta_p, $
                 LATPOLE = latpole, AT_POLE = at_pole
 
 IF at_pole && ABS((alpha_p - phi_p) mod twopi) EQ !dpi THEN BEGIN
     ; Native and celestial frames coincide
     ; No rotation needed
     IF KEYWORD_SET(REVERSE) THEN BEGIN
         latitude  = theta
         longitude = phi
     ENDIF ELSE BEGIN
         phi   = longitude
         theta = latitude
     ENDELSE
     RETURN
 ENDIF
 
; compute useful quantities relating to reference angles
 sa = sin(alpha_p)
 ca = cos(alpha_p)
 sd = sin(delta_p)
 cd = cos(delta_p)
 IF at_pole THEN cd = 0d0 ; suppress rounding errors.
 
; calculate rotation matrix 

  r = [ [-sa*sp - ca*cp*sd,  ca*sp - sa*cp*sd, cp*cd ] , $
        [ sa*cp - ca*sp*sd, -ca*cp - sa*sp*sd, sp*cd ] , $
        [ ca*cd           ,  sa*cd           , sd    ] ]

; solve the set of equations for each datum point

 if keyword_set(REVERSE) then begin
        latitude = phi
        longitude = theta
        g = where( finite(phi) and finite(theta), Ng )
        if Ng EQ 0 then return
        phi1 = double(phi[g])/radeg
        theta1 = double(theta[g])/radeg
        r = transpose(r)
 endif else begin
        phi = longitude
	phi1 = dblarr(N_elements(longitude) ) 
	g = where(abs(longitude) NE 180.0d, Ng)    ;Avoid roundoff error
	if Ng GT 0 then phi1[g] = double(longitude[g])/radeg
        theta1 = double(latitude)/radeg
 endelse

; define the right-hand side of the equations

 l = cos(theta1)*cos(phi1)
 m = cos(theta1)*sin(phi1)
 n = sin(theta1)

; find solution to the system of equations and put it in b
; Can't use matrix notation in case l,m,n are vectors

 b0 = r[0,0]*l + r[1,0]*m + r[2,0]*n
 b1 = r[0,1]*l + r[1,1]*m + r[2,1]*n
 b2 = (r[0,2]*l + r[1,2]*m + r[2,2]*n) > (-1) < 1 ;Account for possible roundoff

; use b0,b1,b2 to compute "native" latitude and longitude

 if keyword_set(REVERSE) then begin
        latitude[g] = asin(b2)*radeg
        longitude[g] = atan( b1, b0)*radeg
 endif else begin
        theta = asin(b2)*radeg
        phi = atan( b1, b0)*radeg
 endelse

 return
 end
