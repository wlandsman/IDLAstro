;+
; NAME:
;       WCS_GETPOLE 
;
; PURPOSE:
;       Compute the coordinates of the native pole
;
; EXPLANATION:
;       WCS_GETPOLE is used to determine the celestial position of the 
;       native pole.    See section 2.4 of the paper 
;       "Representation of Celestial Coordinates in FITS" by Calabretta 
;       Greisen (2002, A&A, 395, 1077, also available at  
;       http://fits.gsfc.nasa.gov/fits_wcs.html    Called by WCS_ROTATE
;
; CALLING SEQUENCE:
;       WCS_GETPOLE, crval, lonpole, theta0, alpha_p, delta_p, [LATPOLE= AT_POLE=]
;
; INPUT PARAMETERS:
;       crval - 2 element vector containing standard system coordinates (the 
;               longitude and latitude) of the reference point in degrees
;       lonpole - native longitude of the celestial North Pole (degrees)
;                 *unless* the fiducial point is at non-zero native longitude
;                 (phi_0 =/ 0), in which case phi_0 should have been subtracted,
;                 i.e. lonpole = phi_p - phi_0.
;       theta0  - native latitude of the fiducial point (degrees)
;
; OUTPUT PARAMETERS:
;       alpha_p, delta_p - celestial longitude and latitude of the native pole
;               (Radians)
; OPTIONAL KEYWORD INPUT PARAMETERS:
;       LATPOLE - native latitude of the celestial North Pole (degrees)
;                 NB only used to resolve ambiguity. Final value is the one 
;                 nearest to input value of LATPOLE. Can be set outside range
;                 [-90,90]
;
; OPTIONAL KEYWORD OUTPUT PARAMETERS
;       AT_POLE (byte) true if delta_p = pi/2 (avoiding some round-off errors)
;
; REVISION HISTORY:
;       Written    W. Landsman               June, 2003
;       Fix calculation when theta0 is not 0 or 90     February 2004
;       E. Hivon: alpha_p, delta_p consistenly in Radians May 2010
;       J. P. Leahy introduced AT_POLE, more traps for special cases to
;       avoid rounding errors                July 2013
;
;-

pro WCS_GETPOLE, crval, lonpole, theta0, alpha_p, delta_p, $
  LATPOLE = latpole, AT_POLE = at_pole
    
 compile_opt idl2, hidden
          
; check to see that enough parameters (at least 4) were sent
 if (N_params() lt 5) then begin
    print,'Syntax - WCS_GETPOLE,  crval, lonpole, theta0 = ,alpha_p, delta_p, '
    print,'                [LATPOLE= ]' 
    return
 endif 

 ; DEFINE ANGLE CONSTANTS 
 pi = !DPI
 pi2 = acos(0d0) ; do it this way to mitigate risks of round-off errors when
                 ; checking equality to pi/2
                 
 radeg = 1.8d2/pi
 alpha_0 = double(crval[0])/radeg
 delta_0 = double(crval[1])/radeg

 if theta0 EQ 90 then begin
     alpha_p = alpha_0
     delta_p = delta_0
     at_pole = crval[1] EQ 90d0
     return
 endif

; Longpole is the longitude in the native system of the North Pole in the
; standard system (default = 180 degrees).

 phi_p   = double(lonpole)/radeg
 theta_p = double(latpole)/radeg
 sp = sin(phi_p)
 cp = cos(phi_p)
 sd = sin(delta_0)
 cd = cos(delta_0)
 tand = tan(delta_0)

 
 if (theta0 EQ 0d0) then begin
        if (delta_0 EQ 0d0) && (abs(lonpole) EQ 90.0d) then begin
            delta_p = theta_p
            at_pole = latpole EQ 90d0 
        endif else begin
            delta_p = acos( sd/cp)               ;Updated May 98
            IF latpole LE -90 then delta_p *= -1d0 else if $
              (latpole LT 90 && abs(theta_p + delta_p) LT abs(theta_p - delta_p)) $
               then delta_p = -delta_p
            at_pole = theta_p ge 0d0 && crval[1] EQ 0d0
        endelse
        alpha_p = alpha_0
        if (lonpole NE 1.8d2) && (cd NE 0d0) THEN CASE delta_p OF
            pi2:  alpha_p += phi_p - !dpi
           -pi2:  alpha_p -= phi_p 
            ELSE: alpha_p -= atan(sp/cd, -tan(delta_p)*tand )
        ENDCASE
 endif else IF theta0 EQ crval[1] && lonpole EQ 0 THEN BEGIN
     delta_p = pi2
     alpha_p = alpha_0 + phi_p - !dpi
     at_pole = 1B
 ENDIF ELSE begin                ;General case for arbitary theta0
        ctheta = cos(theta0/RADEG)
        stheta = sin(theta0/RADEG)
        term1 = atan(stheta, ctheta*cp ) 
        term2 = acos( sd/( sqrt(1.0d - ctheta^2*sp^2)  ))
        if term2 EQ 0d0 then delta_p = term1 else begin
           delta_p1 = abs( (term1 + term2)*radeg)
           delta_p2 = abs( (term1 - term2)*radeg)
           case 1 of 
           (delta_p1 GT 90) and (delta_p2 GT 90):message,'No valid solution'
           (delta_p1 LE 90) and (delta_p2 GT 90): delta_p = term1 + term2
           (delta_p1 GT 90) and (delta_p2 LE 90): delta_p = term1 - term2
           else: begin             ;Two valid solutions
                 delta_p1 = (term1 + term2)*radeg
                 delta_p2 = (term1 - term2)*radeg
                 print, delta_p1, delta_p2, latpole
                 if abs(latpole-delta_p1) LT abs(latpole - delta_p2) then $
                       delta_p = term1+term2 else delta_p = term1 - term2
                 end
           endcase
           if (cd EQ 0d0) then alpha_p = alpha_0 else begin
              sdelt = sin(delta_p)
              if (sdelt EQ 1) then alpha_p = alpha_0 - phi_p - !DPI else $
              if (sdelt EQ -1) then alpha_p = alpha_0 -phi_p else $
              alpha_p = alpha_0 - $
               atan( (stheta-sin(delta_p)*sd)/(cos(delta_p)*cd), sp*ctheta/cd )
           endelse
         endelse
         at_pole = delta_p EQ pi2
 endelse 

 return
 end
