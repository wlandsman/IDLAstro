pro dist_ellipse,im,n,xc,yc,ratio,pos_ang, DOUBLE = double
;+
; NAME:
;       DIST_ELLIPSE
; PURPOSE:
;       Create a mask array useful for elliptical aperture photometry
; EXPLANATION:
;       Form an array in which the value of each element is equal to the
;       semi-major axis of the ellipse of specified center, axial ratio, and 
;       position  angle, which passes through that element.  Useful for 
;       elliptical aperture photometry.
;
; CALLING SEQUENCE:
;       DIST_ELLIPSE, IM, N, XC, YC, RATIO, [ POS_ANG] , /DOUBLE
;
; INPUTS:
;       N = either  a scalar specifying the size of the N x N square output
;               array, or a 2 element vector specifying the size of the
;               M x N rectangular output array.
;       XC,YC - Scalars giving the position of the ellipse center.   This does
;               not necessarily have to be within the image
;       RATIO - Scalar giving the ratio of the major to minor axis.   This 
;               should be greater than 1 for position angle to have its 
;               standard meaning.
;
; OPTIONAL INPUTS:
;       POS_ANG - Position angle of the major axis in degrees, measured counter-clockwise
;               from the Y axis.  For an image in standard orientation 
;               (North up, East left) this is the astronomical position angle.
;               Default is 0 degrees.
;
; OPTIONAL INPUT KEYWORD:
;       /DOUBLE - If this keyword is set and nonzero, the output array will
;               be of type DOUBLE rather than floating point.
;
; OUTPUT:
;       IM - REAL*4 elliptical mask array, of size M x N.  THe value of each 
;               pixel is equal to the semi-major axis of the ellipse of center
;                XC,YC, axial ratio RATIO, and position angle POS_ANG, which 
;               passes through the pixel.
;
; EXAMPLE:
;       Total the flux in a elliptical aperture with a major axis of 3', an
;       axial ratio of 2.3, and a position angle of 25 degrees centered on 
;       a specified RA and DEC.   The image array, IM is 200 x 200, and has 
;       an associated FITS header H.
;
;       ADXY, H, ra, dec, x, y       ;Get X and Y corresponding to RA and Dec
;       GETROT, H, rot, cdelt        ;CDELT gives plate scale degrees/pixel
;       cdelt = abs( cdelt)*3600.    ;CDELT now in arc seconds/pixel
;       DIST_ELLIPSE, ell, 200, x, y, 2.3, 25  ;Create a elliptical image mask
;       ell = ell*cdelt[0]           ;Distances now given in arcseconds
;       good = where( ell lt 180 )   ;Within 3 arc minutes
;       print,total( im(good) )      ;Total pixel values within 3'
;
; RESTRICTIONS:
;       The speed of DIST_ELLIPSE decreases and the the demands on virtual
;       increase as the square of the output dimensions.   Users should
;       dimension the output array as small as possible, and re-use the
;       array rather than re-calling DIST_ELLIPSE
;
; REVISION HISTORY:
;       Written    W. Landsman             April, 1991
;       Somewhat faster algorithm          August, 1992
;       Allow rectangular output array     June, 1994
;       Added /DOUBLE keyword   W. Landsman   July 2000
;       Make POS_ANG optional, as documented  W. Landsman Aug 2015
;-
 On_error,2                             ;Return to caller

 if N_params() LT 5 then begin
    print,'Syntax - DIST_ELLIPSE, im, n, xc, yc, ratio, [pos_ang], /DOUBLE'
    print,'   im - output elliptical mask image array'
    print,'   n -  size of output image mask, scalar or 2 element vector'
    print,'   xc,yc - coordinates of ellipse center, scalars'
    print,'   ratio - ratio of major to minor axis of ellipse, scalar'
    print,'   pos_ang - position angle, counterclockwise from up'
    return
 endif
                                          ;Check some parameters
 if N_elements(ratio) NE 1 then message, $
     'ERROR - Axial ratio (fifth parameter) must be a scalar value'

 if N_elements(pos_ang) GT 1 then message, $
     'ERROR - Position angle (sixth parameter) must be a scalar value'

 if N_elements(pos_ang) EQ 0 then pos_ang = 0
 ang = pos_ang /!RADEG                      ;Convert to radians
 cosang = cos(ang)
 sinang = sin(ang)

 if N_elements(N) EQ 2 then begin
        nx = n[0]
        ny = n[1] 
 endif else if N_elements(N) EQ 1 then begin
        ny = n
        nx = n                    ;Make a row
 endif else message, $
        'ERROR - Output size parameter N must contain 1 or 2 elements'
        
 if keyword_set(double) then begin
    x = dindgen(nx) - xc
    y = dindgen(ny) - yc
    im = dblarr(nx, ny, /NOZERO)
 endif else begin
    x = findgen( nx ) - xc
    y = findgen( ny ) - yc
    im = fltarr( nx, ny, /NOZERO )
 endelse
                         ;Rotate pixels to match ellipse orientation
 xcosang = x*cosang
 xsinang = x*sinang

 for i = 0,ny-1 do begin
   xtemp =  xcosang + y[i]*sinang
   ytemp = -xsinang + y[i]*cosang
   im[0,i] = sqrt( (xtemp*ratio)^2 + ytemp^2 )
 endfor

 return
 end
