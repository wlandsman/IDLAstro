pro update_distort, distort, xcoeff, ycoeff
;+
; NAME:
;    UPDATE_DISTORT
; PURPOSE:
;    Update SIP nonlinear distortion coefficients for a linear transformation
; EXPLANATION:
;    The SIP coefficients can account for nonlinearities in the astrometry
;    of an astronomical image.    When the image is compressed or expanded
;    these coefficients must be adjusted in a nonlinear way.
; CALLING SEQUENCE:
;    UPDATE_DISTORT, distort, xcoeff, ycoeff
; INPUT/OUTPUT:
;    distort - structure giving SIP coefficients.    See extast.pro for 
;             description of the SIP distortion structure
;    xcoeff - 2 element numeric vector describing the linear transformation
;              xp = xcoeff[0]*x + xcoeff[1]
;    xcoeff - 2 element numeric vector describing the linear transformation
;              yp = ycoeff[0]*x + ycoeff[1]
;
; METHOD:
;     The procedure TRANSFORM_COEFF is  used to determine how the
;     coefficients change under the linear transformation.
;
;     See example of usage in hrebin.pro
; REVISION HISTORY:
;     Written, December 2007            W. Landsman
;-
 compile_opt idl2
 On_error,2
 if N_params() LT 3 then begin 
    print,'Syntax - UPDATE_DISTORT, distort, xcoeff, ycoeff'
    return
    endif
    
 a = distort.a
 b = distort.b
 a_sz = size(a,/dimen)

 for i=0,a_sz[0] - 1 do begin
     a[0,i] = transform_coeff(a[*,i], xcoeff[0], xcoeff[1] )
     b[0,i] = transform_coeff(b[*,i], xcoeff[0], xcoeff[1] )
 endfor     

 a = transpose(a)
 b = transpose(b)
 for i=0,a_sz[1] - 1 do begin
     a[0,i] = transform_coeff(a[*,i], ycoeff[0], ycoeff[1] )
     b[0,i] = transform_coeff(b[*,i], ycoeff[0], ycoeff[1] )
 endfor
 distort.a = transpose(a)/xcoeff[0]
 distort.b = transpose(b)/ycoeff[0]

 if N_elements(distort.ap) GT 1 then begin 

 ap = distort.ap
 bp = distort.bp
 ap_sz = size(ap,/dimen)

 for i=0,ap_sz[0] - 1 do begin
     ap[0,i] = transform_coeff(ap[*,i], xcoeff[0], xcoeff[1] )
     bp[0,i] = transform_coeff(bp[*,i], xcoeff[0], xcoeff[1] )
 endfor     

 ap = transpose(ap)
 bp = transpose(bp)
 for i=0,ap_sz[1] - 1 do begin
     ap[0,i] = transform_coeff(ap[*,i], ycoeff[0], ycoeff[1] )
     bp[0,i] = transform_coeff(bp[*,i], ycoeff[0], ycoeff[1] )
 endfor
 distort.ap = transpose(ap)/xcoeff[0]
 distort.bp = transpose(bp)/ycoeff[0]

 endif

 return
 end

