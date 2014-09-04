FUNCTION  DAO_VALUE, XX, YY, GAUSS, PSF, DVDX, DVDY
;+
; NAME:
;	DAO_VALUE
; PURPOSE:
;	Returns the value of a DAOPHOT point-spread function at a set of points.
; EXPLANATION:
;	The value of the point-spread function is the sum of a
;	two-dimensional integral under a bivariate Gaussian function, and 
;	a value obtained by interpolation in a look-up table.  DAO_VALUE will
;	optionally compute the derivatives wrt X and Y
;
; CALLING SEQUENCE:
;	Result = DAO_VALUE( xx, yy, gauss, psf, [ dvdx, dvdy ] )
;
; INPUTS:
;	XX,YY   - the real coordinates of the desired point relative 
;		to the centroid of the point-spread function.
;	GAUSS  -  5 element vector describing the bivariate Gaussian
;	GAUSS(0)- the peak height of the best-fitting Gaussian profile.
;	GAUSS(1,2) - x and y offsets from the centroid of the point-spread 
;		function to the center of the best-fitting Gaussian.
;	GAUSS(3,4) - the x and y sigmas of the best-fitting Gaussian.
;	PSF  -  a NPSF by NPSF array containing the look-up table.
;
; OUTPUTS:
;    RESULT - the computed value of the point-spread function at
;             a position XX, YY relative to its centroid (which 
;             coincides with the center of the central pixel of the
;             look-up table).
;
; OPTIONAL OUTPUTS:
;       DVDX,DVDY - the first derivatives of the composite point-spread
;             function with respect to x and y.
;
; NOTES
; 	although the arguments XX,YY of the function DAO_VALUE
;	are relative to the centroid of the PSF, the function RINTER which
;	DAO_VALUE calls requires coordinates relative to the corner of the 
;	array (see code).
;
; PROCEDURES CALLED:
;	DAOERF, RINTER()
; REVISON HISTORY:
;	Adapted to IDL by B. Pfarr, STX, 11/17/87 from 1986 STSDAS version
;	of DAOPHOT
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
 s = size(psf)
 npsf = s[1]
 half = float(npsf-1)/2 

 x = 2.*xx + half   ;Initialize
 y = 2.*yy + half

; X and Y are the coordinates relative to the corner of the look-up table, 
; which has a half-pixel grid size.  

 if ( (min(x) LT 1.) or ( max(x) GT npsf-2.) or  $
      (min(y) LT 1.) or ( max(y) GT npsf-2.) ) then begin
         message,'X,Y positions too close to edge of frame',/INF
         return,xx*0
  endif

; Evaluate the approximating Gaussian.
; Then add a value interpolated from the look-up table to the approximating
; Gaussian.  Since the lookup table has a grid size of one-half pixel in each
; coordinate, the spatial derivatives must be multiplied by two to yield
; the derivatives in units of ADU/pixel in the big frame.

 if N_params() GT 4 then begin   ;Compute derivatives?

     DAOERF, xx, yy, gauss, e, pder 
     value = e + RINTER( psf, x, y, dfdx, dfdy)
     dvdx = 2.*dfdx - pder[*,1]
     dvdy = 2.*dfdy - pder[*,2]           

 endif else begin  

     DAOERF, xx, yy, gauss, e
     value = e + RINTER(psf,x,y)

 endelse

 return, value

 end                                             
