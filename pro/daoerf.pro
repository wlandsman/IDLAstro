pro daoerf,x,y,a,f,pder	;DAOphot ERRor function
;+
; NAME:
;	DAOERF
; PURPOSE:         
;	Calulates the intensity, and derivatives, of a 2-d Gaussian PSF
; EXPLANATION:
;	Corrects for the finite size of a pixel by integrating the Gaussian
;	over the size of the pixel.    Used in the IDL-DAOPHOT sequence.   
;
; CALLING SEQUENCE:
;	DAOERF, XIN, YIN, A, F, [ PDER ] 
;
; INPUTS:
;	XIN - input scalar, vector or array, giving X coordinate values
;	YIN - input scalar, vector or array, giving Y coordinate values, must 
;		have same number of elements as XIN.
;	A - 5 element parameter array describing the Gaussian
;		A(0) - peak intensity
;		A(1) - X position of peak intensity (centroid)
;		A(2) - Y position of peak intensity (centroid)
;		A(3) - X sigma of the gaussian (=FWHM/2.345)         
;		A(4) - Y sigma of gaussian
;
; OUTPUTS:
;	F - array containing value of the function at each (XIN,YIN) 
;	    The number of output elements in F and PDER is identical with
;		the number of elements in X and Y
;
; OPTIONAL OUTPUTS:
;	PDER - 2 dimensional array of size (NPTS,5) giving the analytic
;		derivative at each value of F with respect to each parameter A.
;
; REVISION HISTORY:
;	Written: W. Landsman                October, 1987
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
 norm = 2.506628275		;norm = sqrt(2*!pi)
 npts = N_elements(x) 

 u2 = (x[*] - a[1] + 0.5)/a[3] & u1 = (x[*] - a[1] - 0.5)/a[3]
 v2 = (y[*] - a[2] + 0.5)/a[4] & v1 = (y[*] - a[2] - 0.5)/a[4]
 fx = norm*a[3]*(gaussint(u2) - gaussint(u1))
 fy = norm*a[4]*(gaussint(v2) - gaussint(v1))
 f =  a[0]*fx*fy
 if N_params() le 4 then return		;Need partial derivatives ?

 pder = fltarr(npts,5)
 pder[0,0] = fx*fy
 uplus = exp(-0.5*u2^2) & uminus = exp(-0.5*u1^2)
 pder[0,1] = a[0]*fy*(-uplus + uminus)
 vplus = exp(-0.5*v2^2) & vminus = exp(-0.5*v1^2)
 pder[0,2] = a[0]*fx*(-vplus + vminus)
 pder[0,3] = a[0]*fy*(fx/a[3] + u1*uminus - u2*uplus)
 pder[0,4] = a[0]*fx*(fy/a[4] + v1*vminus - v2*vplus)

 return
 end
