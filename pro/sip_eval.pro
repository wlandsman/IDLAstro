function sip_eval, xy
;+
; NAME:
;     SIP_EVAL
; PURPOSE:
;     Compute distorted coordinates given SIP (simple imaging polynomial) 
;     coefficients.
; EXPLANATION:
;     See http://fits.gsfc.nasa.gov/registry/sip.html for the SIP convention
;      
;     The coefficients are passed via common block.    This is because this
;     routine is called by the intrinisc BROYDEN() function in AD2XY, and 
;     common blocks are the only way to pass parameters to the user supplied 
;     function in BROYDEN().  
; CALLING SEQUENCE:
;     res = SIP_EVAL(xy)   
; INPUTS:
;     xy - 2 elements vector giving the undistorted X,Y position  
; OUTPUTS:
;     res - 2 element vector giving the distorted position 
; COMMON BLOCKS: 
;      common broyden_coeff,xcoeff,ycoeff
;
;      XCOEFF, YCOEFF are both nxn arrays giving the SIP coefficient for an
;      n x n polynomial.
; REVISION HISTORY:
;     Written   W. Landsman                  Dec 2013
;-
compile_opt idl2,hidden
common broyden_coeff,xcoeff,ycoeff

dim = size(xcoeff,/dimen)
n = dim[0]
xp = xy[0]
yp = xy[1]

for i= 0,n-1 do begin
     for j=0,n-1 DO begin 
         if xcoeff[i,j] NE 0.0 then  xp += xcoeff[i,j]*xy[0]^i*xy[1]^j
	 if ycoeff[i,j] NE 0.0 then  yp += ycoeff[i,j]*xy[0]^i*xy[1]^j
      endfor
endfor 
     	 
return, [xp,yp]

end
