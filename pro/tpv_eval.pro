function TPV_eval, xy
;+
; NAME:
;     TPV_EVAL
; PURPOSE:
;     Compute distorted coordinates given TPV (Tangent + PV_ polynomial) 
;     coefficients.
; EXPLANATION:
;     See http://fits.gsfc.nasa.gov/registry/tpvwcs.html for the TPV convention
;   
;     This distortion convention is used by the SCAMP software 
;     ( http://www.astromatic.net/software/scamp ) though SCAMP does not 
;     include the '-TPV' in the CTYPE keyword.
;   
;     The coefficients are passed via common block.    This is because this
;     routine is called by the intrinisc BROYDEN() function in AD2XY, and 
;     common blocks are the only way to pass parameters to the user supplied 
;     function in BROYDEN().  
; CALLING SEQUENCE:
;     res = TPV_EVAL(xy)   
; INPUTS:
;     xy - 2 elements vector giving the undistorted X,Y position  
; OUTPUTS:
;     res - 2 element vector giving the distorted position 
; COMMON BLOCKS: 
;      common broyden_coeff,pv1,ycoeff
;
;      pv1, YCOEFF are both vectors giving the TPV coefficients
; REVISION HISTORY:
;     Written   W. Landsman                  Dec 2013
;     Correct several typos for 4th power terms    M. Sullivan  Mar 2014
;-
compile_opt idl2,hidden
common broyden_coeff,pv1,pv2

Npv1 = N_elements(pv1)
NPv2 = N_elements(pv2)

if N_elements(xy) EQ 2 then begin
   x = xy[0]
   y = xy[1]
endif else begin    
   x = reform(xy[*,0])
   y = reform(xy[*,1])
endelse   
x2 = x*x
y2 = y*y

xp = pv1[0] + pv1[1]*x + pv1[2]*y
if Npv1 GT 3 && (pv1[3] NE 0.0) then xp += pv1[3]*sqrt(x2 + y2)
if Npv1 GT 4 && (pv1[4] NE 0.0) then xp += pv1[4]*x2
if Npv1 GT 5 && (pv1[5] NE 0.0) then xp += pv1[5]*x*y
if Npv1 GT 6 && (pv1[6] NE 0.0) then xp += pv1[6]*y2
if Npv1 GT 7 then begin
  if pv1[7] NE 0.0 then xp += pv1[7]*x^3
  if Npv1 GT 8 && (pv1[8] NE 0.0) then xp += pv1[8]*x2*y
  if Npv1 GT 9 && (pv1[9] NE 0.0) then xp += pv1[9]*x*y2
  if Npv1 GT 10 && (pv1[10] NE 0.0) then xp += pv1[10]*y2*y
  if Npv1 GT 11 && (pv1[11] NE 0.0) then xp += pv1[11]*sqrt(x2+y2)^3
  if Npv1 GT 12 then begin
      if (pv1[12] NE 0.0) then xp += pv1[12]*y2*y2
      if Npv1 GT 13 && (pv1[13] NE 0.0) then xp += pv1[13]*x2*x*y
      if Npv1 GT 14 && (pv1[14] NE 0.0) then xp += pv1[14]*x2*y2
      if Npv1 GT 15 && (pv1[15] NE 0.0) then xp += pv1[15]*x*y2*y
      if Npv1 GT 16 && (pv1[16] NE 0.0) then xp += pv1[16]*y2*y2
      endif
 endif      

yp = pv2[0] + pv2[1]*y + pv2[2]*x
if Npv2 GT 3 && (pv2[3] NE 0.0) then yp += pv2[3]*sqrt(x2 + y2)
if NPv2 GT 4 && (pv2[4] NE 0.0) then yp += pv2[4]*y2
if NPv2 GT 5 && (pv2[5] NE 0.0) then yp += pv2[5]*x*y
if NPv2 GT 6 && (pv2[6] NE 0.0) then yp += pv2[6]*x2
if NPv2 GT 7 then begin
  if pv2[7] NE 0.0 then yp += pv2[7]*y^3
  if NPv2 GT 8 && (pv2[8] NE 0.0) then yp += pv2[8]*y2*x
  if NPv2 GT 9 && (pv2[9] NE 0.0) then yp += pv2[9]*y*x2
  if NPv2 GT 10 && (pv2[10] NE 0.0) then yp += pv2[10]*x2*x
  if NPv2 GT 11 && (pv2[11] NE 0.0) then yp += pv2[11]*sqrt(x2+y2)^3
  if NPv2 GT 12 then begin
      if (pv2[12] NE 0.0) then yp += pv2[12]*y2*y2
      if NPv2 GT 13 && (pv2[13] NE 0.0) then yp += pv2[13]*y2*y*x
      if NPv2 GT 14 && (pv2[14] NE 0.0) then yp += pv2[14]*y2*x2
      if NPv2 GT 15 && (pv2[15] NE 0.0) then yp += pv2[15]*y*x2*x
      if NPv2 GT 16 && (pv2[16] NE 0.0) then yp += pv2[16]*x2*x2
      endif
      
 endif      
   	 
return, [[xp],[yp]]

end
