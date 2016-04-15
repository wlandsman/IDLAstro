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
;     Corrected 4th order terms once again and 
;     added 5th,6th and 7th order terms Arjun Dey Sep 3, 2015
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
      if (pv1[12] NE 0.0) then xp += pv1[12]*x2*x2
      if Npv1 GT 13 && (pv1[13] NE 0.0) then xp += pv1[13]*x2*x*y
      if Npv1 GT 14 && (pv1[14] NE 0.0) then xp += pv1[14]*x2*y2
      if Npv1 GT 15 && (pv1[15] NE 0.0) then xp += pv1[15]*x*y2*y
      if Npv1 GT 16 && (pv1[16] NE 0.0) then xp += pv1[16]*y2*y2
      if Npv1 GT 17 then begin
	   x4 = x2*x2
	   y4 = y2*y2
           if (pv1[17] NE 0.0) then xp += pv1[17]*x4*x
           if Npv1 GT 18 && (pv1[18] NE 0.0) then xp += pv1[18]*x4*y
           if Npv1 GT 19 && (pv1[19] NE 0.0) then xp += pv1[19]*x2*x*y2
           if Npv1 GT 20 && (pv1[20] NE 0.0) then xp += pv1[20]*x2*y2*y
           if Npv1 GT 21 && (pv1[21] NE 0.0) then xp += pv1[21]*x*y4
           if Npv1 GT 22 && (pv1[22] NE 0.0) then xp += pv1[22]*y4*y
           if Npv1 GT 23 && (pv1[23] NE 0.0) then xp += pv1[23]*sqrt(x2+y2)^5
           if Npv1 GT 24 then begin
                if (pv1[24] NE 0.0) then xp += pv2[24]*x4*x2
                if Npv1 GT 25 && (pv1[25] NE 0.0) then xp += pv1[25]*x4*x*y
                if Npv1 GT 26 && (pv1[26] NE 0.0) then xp += pv1[26]*x4*y2
                if Npv1 GT 27 && (pv1[27] NE 0.0) then xp += pv1[27]*x2*x*y2*y
                if Npv1 GT 28 && (pv1[28] NE 0.0) then xp += pv1[28]*x2*y4
                if Npv1 GT 29 && (pv1[29] NE 0.0) then xp += pv1[29]*x*y4*y
                if Npv1 GT 30 && (pv1[30] NE 0.0) then xp += pv1[30]*y4*y2
                if Npv1 GT 31 then begin
                     if (pv1[31] NE 0.0) then xp += pv1[31]*x4*x2*x
                     if Npv1 GT 32 && (pv1[32] NE 0.0) then xp += pv1[32]*x4*x2*y
                     if Npv1 GT 33 && (pv1[33] NE 0.0) then xp += pv1[33]*x4*x*y2
                     if Npv1 GT 34 && (pv1[34] NE 0.0) then xp += pv1[34]*x4*y2*y
                     if Npv1 GT 35 && (pv1[35] NE 0.0) then xp += pv1[35]*x2*x*y4
                     if Npv1 GT 36 && (pv1[36] NE 0.0) then xp += pv1[36]*x2*y*y4
                     if Npv1 GT 37 && (pv1[37] NE 0.0) then xp += pv1[37]*x*y2*y4
                     if Npv1 GT 38 && (pv1[38] NE 0.0) then xp += pv1[38]*y4*y2*y
                     if Npv1 GT 39 && (pv1[39] NE 0.0) then xp += pv1[39]*sqrt(x2+y2)^7
                     if Npv1 GT 40 then print,'PV1 TERMS > 40 (ORDER > 7) NOT EVALUATED'
                     endif
                endif
           endif
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
      if Npv2 GT 17 then begin
	   x4 = x2*x2
	   y4 = y2*y2
           if (pv2[17] NE 0.0) then yp += pv2[17]*y4*y
           if Npv2 GT 18 && (pv2[18] NE 0.0) then yp += pv2[18]*y4*x
           if Npv2 GT 19 && (pv2[19] NE 0.0) then yp += pv2[19]*y2*y*x2
           if Npv2 GT 20 && (pv2[20] NE 0.0) then yp += pv2[20]*y2*x2*x
           if Npv2 GT 21 && (pv2[21] NE 0.0) then yp += pv2[21]*y*x4
           if Npv2 GT 22 && (pv2[22] NE 0.0) then yp += pv2[22]*x4*x
           if Npv2 GT 23 && (pv2[23] NE 0.0) then yp += pv2[23]*sqrt(x2+y2)^5
           if Npv2 GT 24 then begin
                if (pv2[24] NE 0.0) then yp += pv2[24]*y4*y2
                if Npv2 GT 25 && (pv2[25] NE 0.0) then yp += pv2[25]*y4*y*x
                if Npv2 GT 26 && (pv2[26] NE 0.0) then yp += pv2[26]*y4*x2
                if Npv2 GT 27 && (pv2[27] NE 0.0) then yp += pv2[27]*y2*y*x2*x
                if Npv2 GT 28 && (pv2[28] NE 0.0) then yp += pv2[28]*y2*x4
                if Npv2 GT 29 && (pv2[29] NE 0.0) then yp += pv2[29]*y*x4*x
                if Npv2 GT 30 && (pv2[30] NE 0.0) then yp += pv2[30]*x4*x2
                if Npv2 GT 31 then begin
                     if (pv2[31] NE 0.0) then yp += pv2[31]*y4*y2*y
                     if Npv2 GT 32 && (pv2[32] NE 0.0) then yp += pv2[32]*y4*y2*x
                     if Npv2 GT 33 && (pv2[33] NE 0.0) then yp += pv2[33]*y4*y*x2
                     if Npv2 GT 34 && (pv2[34] NE 0.0) then yp += pv2[34]*y4*x2*x
                     if Npv2 GT 35 && (pv2[35] NE 0.0) then yp += pv2[35]*y2*y*x4
                     if Npv2 GT 36 && (pv2[36] NE 0.0) then yp += pv2[36]*y2*x*x4
                     if Npv2 GT 37 && (pv2[37] NE 0.0) then yp += pv2[37]*y*x2*x4
                     if Npv2 GT 38 && (pv2[38] NE 0.0) then yp += pv2[38]*x4*x2*x
                     if Npv2 GT 39 && (pv2[39] NE 0.0) then yp += pv2[39]*sqrt(x2+y2)^7
                     if Npv2 GT 40 then print,'PV2 TERMS > 40 (ORDER > 7) NOT EVALUATED'
                     endif
                endif
           endif
      endif
 endif      
   	 
return, [[xp],[yp]]

end
