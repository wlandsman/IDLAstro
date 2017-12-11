function TNX_eval, xy

;+
; NAME:
;     TNX_EVAL
; PURPOSE:
;     Compute distorted coordinates given TNX (Tangent + Iraf tnx
;     distortion  polynomial) coefficients.
; EXPLANATION:
;     See http://fits.gsfc.nasa.gov/registry/tnx.html for the TNX convention
;   
;     This distortion convention is used by IRAF.    The current procedures only
;     supports simple polynomials and not Legendre or Chebyshev polynomials
;   
;     The coefficients and information are passed via common block.    This is because this
;     routine is called by the intrinisc BROYDEN() function in AD2XY, and 
;     common blocks are the only way to pass parameters to the user supplied 
;     function in BROYDEN().  
; CALLING SEQUENCE:
;     res = TNX_EVAL(xy)   
; INPUTS:
;     xy - 2 elements vector giving the undistorted X,Y position  
; OUTPUTS:
;     res - 2 element vector giving the distorted position 
; COMMON BLOCKS: 
;      common broyden_coeff,pv1,pv2
;
;      pv1, pv2 are both structures giving the TNX coefficients. The
;      pv1/pv2 naming convention is a hangover from tpv_eval.pro on
;      which this approach is heavily based.
;      pv1.functype gives the TNX function type. Only type 3
;         (polynomial) is supported.
;      pv1.xterms gives the type of cross-terms (1: full, 2: half, 0: none)
;      pv1.etaorder gives the order in eta
;      pv1.xiorder gives the order in xi
;      pv1.coeff gives the actual coefficients.
; REVISION HISTORY:
;     Written   M. Sullivan                  Mar 2014
;     Use post-V6.0 notation  W. Landsman    Feb 2015
;-

compile_opt idl2,hidden
common broyden_coeff,pv1,pv2

lngcor=pv1
latcor=pv2

if N_elements(xy) EQ 2 then begin
   x = xy[0]
   y = xy[1]
endif else begin    
   x = reform(xy[*,0])
   y = reform(xy[*,1])
endelse   

IF(lngcor.functype NE 3 || latcor.functype NE 3)THEN BEGIN
   PRINT,'ERROR in tnx_eval: only functype=3 (polynominal) is supported)'
   RETURN,0
ENDIF


IF(lngcor.functype EQ 1 || lngcor.functype EQ 2)THEN xin = (2. * x - (lngcor.ximax + lngcor.ximin)) / (lngcor.ximax - lngcor.ximin) ELSE xin=x
IF(latcor.functype EQ 1 || latcor.functype EQ 2)THEN etain = (2. * y - (latcor.etamax + latcor.etamin)) / (latcor.etamax - latcor.etamin) ELSE yin=y

xp=0.d0
icount=0L
IF(lngcor.xterms EQ 1)THEN BEGIN
   ;; full cross-terms
   FOR n=0,lngcor.etaorder-1 DO BEGIN
      FOR m=0,lngcor.xiorder-1 DO BEGIN
         xp += xin^m * yin^n * lngcor.coeff[icount]
         icount++
      ENDFOR
   ENDFOR
ENDIF ELSE IF(lngcor.xterms EQ 0)THEN BEGIN
   ;; no cross-terms
   FOR m=0,lngcor.xiorder-1 DO BEGIN
      xp += xin^m * lngcor.coeff[icount]
      icount++
   ENDFOR
   FOR n=0,lngcor.etaorder-1 DO BEGIN
      xp += yin^n * lngcor.coeff[icount]
      icount++
   ENDFOR
ENDIF ELSE IF(lngcor.xterms EQ 2)THEN BEGIN
   ;; half cross terms
   maxxt=MAX([lngcor.xiorder,lngcor.etaorder])-1
   FOR n=0,lngcor.etaorder-1 DO BEGIN
      FOR m=0,lngcor.xiorder-1 DO BEGIN
         IF(m+n GT maxxt)THEN CONTINUE
         xp += xin^m * yin^n * lngcor.coeff[icount]
         icount++
      ENDFOR
   ENDFOR   
ENDIF

yp = 0.d0
icount = 0L
IF(latcor.xterms EQ 1)THEN BEGIN
   ;; full cross-terms
   FOR n=0,latcor.etaorder-1 DO BEGIN
      FOR m=0,latcor.xiorder-1 DO BEGIN
         yp += xin^m * yin^n * latcor.coeff[icount]
         icount++
      ENDFOR
   ENDFOR
ENDIF ELSE IF(latcor.xterms EQ 0)THEN BEGIN
   ;; no cross-terms
   FOR m=0,latcor.xiorder-1 DO BEGIN
      yp += xin^m * latcor.coeff[icount]
      icount++
   ENDFOR
   FOR n=0,latcor.etaorder-1 DO BEGIN
      yp += yin^n * latcor.coeff[icount]
      icount++
   ENDFOR
ENDIF ELSE IF(latcor.xterms EQ 2)THEN BEGIN
   ;; half cross terms
   maxxt=MAX([latcor.xiorder,latcor.etaorder])-1
   FOR n=0,latcor.etaorder-1 DO BEGIN
      FOR m=0,latcor.xiorder-1 DO BEGIN
         IF(m+n GT maxxt)THEN CONTINUE
         yp += xin^m * yin^n * latcor.coeff[icount]
         icount++
      ENDFOR
   ENDFOR   
ENDIF

xp = x+xp
yp = y+yp

return, [[xp],[yp]]

end
