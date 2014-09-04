pro eqpole,l,b,x,y,southpole=southpole
;+
; NAME:
;       EQPOLE
; PURPOSE:
;       Convert RA and Dec to X,Y using an equal-area polar projection.
; EXPLANATION:
;       The output X and Y coordinates are scaled to be between
;       -90 and +90 to go from equator to pole to equator. Output map points 
;       can be centered on the north pole or south pole.
;
; CALLING SEQUENCE:
;       EQPOLE, L, B, X, Y, [ /SOUTHPOLE ]
;
; INPUTS:
;       L - longitude - scalar or vector, in degrees
;       B - latitude - same number of elements as RA, in degrees
;
; OUTPUTS:
;       X - X coordinate, same number of elements as RA.   X is normalized to
;               be between -90 and 90.
;       Y - Y coordinate, same number of elements as DEC.  Y is normalized to
;               be between -90 and 90.
;
; KEYWORDS:
;
;       /SOUTHPOLE      - Keyword to indicate that the plot is to be centered 
;               on the south pole instead of the north pole.
;
; REVISION HISTORY:
;       J. Bloch        LANL, SST-9     1.1     5/16/91
;       Converted to IDL V5.0   W. Landsman   September 1997
;-

 if N_params() NE 4 then begin
     print,'Syntax - EQPOLE,L, B, X, Y, [/SOUTHPOLE]'
     print,'         Input longitude L, latitude B in *degrees*'
     return
 endif

 if keyword_set(southpole) then begin
        l1 = double(-l/!RADEG)
        b1 = double(-b/!RADEG)
 endif else begin
        l1 = double(l/!RADEG)
        b1 = double(b/!RADEG)
 endelse

 sq = 2.0d0*(1.0d0 - sin(double(b1)))
 chk = where(sq lt 0.0d0)
 if chk[0] ge 0 then sq[chk] = 0.0d0
 r = 18.0d0*3.53553391d0*sqrt(sq)
 y =r*cos(l1)
 x =r*sin(l1)

 return
 end
