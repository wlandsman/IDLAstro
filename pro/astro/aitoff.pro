pro aitoff,l,b,x,y
;+
; NAME:
;       AITOFF
; PURPOSE:
;       Convert longitude, latitude to X,Y using an AITOFF projection.
; EXPLANATION:
;       This procedure can be used to create an all-sky map in Galactic 
;       coordinates with an equal-area Aitoff projection.  Output map 
;       coordinates are zero longitude centered.
;
; CALLING SEQUENCE:
;       AITOFF, L, B, X, Y 
;
; INPUTS:
;       L - longitude - scalar or vector, in degrees
;       B - latitude - same number of elements as L, in degrees
;
; OUTPUTS:
;       X - X coordinate, same number of elements as L.   X is normalized to
;               be between -180 and 180
;       Y - Y coordinate, same number of elements as L.  Y is normalized to
;               be between -90 and 90.
;
; NOTES:
;       See AIPS memo No. 46, page 4, for details of the algorithm.  This
;       version of AITOFF assumes the projection is centered at b=0 degrees.
;
; REVISION HISTORY:
;       Written  W.B. Landsman  STX          December 1989
;       Modified for Unix:
;               J. Bloch        LANL SST-9      5/16/91 1.1
;       Converted to IDL V5.0   W. Landsman   September 1997
;-
 if N_params() ne 4 then begin
     print,'Syntax - AITOFF, L, B, X, Y'
     return
 endif

 sa = l
 if N_elements(sa) eq 1 then sa = fltarr(1) + sa
 x180 = where (sa gt 180.0)
 if x180[0] ne -1 then sa[x180]  = sa[x180] - 360.
 alpha2 = sa/(2*!RADEG)
 delta = b/!RADEG   
 r2 = sqrt(2.)    
 f = 2*r2/!PI     
 cdec = cos(delta)    
 denom =sqrt(1. + cdec*cos(alpha2))
 x = cdec*sin(alpha2)*2.*r2/denom
 y = sin(delta)*r2/denom
 x = x*!radeg/f
 y = y*!radeg/f

 return
 end
