pro one_ray,xcen,ycen,len,angle,terminus,nodraw=nodraw, _EXTRA=_extra, $
    data = data, normal = normal
;+
; NAME:
;       ONE_RAY
; PURPOSE:
;       Draw a line with a specified starting point, length, and  angle
;
; CALLING SEQUENCE:
;       one_ray, xcen, ycen, len, angle, terminus, /NODRAW ]
;
; INPUT PARAMETERS:
;       xcen, ycen = starting point in device coordinates, floating point 
;                       scalars
;       len        = length in pixels, device coordinates
;       angle      = angle in degrees counterclockwise from +X direction
;
; OUTPUT PARAMETERS:
;       terminus = two-element vector giving ending point of ray in device
;               coordinates
;
; OPTIONAL KEYWORD INPUT PARAMETERS:
;       /nodraw   if non-zero, the ray is not actually drawn, but the terminus
;               is still calculated
;
;        Any valid keyword to cgPLOTS can also be passed ot ONE_RAY.   In
;        particular, COLOR, THICK, and LINESTYLE control the color, thickness
;        and linestyle of the drawn line.
; EXAMPLE:
;       Draw a double thickness line of length 32 pixels from (256,256) 
;       45 degrees counterclockwise from the X axis
;
;       IDL> one_ray, 256, 256, 32, 45 ,term, THICK = 2
;
; PROCEDURE:  straightforward matrix arithmetic
;
; MODIFICATION HISTORY:
;    Written by R. S. Hill, Hughes STX Corp., 20-May-1992.
;    Modified to work correctly for COLOR=0  J.Wm.Parker  HITC   1995 May 25
;    Added _EXTRA keywords to PLOT   W. Landsman   November 2006
;    Work with Coyote Graphcis W. Landsman February 2011
;-
 On_error,2
 compile_opt idl2

 if N_params() LT 3 then begin
    print,'Syntax -  one_ray, xcen, ycen, len, angle, [terminus,] ' + $
               '[ /DATA, /NORMAL, THICK= ,COLOR =, /NODRAW ]'
 endif

 device = ~keyword_set(normal) &&  ~keyword_set(data)
 sina = sin(angle/!radeg)
 cosa = cos(angle/!radeg)
 rot_mat = [ [ cosa, sina ], [-sina, cosa ] ]
 terminus =  (rot_mat # [len, 0.0]) + [xcen, ycen]

 if ~keyword_set(nodraw) then $
   cgplots, [xcen, terminus[0]], [ycen, terminus[1]], $
      DEVICE=device, Normal=Normal,_STRICT_Extra= _extra

 return
 end
