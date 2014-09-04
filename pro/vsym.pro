PRO VSYM, Nvert, STAR=star, SKELETON=skeleton, POLYGON=polygon, $
    FILL=fill, ROT=rot, THICK=thick

;+
; NAME:
;       VSYM
;
; PURPOSE:
;       Create "Mongo"-like polygonal plot symbols
; EXPLANATION:
;       This procedure generates a subset of Mongo-like plot symbols.
;       The symbols are the rotationally symmetric ones that have
;       a specified number of vertices and are either open or filled.
;       (The half-filled symbols are not included.)     After defining the
;       plot symbol with VSYM, make the call to PLOT (or PLOTS or OPLOT) with 
;       PSYM=8.
;
; CATEGORY:
;       Graphics
;
; CALLING SEQUENCE:
;       VSYM, Nvert
;
; INPUT POSITIONAL PARAMETERS:
;       Nvert:     Number of vertices in plot symbol.  Maximum value
;                  used is 24.
;
; INPUT KEYWORD PARAMETERS:
;       STAR:      Set this flag to get a star.  E.g., 
;                  vsym, 5,/star gets you a pentagram.
;       SKELETON:  Set this flag to get an asterisk-like symbol, where
;                  the center is connected to each vertex.  E.g.,
;                  vsym, 4, /skel gets you an X.
;       POLYGON:   Set this flag to get a regular polygon.  This is
;                  the default symbol type.
;       FILL:      Set this flag to get filled symbol.  Default=open
;       ROT:       Rotation of symbol about center, in degrees.
;                  E.g., vsym, 4, rot=45 gets you a diamond, whereas
;                  vsym, 4 gets you a square.
;       THICK:     Line thickness of symbol.  Default=!P.thick
;
; MODIFICATION HISTORY:
;       Written by:     R. S. Hill, RITSS, 2 Oct 98
;-

On_error, 0

IF n_elements(nvert) LT 1 THEN nvert=4

IF nvert GT 24 THEN $
    message,/info,'More than 24 vertices requested; 24 used'

nv = nvert < 24
vangle = (nv-2.)/nv*180.

st = keyword_set(star)
sk = keyword_set(skeleton)
po = keyword_set(polygon)
fi = keyword_set(fill)
rt = keyword_set(rot)

IF n_elements(thick) LT 1 THEN thick=!P.thick

rot_zero = -0.5*vangle
if rt then rot_zero = rot_zero + 180./nvert

IF st + sk + po GT 1 THEN message, 'More than one symbol type specified'
IF st + sk + po EQ 0 THEN po=1

angles = indgen(nv+1)/float(nv) * 2 * !pi + rot_zero/180.0*!pi
x = cos(angles) & y = sin(angles)

inv2 = indgen(nv+1)*2
inv2_1 = indgen(nv)*2 + 1

IF po THEN BEGIN
    usersym, x, y, fill=fi, thick=thick
ENDIF ELSE IF sk THEN BEGIN
    xx = fltarr(2*nv+1) & yy = xx
    xx[inv2] = x
    yy[inv2] = y 
    usersym, xx, yy, thick=thick
ENDIF ELSE IF st THEN BEGIN
    rot2 = rot_zero + 180./nv
    inner_angles = $
        indgen(nv)/float(nv) * 2 * !pi + rot2/180.0*!pi
    inner_x = cos(inner_angles)*0.32
    inner_y = sin(inner_angles)*0.32
    xx = fltarr(2*nv+1) & yy = xx
    xx[inv2] = x
    xx[inv2_1] = inner_x
    yy[inv2] = y
    yy[inv2_1] = inner_y    
    usersym, xx, yy, fill=fi, thick=thick
ENDIF

RETURN
END
