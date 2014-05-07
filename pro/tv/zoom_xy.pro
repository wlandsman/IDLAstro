pro zoom_xy, xim, yim, xtv, ytv, OFFSET=offset, ZOOM = zoom
;+
; NAME:
;      ZOOM_XY
; PURPOSE:
;       Converts X, Y position on the image array to the the X,Y position 
;       in the current window.   (These  positions are identical 
;       only for an unroamed, zoomed image with with pixel [0,0] of the 
;       image placed at position [0,0] on the image display.)
;
; CALLING SEQUENCE:
;      ZOOM_XY, Xim,Yim,Xtv,Ytv, [ OFFSET =, ZOOM = ]
;
; INPUTS:
;      XIM - Scalar or vector giving X position(s) in an image array.
;      YIM - Like XTV but giving Y position(s) in an image array.
;
;      If only 2 parameters are supplied then XIM and YIM will be modfied
;      on output to contain the converted coordinates.
;
; OPTIONAL KEYWORD INPUT:
;      OFFSET - 2 element vector giving the location of the image pixel (0,0) 
;               on the window display.   OFFSET can be positive (e.g if the 
;               image is centered in a larger window) or negative (e.g. if the
;               only the central region of an image much larger than the window
;               is being displayed. 
;               Default value is [0,0], or no offset.
;
;       ZOOM - Scalar specifying the magnification of the window with respect
;               to the image variable.
; OUTPUTS:
;      XTV,YTV - REAL*4 X and Y coordinates on the image display corresponding
;              to the input data coordinates. 
; COMMON BLOCKS:
;       If present, ZOOM_XY will use the TV and IMAGE common blocks which are
;       defined in the MOUSSE software system (see 
;        http://archive.stsci.edu/uit/analysis.html)   If the user is not using
;       the MOUSSE software (which keeps track of the offset and zoom in each
;       window) then the common blocks are ignored.
; NOTES:
;       The integer value of a pixel is assumed to refer to the *center*
;       of a pixel.
; REVISON HISTORY:
;       Adapted from MOUSSE procedure of the same name W. Landsman HSTX Mar 1996
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Properly include ZOOM keyword  W. Landsman   May 2000
;       Put back common blocks for MOUSSE compatibility    September 2004
;-
 On_error,2
 Compile_opt idl2
 common tv,chan,czoom,xroam,yroam
 common images,x00,y00,xsize,ysize 

 if N_params() LT 2 then begin
        print,'Syntax - Zoom_XY, Xtv, Ytv, Xim, Yim, [ Offset=, Zoom = ]'
        return
 endif
    
 if N_elements(offset) NE 2 then begin
;Determine if Images common block defined
      if n_elements(x00) eq 0 then offset = [0,0] $ 
                              else offset = [x00[chan],y00[chan]]
 endif
 if N_elements(zoom) NE 1 then begin 
          if N_elements(czoom) GT 0 then zoom = czoom[chan] else $
             zoom = 1
 endif

 cen =  (zoom-1)/2.

 xtv =  cen + zoom*(xim + offset[0] )
 ytv =  cen + zoom*(yim + offset[1] )

 if N_Params() LT 3 then begin
    xim = xtv  & yim = ytv
 endif                  

 return
 end                                    
