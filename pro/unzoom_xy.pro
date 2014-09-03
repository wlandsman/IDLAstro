pro unzoom_xy,xtv,ytv,xim,yim,OFFSET=offset, ZOOM = zoom
;+
; NAME:
;      UNZOOM_XY
; PURPOSE:
;      Converts X, Y position on the image display to the the X,Y position 
;      on the corresponding data array.  (These  positions are identical 
;      only for an unroamed, unzoomed image with with pixel [0,0] of the 
;      image placed at position [0,0] on the image display.)
;
; CALLING SEQUENCE:
;      UNZoom_XY, Xtv,Ytv,Xim,Yim, [ OFFSET =, ZOOM = ]   
;
; INPUTS:
;      XTV - Scalar or vector giving X position(s) as read on the image
;            display (e.g. with Cursor, Xtv, Ytv,/DEVICE)
;      YTV - Scalar or vector giving Y position(s) on the image display.
;
;      If only 2 parameters are supplied then XTV and YTV will be modified
;      on output to contain the image array coordinates.
;
; OPTIONAL KEYWORD INPUT:
;      OFFSET - 2 element vector giving the location of the image pixel [0,0] 
;               on the window display.   OFFSET can be positive (e.g if the 
;               image is centered in a larger window) or negative (e.g. if the
;               only the central region of an image much larger than the window
;               is being displayed. 
;               Default value is [0,0], or no offset.
;      ZOOM - scalar giving the ratio of the size on the image display to the
;             original data size.     There is no capability for separate X 
;             and Y zoom.   Default = 1.
; OUTPUTS:
;      XIM,YIM - X and Y coordinates of the image corresponding to the
;            cursor position on the image display.
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
;       Adapted from MOUSSE procedure  W. Landsman       March 1996
;       Proper handling of offset option          S. Ott/W. Landsman May 2000
;       Put back common blocks for MOUSSE compatibility    September 2004
;       Fix algorithm for non-unity ZOOM values  Aug. 2013
;-

 On_error,2
 Compile_opt idl2
 common tv,chan,czoom,xroam,yroam
 common images,x00,y00,xsize,ysize 

 if N_params() LT 2 then begin
        print,'Syntax - UNZOOM_XY, xtv, ytv, xim, yim, [OFFSET= ,ZOOM = ]'
        return
 endif

    
 if N_elements(offset) NE 2 then begin
;Determine if Images common block defined
      if N_elements(x00) eq 0 then offset = [0,0] $ 
                              else offset = [x00[chan],y00[chan]]
 endif
 if N_elements(zoom) NE 1 then begin 
          if N_elements(czoom) GT 0 then zoom = czoom[chan] else $
             zoom = 1
 endif


 cen =  (zoom-1)/2.
 xim =  float((xtv-cen)/zoom) - offset[0]
 yim =  float((ytv-cen)/zoom) - offset[1]
 if N_Params() LT 3 then begin
   xtv = xim & ytv = yim
 endif

return
end                                    

