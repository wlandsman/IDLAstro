pro one_arrow,xcen,ycen,angle,label, linestyle = linestyle, $
              charsize=charsize,thick=thick,color=color, $
              arrowsize=arrowsize,font = font, data=data, normal=normal
;+
; NAME:
;       ONE_ARROW
; PURPOSE:
;       Draws an arrow labeled with a single character on the current device
; EXPLANATION:
;       ONE_ARROW is called, for example, by ARROWS to create a
;       "weathervane" showing the N-E orientation of an image.
;
; CALLING SEQUENCE:
;       one_arrow, xcen, ycen, angle, label, CHARSIZE = , THICK = , COLOR = 
;                       ARROWSIZE=, FONT =  ]
; INPUT PARAMETERS:
;    xcen, ycen = starting point of arrow, floating point scalars,
;                 In device coordinates unless /DATA or /NORMAL set
;    angle      = angle of arrow in degrees counterclockwise from +X direction
;    label      = single-character label (may be blank)
;
; OUTPUT PARAMETERS:  none
;
; OPTIONAL INPUT PARAMETERS:
;       ARROWSIZE  = 3-element vector defining appearance of arrow.
;               For device coordinates the default is  [30.0, 9.0, 35.0], 
;               meaning arrow is 30 pixels long; arrowhead lines 9 pixels 
;               long and inclined 35 degrees from arrow shaft.     For 
;               normalized coordinates the default is divided by 512., for 
;               data coordinates the default is multiplied by 
;               (!X.crange[1] - !X.crange[0])/512..
;       CHARSIZE   = usual IDL meaning, default = 2.0
;       COLOR      = name or number give the color to draw the arrow.  See
;             cgCOLOR for a list of color names.
;       /DATA - If set, then the input position (xcen, ycen) and the ARROWSIZE
;                lengths are interpreted as being in data coordinates
;       FONT - IDL vector font number to use (1-20).   For example, to write
;               the 'N' and 'E' characters in complex script, set font=13
;       /NORMAL - If set, then the input position (xcen, ycen) and the ARROWSIZE
;                lengths are interpreted as being in normal coordinates
;       THICK      = usual IDL meaning, default = 2.0
; EXAMPLE:
;       Draw an triple size arrow emanating from the point (212,224)
;       and labeled with the character 'S'
;
;       IDL> one_arrow,212,224,270,'S',charsize=3
; PROCEDURE:  
;       Calls one_ray to vector-draw arrow.
; MODIFICATION HISTORY:
;       Written by R. S. Hill, Hughes STX Corp., 20-May-1992.
;       Added font keyword, W.B. Landsman Hughes STX Corp. April 1995
;       Modified to work correctly for COLOR=0  J.Wm.Parker, HITC   1995 May 25
;       Add /NORMAL and /DATA keywords  W.Landsman    November 2006
;       Work with Coyote graphics W. Landsman  February 2011
;-
 On_error,2
 compile_opt idl2

 if N_params() LT 4 then begin
      print,'Syntax - one_arrow, xcen, ycen, angle, label, CHARSIZE = , FONT=' 
      print,'                [ /DATA, /NORMAL, THICK= , COLOR=, ARROWSIZE = ]'
      return
 endif 

  if (n_elements(arrowsize) ge 1) and (n_elements(arrowsize) ne 3) then begin
   print,'Error in ONE_ARROW:  returning to main level.'
   print,'Arrowsize is [length, head_length, head_angle]'
   print,'Defaults are [30.0,9.0,35.0]'
   return
 endif

 setdefaultvalue, charsize, 2.0
 setdefaultvalue, thick, 2.0
   if keyword_set(data) then scale = (!X.CRANGE[1] - !X.CRANGE[0])/512. $
    else if keyword_set(normal) then scale = 1/512. else scale = 1.
 if N_elements(arrowsize) eq 0 then $
    arrowsize=[30.0*scale,9.0*scale,35.0] else $
    arrowsize = [arrowsize[0]*scale, arrowsize[1]*scale, arrowsize[2] ]

 device = ~keyword_set(data) && ~keyword_set(normal)
 label = strmid(strtrim(label,2),0,1)
 if keyword_set(font) then label = '!' + strtrim(font,2) + label + '!X '
 len       = arrowsize[0]
 headlen   = arrowsize[1]
 headangle = arrowsize[2]
 baseline  = (!d.y_ch_size+!d.x_ch_size)/2.0
 char_cen_offset  = baseline*charsize
 if keyword_set(data) then char_cen_offset = $
        convert_coord(char_cen_offset,0,/device,/to_data) - $ 
        convert_coord(0,0,/device,/to_data)
 if keyword_set(normal) then char_cen_offset = $ 
       convert_coord(char_cen_offset,0,/device,/to_normal) - $
       convert_coord(0,0,/device,/to_normal)
 char_cen_offset = char_cen_offset[0]           
 char_orig_len    = char_cen_offset/2.0
 char_orig_angle  = 225.0
;  Draw shaft of arrow
one_ray,xcen,ycen,len,angle,terminus,thick=thick,color=color,data= data, $
   normal=normal,linestyle=linestyle

;  Draw head of arrow
one_ray,terminus[0],terminus[1],headlen,(angle+180.0+headangle),t2, $
   thick=thick,color=color,data=data,normal=normal,linestyle=linestyle
one_ray,terminus[0],terminus[1],headlen,(angle+180.0-headangle),t2, $
   thick=thick,color=color,data = data, normal = normal,linestyle=linestyle

;  Draw label
one_ray,xcen,ycen,len+char_cen_offset,angle,terminus,/nodraw
one_ray,terminus[0],terminus[1],char_orig_len,char_orig_angle,char_orig,/nodraw
cgtext, char_orig[0], char_orig[1], label, charthick=thick, color=color, $
 charsize=charsize, device=device, normal=normal


 return
 end
