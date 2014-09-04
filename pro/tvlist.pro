pro tvlist, image, dx, dy, TEXTOUT = textout, OFFSET = offset, ZOOM = ZOOM
;+
; NAME:
;	TVLIST
; PURPOSE:
;	Cursor controlled listing of image pixel values in a window. 
;
; CALLING SEQUENCE:
;	TVLIST, [image, dx, dy, TEXTOUT=, OFFSET= , ZOOM= ]
;
; OPTIONAL INPUTS:
;	IMAGE - Array containing the image currently displayed on the screen.
;		If omitted, the byte pixel intensities are read from the TV
;		If the array does not start at position (0,0) on the window then
;		the OFFSET keyword should be supplied.
;
;	DX     -Integer scalar giving the number of pixels in the X direction 
;		to be displayed.  If omitted then DX = 18 for byte images, and 
;		DX = 14 for integer images.  TVLIST will display REAL data 
;		with more significant figures if more room is availble to 
;		print.  
;
;	DY    - Same as DX, but in Y direction.  If omitted, then DY = DX 
;
; OPTIONAL INPUT KEYWORDS:
;      OFFSET - 2 element vector giving the location of the image pixel (0,0) 
;		on the window display.   OFFSET can be positive (e.g if the 
;		image is centered in a larger window) or negative (e.g. if the
;		only the central region of an image much larger than the window
;		is being displayed. 
;		Default value is [0,0], or no offset.
;	ZOOM - Scalar specifying the magnification of the window with respect
;		to the image variable.    Use, for example, if image has been
;		REBINed before display.
;	TEXTOUT - Optional keyword that determines output device.
;		The following dev/file is opened for output.
;
;		textout=1	TERMINAL using /more option (default)
;		textout=2	TERMINAL without /more option
;		textout=3	<program>.prt  
;		textout=4	laser.tmp
;		textout=5       user must open file
;		textout=7	Append to an existing <program>.prt file if it
;				exists
;		textout = filename (default extension of .prt)
;
;	If TEXTOUT > 3 or set to a filename, then TVLIST will prompt for a 
;	brief description to be included in the output file
; OUTPUTS:
;	None.
; PROCEDURE:
;	Program prompts user to place cursor on region of interest in 
;	image display.  Corresponding region of image is then displayed at
;	the terminal.   A compression factor between the image array and the
;	displayed image is determined using the ratio of image sizes.  If 
;	necessary, TVLIST will divide all pixel values in a REAL*4 image by a 
;	(displayed) factor of 10^n (n=1,2,3...) to make a pretty format.
;
; SYSTEM VARIABLE:
;	The nonstandard system variable !TEXTOUT is used as an alternative to
;	the keyword TEXTOUT.   The procedure ASTROLIB can be used to define
;	!TEXTOUT (and !TEXTUNIT) if necessary.
;
; RESTRICTIONS:
;	TVLIST may not be able to correctly format all pixel values if the
;	dynamic range near the cursor position is very large.
;
;       For the cursor to work under Mac OSX  the "Click-through Inactive 
;       Windows" setting the in X11:Preferences:Window needs to be enabled.
; PROCEDURES CALLED:
;	IMLIST, UNZOOM_XY
; REVISION HISTORY:
;	Written by rhc, SASC Tech, 3/14/86.
;	Added textout keyword option, J. Isensee, July, 1990
;	Check for readable pixels     W. Landsman   May 1992
;	Use integer format statement from F_FORMAT    W. Landsman   Feb 1994
;	Added OFFSET, ZOOM keywords  W. Landsman   Mar 1996
;	More intelligent formatting of longword, call TEXTOPEN with /STDOUT
;		W. Landsman  April, 1996
;	Added check for valid dx value  W. Landsman   Mar 1997
;	Converted to IDL V5.0   W. Landsman   September 1997
;       Major rewrite to call IMLIST, recognize new integer data types
;                                           W. Landsman Jan 2000
;       Remove all calls to !TEXTUNIT   W. Landsman   Sep 2000
;       Always call UNZOOM_XY for MOUSSE compatibility  W. Landsman Sep. 2004
;-
 On_error,2
 Compile_opt idl2

 npar = N_params()

 if npar GE 2 then $
    if N_elements( dx) NE 1 then $
          message, 'ERROR - Second parameter (format width) must be a scalar'

 if npar EQ 0 then begin 	;Read pixel values from TV

        if (!D.FLAGS and 128) NE 128 then message, $
             'ERROR -- Unable to read pixels from current device ' + !D.NAME
	message,'No image array supplied, pixel values read from TV',/INF 
	type = 1		;Byte format

 endif else begin

	sz = size(image)
	if (sz[0] LT 2) or (sz[sz[0]+2] NE sz[1]*sz[2]) then $
		message,'Image array (first parameter) not 2-dimensional'
    	type = sz[sz[0]+1]	     ;Byte or Integer image?

 endelse 

 if (!D.FLAGS AND 256) EQ 256 THEN wshow,!D.WINDOW

 if ( npar GT 0 ) then begin 	;get X and Y dimensions of the image
	xdim = sz[1] - 1 
	ydim = sz[2] - 1 
 endif else begin		;dimensions of TV display
	xdim = !d.x_vsize
	ydim = !d.y_vsize
 endelse

 if N_elements(dx) EQ 0 then  $  ;Use default print size? 
    if type EQ 1 then dx = 18 else dx = 15 else $
    if (dx GT 38) then begin 
	message, 'ERROR - X Pixel Width (second parameter) value of ' + $
		strtrim(dx,2) + ' is too large',/CON
    return
 endif

 tvcrs, 1                                    ;Make sure cursor is on
 print, 'Put the cursor on the area you want to list; press any mousse button'
 if Npar GT 0 then begin
   cursor, xtv, ytv, /WAIT, /DEVICE
   unzoom_xy, xtv, ytv, xim, yim, OFFSET=offset, ZOOM=zoom 
   xim = fix(xim+0.5)
   yim = fix(yim+0.5)
 endif else cursor, xim, yim, /WAIT, /DEVICE

 if npar LT 3 then dy = dx
; Don't try to print outside the image
  xmax = (xim + dx/2) < xdim
  xmin = (xim - dx/2) > 0 
  ymax = (yim + dy/2) < ydim
  ymin = (yim - dy/2) > 0 

 dx = xmax - xmin + 1 & dy = ymax - ymin + 1

 if xmin GE xmax then $
    message,'ERROR - The cursor is off the image in the x-direction'
 if ymin GE ymax then $
    message,'ERROR - The cursor is off the image in the y-direction'


 if npar EQ 0 then begin 
    image = tvrd( xmin,ymin,dx,dy)
    xim = dx/2
    yim = dy/2
    zoffset = [xmin,ymin]
 endif

 imlist,image,xim,yim,dx=dx,dy=dy,textout=textout,offset=zoffset
 
 return
 end
