pro pixcolor, pix_value, color
;+
; NAME:
;	PIXCOLOR
; PURPOSE:
;	Assign colors to specified pixel values in a color lookup table
; EXPLANATION:
;       Colors can be specified either from the list in cgcolor 
;       (http://www.idlcoyote.com/programs/cgcolor.pro ) or as 1 letter 
;       abbreviations for 8 common colors.
;
; CALLING SEQUENCE:
;      	PIXCOLOR, pixvalue, color         ;Set color at specified pixel values
;
; INPUT PARMETERS:
;	pixvalue - value or range of pixel values whose color will be modified.
;		A single pixel value may be specified by an integer
;		If a range of values is specified, then it must be written
;		as a string, with a colon denoting the range (e.g.'102:123')
;		If omitted, program will prompt for this parameter.
;
;  OPTIONAL INPUT PARAMETER
;	color - scalar string specifying either a full color name available in
;               CGCOLOR, or a  single character string giving one of the 
;               specified colors: 'R' (red), 'B' (blue), 'G' (green)
;		'Y' (yellow), 'T' (turquoise), 'V' (violet), 'W' (white)
;		or 'D' (dark).  If omitted, program will prompt for this 
;		parameter.
;
; OUTPUTS:
;	None
; PROCEDURE:
;	TVLCT is used in RGB mode to load the specified pixel values.
;
; EXAMPLE:
;	Set pixel values of 245 to a color of red
;
;	IDL> pixcolor,245,'R'
;
;       Set pixel values 120 to 150 to Magenta
;
;       IDL> pixcolor,'120:150','Magenta'      
; REVISION HISTORY:
;	Written, W. Landsman ST Systems Corp.		February, 1987
;	Converted to IDL V5.0   W. Landsman   September 1997
;       Allow specification of cgcolor names   April 2011
;-
 On_error,2
 compile_opt idl2

 if N_params() EQ 0 then begin 
     print,'Syntax - pixcolor, value, color_name'
     return
 endif 
     
 if ( N_elements(pix_value) EQ 0) then begin
	pix_value = ''
	print,'Enter pixel value(s) to be assigned a color value'
	print,'Value may be either number or a range (e.g. 102:123)'
	read,'Pixel Value(s): ',pix_value
 endif

 type = size(pix_value)
 if ( type[1] EQ 7 ) then begin
	pixmin = fix(gettok(pix_value,':')) >0
	if strlen(pix_value) eq 0 then pixmax = fix(pixmin)  $
		else pixmax = fix(pix_value) > pixmin < 255
 endif else begin                                               
	pixmin = fix(pix_value)>0<255
	pixmax = pixmin
 endelse 
 npts = pixmax - pixmin + 1

GETCOL: if ( N_params() LT 2 ) then begin
	color = ''
	print,'Enter color name to which pixel(s) will be asssigned'
	print,'Available 1 character options are '
	print,'Red (R), Blue (B), Green (G), Yellow (Y), Turquoise (T),
	print,'Violet (V), White (W), or Dark (D)
        read,color
 endif

 case strupcase(color) of
	'R': col = 'red'
	'G': col = 'green'
	'B': col = 'blue'
	'Y': col = 'yellow'
	'T': col = 'turquoise'
	'V': col = 'violet
	'W': col = 'white'
	'D': col = 'black'
	else: col = color
 endcase

 cc = cgcolor(col,/triple)
 if npts GT 1 then cc = rebin(cc,npts,3)
 tvlct,cc,pixmin

 return
 end
