pro AstDisp, x, y, ra, dec, DN, Coords=Coords, silent=silent
;+
; NAME:
;	ASTDISP
;
; PURPOSE:
;	Print astronomical and pixel coordinates in a standard format
; EXPLANATION:
;	This procedure (ASTrometry DISPlay) prints the astronomical and
;	pixel coordinates in a standard format.  X,Y must be supplied.  RA,DEC
;	may also be supplied, and a data number (DN) may also be 
;	supplied.   With use of the Coords= keyword, a string containing the 
;	formatted data can be returned in addition or instead (with /silent) 
;	of printing.
;
; CALLING SEQUENCE:
;	ASTDISP, x, y, [Ra, Dec, DN, COORD = , /SILENT ]
;
; INPUT:
;	X  - The X pixel coordinate(s), scalar or vector
;	Y  - The Y pixel coordinate(s), scalar or vector
;
; OPTIONAL INPUTS:
;	RA -  Right Ascension in *degrees*, scalar or vector
;	DEC - DEClination in *degrees*, scalar or vector (if RA is supplied, DEC must also be supplied)
;	DN -  Data Number or Flux values
;
;	Each of the inputs X,Y, RA, DEC, DN should have the same number of 
;		elements
; OPTIONAL INPUT KEYWORDS:
;	SILENT    Prevents printing.  Only useful when used with Coords=
; OUTPUT:
;	Printed positions in both degrees and sexagesimal format
;	All passed variables remain unchanged
; OPTIONAL KEYWORD OUTPUT:
;	COORDS    Returns the formatted coordinates in a string
; PROCEDURES CALLED:
;	ADSTRING - used to format the RA and Dec
; HISTORY:
;	10-AUG-90 Version 1 written by Eric W. Deutsch
;	20-AUG-91 Converted to standard header.  Vectorized Code.  E. Deutsch
;	20-NOV-92 Added Coords= and /silent.  E.Deutsch
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
  On_error,2

  arg = N_params()
  if (arg lt 2) then begin
    print,'Call: IDL> AstDisp,x_pixel,y_pixel,[RA,DEC],[DN],[/silent,coords=]'
    print,'e.g.: IDL> AstDisp,x,y,ra,dec'
    return
    endif

  if (arg eq 3) then message,'ERROR - Both RA and Dec values must be supplied'

  silent = keyword_set(SILENT)

; X and Y must be supplied

  hdr = '    X        Y'
  fmt = '(f8.2,1x,f8.2'
  if (arg le 2) then begin & type=0 & goto,PRN & endif

; Ra and Dec can be optionally supplied

  hdr = hdr+'         RA       DEC           RA           DEC'
  fmt = fmt+',2x,F9.4,1x,F9.4,2x,A'
  if (arg le 4) then begin & type=1 & goto,PRN & endif

; A data number can be optionally supplied

  hdr = hdr+'           DN'
  fmt = fmt+',3x,f9.3'
  type = 2

PRN:
  if not SILENT then print,hdr
  Coords = strarr( N_elements(x)+1 )
  Coords[0] = hdr

  for i = 0, N_elements(x)-1 do begin

	case type of 

	0: out = string(format=fmt+')',x[i],y[i],/print)
	1: out = string(format=fmt+')',x[i],y[i],ra[i],dec[i], $
		 adstring(ra[i],dec[i],2),/print)
	2: out = string(format=fmt+')',x[i],y[i],ra[i],dec[i], $
		 adstring(ra[i],dec[i],2),DN[i],/print)
	endcase

	if not SILENT then print,out
	Coords[i+1] = out

   endfor

  return
 end
