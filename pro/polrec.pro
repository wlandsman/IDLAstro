;-------------------------------------------------------------
;+
; NAME:
;       POLREC
; PURPOSE:
;       Convert 2-d polar coordinates to rectangular coordinates.
; CATEGORY:
; CALLING SEQUENCE:
;       polrec, r, a, x, y
; INPUTS:
;       r, a = vector in polar form: radius, angle (radians).  in
; KEYWORD PARAMETERS:
;       Keywords:
;         /DEGREES means angle is in degrees, else radians.
; OUTPUTS:
;       x, y = vector in rectangular form, double precision     out
; COMMON BLOCKS:
; NOTES:
; MODIFICATION HISTORY:
;       R. Sterner. 18 Aug, 1986.
;       Johns Hopkins University Applied Physics Laboratory.
;       RES 13 Feb, 1991 --- added /degrees.
;	Converted to IDL V5.0   W. Landsman   September 1997
;       1999 May 03 --- Made double precision.  R. Sterner.
;
; Copyright (C) 1986, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;-
;-------------------------------------------------------------
 
	PRO POLREC, R, A, X, Y, help=hlp, degrees=degrees
 
	IF (N_PARAMS(0) LT 4) or keyword_set(hlp) THEN BEGIN
	  PRINT,' Convert 2-d polar coordinates to rectangular coordinates.
	  PRINT,' polrec, r, a, x, y
	  PRINT,'   r, a = vector in polar form: radius, angle (radians).  in'
	  PRINT,'   x, y = vector in rectangular form.                     out'
          print,' Keywords:'
          print,'   /DEGREES means angle is in degrees, else radians.'
	  RETURN
	ENDIF
 
	cf = 1.D0
	if keyword_set(degrees) then cf = 180.0d/!dpi
 
	X = R*COS(A/cf)
	Y = R*SIN(A/cf)	
	RETURN
	END
