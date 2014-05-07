function nulltrim,st
;+
; NAME:
;	NULLTRIM
; PURPOSE:
;	Trim a string of all characters after and including the first null
; EXPLANATION:
;	The null character is an ascii 0b
;
; CALLING SEQUENCE:
;	result = nulltrim( st )
;
; INPUTS:
;	st = input string
; OUTPUTS:
;	trimmed string returned as the function value.
; HISTORY:
;	D. Lindler  July, 1987
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
;--------------------------------------------------------------------
;
 b = byte(st)
 null = where( b eq 0, nfound )
 if nfound lt 1 then return, st else return, strmid( st,0,null[0] )
 end
