function rem_dup, a, flag
;+
; NAME:	
;	REM_DUP
; PURPOSE:  
;	Function to remove duplicate values from a vector.
;
; CALLING SEQUENCE:
;	result = rem_dup( a, [ flag ] )
;
; INPUTS:
;	a - vector of values from which duplicates are to be found
;	flag - (optional) if supplied then when duplicates occur,
;		the one with the largest value of flag is selected.
;		If not supplied the the first occurence of the value
;		in a is selected.     Should be a vector with the same
;               number of elements as a.
;
; OUTPUT:
;	A vector of subscripts in a is returned.  Each subscript
;	points to a selected value such that a(rem_dup(a,flag))
;	has no duplicates.
;
; SIDE EFFECTS:
;	The returned subscripts will sort the values in a in ascending
;	order with duplicates removed.
;
; EXAMPLES:
;
;	Remove duplicate values in vector a.
;	 	a = a[ rem_dup(a)]
;
;	Remove duplicates in vector WAVE.  When duplicate values
;	are found, select the one with the largest intensity, INTE.
;
;		sub = rem_dup( wave, inte)
;		wave = wave[sub]      
;		inte = inte[sub]
;
; NOTES:
;	The UNIQ function in the User's Library uses a faster algorithm,
;	but has no equivalent of the "flag" parameter.    Also, note that
;       REM_DUP() gives the index of the *first* equal value found, while
;       UNIQ() gives the index of the *last* equal value found.
;
; MODIFICATION HISTORY:
;	D. Lindler  Mar. 87
;	11/16/90 JKF ACC - converted to IDL Version 2.
;	August 1997  -- Changed loop index to type LONG
;	October 1997 -- Also changed NGOOD index to LONG
;       April 2007 - Use faster algorithm when Flag vector not set, W. Landsman
;       Feb 2011 - Remove spurious line W.L.
;       Jan 2012 - Call BSORT() to ensure original order maintained for equal
;            values
;-
;-------------------------------------------------------------------------------
;
 compile_opt idl2
 On_error,2
 npar = N_params()		;number of input parameters supplied
 if npar EQ 0 then begin
 	print,'Syntax -  b = rem_dup( a, [ flag ] )'
 	return, -1
 end

 n = N_elements(a)			;number of values in a
 if n lt 2 then return, lonarr(1)	;only one value in a
 sub = Npar GE 2 ? sort(a) : bsort(a)                      ;sorted subscripts
 aa = a[sub]			;sorted a
;
; loop on aa
;
 val = aa[0]			;first value processed
 if npar GE 2 then begin 
 
 good = lonarr(n)		;values to keep
 ngood = 0L			;number kept.
ff = flag[sub]			;sorted flags
 f = ff[0]			;flag for first value
 for i = 1L, n-1 do begin
	if aa[i] ne val then begin
		val = aa[i]
		f = ff[i]
		ngood++
		good[ngood] = i
	  end else begin
		if ff[i] gt f then begin
			f = ff[i]
			good[ngood] = i
		endif
	endelse
 endfor
  good = good[0:ngood]

 endif else begin
 
   good    = where( shift( aa, 1) NE aa, count)
   if count EQ 0 then good = 0

 endelse
 
 return, sub[good]		;return subscripts in original a
 end

