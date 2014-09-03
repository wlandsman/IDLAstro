;-------------------------------------------------------------
;+
; NAME:
;       YMD2DN
; PURPOSE:
;       Convert from year, month, day to day number of year.
; CATEGORY:
; CALLING SEQUENCE:
;       dy = ymd2dn(yr,m,d)
; INPUTS:
;       yr = year (like 1988).      scalar or vector
;       m = month number (like 11 = Nov).   scalar or vector
;       d = day of month (like 5).        scalar or vector
; KEYWORD PARAMETERS:
; OUTPUTS:
;       dy = day number in year (like 310).  out
; COMMON BLOCKS:
; NOTES:
; MODIFICATION HISTORY:
;       Written by R. Sterner, 20 June, 1985.
;       Johns Hopkins University Applied Physics Laboratory.
;       RES 18 Sep, 1989 --- converted to SUN
;       R. Sterner, 1997 Feb 3 --- Made work for arrays.
;
; Copyright (C) 1985, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;	Converted to IDL V5.0   W. Landsman  2-Jan-1998
;-
;-------------------------------------------------------------
 
	function ymd2dn,yr,m,d, help=hlp
 
	if (n_params(0) lt 3) or keyword_set(hlp) then begin
	  print,' Convert from year, month, day to day number of year.'
	  print,' dy = ymd2dn(yr,m,d)'
	  print,'   yr = year (like 1988).               in'
	  print,'   m = month number (like 11 = Nov).    in'
	  print,'   d = day of month (like 5).           in'
	  print,'   dy = day number in year (like 310).  out'
	  return, -1
	endif
 
	;----  Days before start of each month (non-leap year)  -----
	idays = [0,31,59,90,120,151,181,212,243,273,304,334,366]
 
	;----  Correct for leap year if month ge 3  -------------
	lpyr = (((yr mod 4) eq 0) and ((yr mod 100) ne 0)) $
            or ((yr mod 400) eq 0) and (m ge 3)
 
	dy = d + idays[m-1] + lpyr
	return, dy
 
	end
