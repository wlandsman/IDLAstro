pro ftkeeprow,h,tab,subs
;+
; NAME:
;	FTKEEPROW
; PURPOSE:
;	Subscripts (and reorders) a FITS table.  A companion piece to FTDELROW.
;
; CALLING SEQUENCE:
;	ftkeeprow, h, tab, subs
;
; INPUT PARAMETERS:
;	h    = FITS table header array
;	tab  = FITS table data array
;	subs = subscript array of FITS table rows.  Works like any other IDL
;		subscript array (0 based, of course).
;
; OUTPUT PARAMETERS:
;	h and tab are modified
;
; MODIFICATION HISTORY:
;	Written by R. S. Hill, ST Sys. Corp., 2 May 1991.
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
 On_error,2                          ;Return to caller
 
 if N_params() LT 3 then begin
     print,'Syntax - ftkeeprow, h, tab, subs'
     return
 endif

 insize = sxpar(h,'NAXIS2')
 tab = tab[*,subs]
 outsize = N_elements(subs)
 sxaddpar, h, 'NAXIS2', outsize
 tag = 'FTKEEPROW '+systime(0)+': '
 sxaddhist, tag + 'table subscripted', h
 sxaddhist, tag + strtrim(string(insize),2) + ' rows in, ' + $
              strtrim(string(outsize),2) + ' rows out',h

 return
 end
