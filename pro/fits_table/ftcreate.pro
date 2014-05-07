pro ftcreate, MAXCOLS,MAXROWS,H,TAB
;+
; NAME:
;       FTCREATE
; PURPOSE:
;       Create a new (blank) FITS ASCII table and header with specified size.
;
; CALLING SEQUENCE:
;       ftcreate, maxcols, maxrows, h, tab
;
; INPUTS:
;       maxcols - number of character columns allocated, integer scalar
;       maxrows - maximum number of rows allocated, integer scalar
;
; OUTPUTS:
;       h - minimal FITS Table extension header, string array
; OPTIONAL OUTPUT:
;       tab - empty table, byte array 
; HISTORY:
;       version 1  D. Lindler   July. 87
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Make table creation optional, allow 1 row table, add comments to 
;       required FITS keywords    W. Landsman    October 2001  
;-
;----------------------------------------------------------------------
 On_error,2

 if n_params() lt 3 then begin
      print,'Syntax - FTCREATE, maxcols, maxrows, h, [tab]'
      return
 endif

; Create blank table if tab output variable supplied 

 if N_params() GE 4 then begin
            tab = replicate(32B, maxcols, maxrows)
            if maxrows EQ 1 then tab = reform(tab,maxcols,1)
 endif
;
; Create header (destroy any previous contents) and add required ASCII table 
; keywords
;
 h = strarr(9) + string(' ',format='(a80)')
 h[0] = 'END' + string(replicate(32b,77))
 sxaddpar, h, 'XTENSION', 'TABLE   ',' ASCII table extension'
 sxaddpar, h, 'BITPIX', 8,' 8 bit bytes'
 sxaddpar, h, 'NAXIS', 2,' 2-dimensional ASCII table'
 sxaddpar, h, 'NAXIS1', 0,' Width of table in bytes'
 sxaddpar, h, 'NAXIS2', 0,' Number of rows in table'
 sxaddpar, h, 'PCOUNT', 0,' Size of special data area'
 sxaddpar, h, 'GCOUNT', 1,' one data group (required keyword)
 sxaddpar, h, 'TFIELDS', 0,' Number of fields in each row'

 return
 end
