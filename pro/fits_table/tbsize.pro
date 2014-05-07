pro tbsize, h, tab, ncols, nrows, tfields, ncols_all, nrows_all
;+
; NAME:
;       TBSIZE
;
; PURPOSE:
;       Procedure to return the size of a FITS binary table.
;
; CALLING SEQUENCE:
;       tbsize, h, tab, ncols, nrows, tfields, ncols_all, nrows_all
;
; INPUTS:
;       h - FITS table header
;       tab - FITS table array
;
; OUTPUTS:
;       ncols - number of characters per row in table
;       nrows - number of rows in table
;       tfields - number of fields per row
;       ncols_all - number of characters/row allocated (size of tab)
;       nrows_all - number of rows allocated
; PROCEDURES USED:
;       SXPAR()
; HISTORY
;       D. Lindler  July, 1987
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Remove obsolete !ERR call   W. Landsman   May 2000
;-
;------------------------------------------------------------------------
 On_error,2

; check for valid header type

 s=size(h) & ndim=s[0] & type=s[ndim+1]
 if (ndim NE 1) or (type ne 7) then $
        message,'Invalid FITS header, it must be a string array'

; check for valid table array

 s = size(tab) & ndim = s[0] & type = s[ndim+1]
 if (ndim gt 2) or (type ne 1) or (ndim lt 1) then $
        message,'Invalid table array, it must be a 2-D byte array'

 ncols_all = s[1]                       ;allocated characters per row
 nrows_all = s[2]                       ;allocated rows

;
; get number of fields
;
 tfields = sxpar( h, 'TFIELDS', Count = N_tfields )
 if N_tfields EQ 0 then $
        message,'Invalid FITS table header, TFIELDS keyword missing'

;
; get number of columns and rows
;
 ncols = sxpar(h, 'NAXIS1' )
 nrows = sxpar(h, 'NAXIS2' )
 if ( ncols GT ncols_all ) or ( nrows GT nrows_all ) then message, $
  'WARNING - Size information in header does not match that in array',/CON

 return
 end
