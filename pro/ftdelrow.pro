pro ftdelrow,h,tab,rows                                               
;+
; NAME:
;       FTDELROW
; PURPOSE:
;       Delete a row of data from a FITS table
;
; CALLING SEQUENCE:
;       ftdelrow, h, tab, rows
;
; INPUTS-OUPUTS
;       h,tab - FITS table header and data array.  H and TAB will
;               be updated on output with the specified row(s) deleted.
;       rows  -  scalar or vector, specifying the row numbers to delete
;               This vector will be sorted and duplicates removed by FTDELROW
;
; EXAMPLE:
;       Compress a table to include only non-negative flux values
;
;       flux = FTGET(h,tab,'FLUX')       ;Obtain original flux vector
;       bad = where(flux lt 0)           ;Find negative fluxes
;       FTDELROW,h,tab,bad               ;Delete rows with negative fluxes
;
; PROCEDURE:
;       Specified rows are deleted from the data array, TAB.  The NAXIS2
;       keyword in the header is updated.
;
; PROCEDURES USED:
;       sxaddpar
;
; REVISION HISTORY:                                           
;       Written   W. Landsman        STX Co.     August, 1988
;       Checked for IDL Version 2, J. Isensee, July, 1990
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Assume since V5.4, use BREAK instead of GOTO  W. Landsman April 2006
;   
;- 
 On_error,2

 if N_params() LT 3 then begin
     print,'Syntax - ftdelrow,h,tab,rows'
     return                                                  
 endif

 nrows = sxpar(h,'NAXIS2')            ;Original number of rows
 if (max(rows) GE nrows) or (min(rows) LT 0) then $
     message,'Specified rows must be between 0 and ' + strtrim(nrows-1,2)

 ndel = N_elements(rows)
 if ndel GT 1 then begin
     rows = rows[rem_dup(rows)]            ;Sort and remove duplicate values
     ndel = N_elements(rows)
 endif

 j = 0L
 i = rows[0]
 for k = long(rows[0]),nrows-1 do begin
 if k EQ rows[j] then begin
     j = j+1 
     if j EQ ndel then BREAK
 endif else begin
     tab[0,i] = tab[*,k]
     i = i+1
 endelse
 
 endfor
 k = k-1

 if k NE nrows-1 then tab[0,i] = tab[*,i+j:nrows-1]
 tab = tab[*,0:nrows-ndel-1]
 sxaddpar,h,'NAXIS2',nrows-ndel      ;Reduce number of rows

 return  
 end
