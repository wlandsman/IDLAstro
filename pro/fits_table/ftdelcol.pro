pro ftdelcol,h,tab,name                                               
;+
; NAME:
;	FTDELCOL
; PURPOSE:
;	Delete a column of data from a FITS table
;
; CALLING SEQUENCE:
;	ftdelcol, h, tab, name
;
; INPUTS-OUPUTS
;	h,tab - FITS table header and data array.  H and TAB will
;		be updated with the specified column deleted
;
; INPUTS:
;	name - Either (1) a string giving the name of the column to delete
;		or (2) a scalar giving the column number to delete
;
; EXAMPLE:
;	Suppose it has been determined that the F7.2 format used for a field
;	FLUX in a FITS table is insufficient.  The old column must first be 
;	deleted before a new column can be written with a new format.
;
;	flux = FTGET(h,tab,'FLUX')       ;Save the existing values
;	FTDELCOL,h,tab,'FLUX'            ;Delete the existing column            
;	FTADDCOL,h,tab,'FLUX',8,'F9.2'   ;Create a new column with larger format
;	FTPUT,h,tab,'FLUX',0,flux        ;Put back the original values
;
; REVISION HISTORY:                                           
;	Written   W. Landsman        STX Co.     August, 1988
;	Adapted for IDL Version 2, J. Isensee, July, 1990
;	Converted to IDL V5.0   W. Landsman   September 1997
;       Updated call to new FTINFO   W. Landsman  May 2000
;- 
; On_error,2

 if N_params() LT 3 then begin
     print,'Syntax - FTDELCOL, h, tab, name'
     return
 endif

 ftsize,h,tab,ncol,nrows,tfields,allcols,allrows

; Make sure column exists

 ftinfo, h, ft_str     ;Get starting column and width (in bytes)
 sz = size(name)
 if ((sz[0] ne 0) or (sz[1] EQ 0)) then $
      message,'Invalid field specification, it must be a scalar'

 if sz[1] EQ 7 then begin        ;If a string, get the field number
    ttype = strtrim(ft_str.ttype,2)
    field = where(ttype EQ strupcase(strtrim(name,2)), Npos) + 1
    if Npos EQ 0 then message, $ 
        'Specified field ' + strupcase(strtrim(field,2)) + ' not in table'
 endif
 

; Eliminate relevant columns from TAB

 field = field[0]
 tbcol = ft_str.tbcol[field-1]-1                     ;Convert to IDL indexing
 width = ft_str.width[field-1]
 case 1 of 
 tbcol eq 0: tab = tab[width:*,*]                     ;First column
 tbcol eq ncol-width: tab = tab[0:tbcol-1,*]          ;Last column
 else: tab = [tab[0:tbcol-1,*],tab[tbcol+width:*,*]]  ;All other columns
 endcase

; Parse the header.  Remove specified keyword from header.  Lower
; the index of subsequent keywords.  Update the TBCOL*** index of
; subsequent keywords

 nh = N_elements(h)
 hnew = strarr(nh)
 j = 0
 key = strupcase(strmid(h,0,5))
 for i= 0,nh-1 do begin    ;Loop over each element in header
 if (key[i] eq 'TTYPE') OR (key[i] eq 'TFORM') or (key[i] eq 'TUNIT') or $
    (key[i] eq 'TNULL') OR (key[i] eq 'TBCOL') then begin
    row = h[i]                    
    ifield = fix(strtrim(strmid(row,5,3)))    
    if ifield GT field then begin    ;Subsequent field?
      if ifield le 10 then fmt = "(I1,' ')" else fmt ='(I2)'
      strput,row,string(ifield-1,format=fmt),5
      if key[i] eq 'TBCOL' then begin
         value = fix(strtrim(strmid(row,10,20)))-width
         v = string(value)
         s = strlen(v)
         strput,row,v,30-s                  ;Right justify
      endif
   endif 
   if ifield ne field then hnew[j] = row else j=j-1

 endif else hnew[j] = h[i]      

 j = j+1
 endfor   

 sxaddpar,hnew,'TFIELDS',tfields-1 ;Reduce number of fields by 1
 sxaddpar,hnew,'NAXIS1',ncol-width ;Reduce num. of columns by WIDTH

 h = hnew[0:j-1]
 message,'Field '+strupcase(name)+' has been deleted from the FITS table',/INF

 return  
 end
