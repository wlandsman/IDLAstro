pro tbdelcol,h,tab,name                                               
;+
; NAME:
;       TBDELCOL
; PURPOSE:
;       Delete a column of data from a FITS binary table
;
; CALLING SEQUENCE:
;       TBDELCOL, h, tab, name
;
; INPUTS-OUPUTS
;       h,tab - FITS binary table header and data array.  H and TAB will
;               be updated with the specified column deleted
;
; INPUTS:
;       name - Either (1) a string giving the name of the column to delete
;                       or (2) a scalar giving the column number to delete
;
; EXAMPLE:
;       Delete the column "FLUX" from FITS binary table test.fits
;
;       IDL> tab = readfits('test.fits',h,/ext)    ;Read table
;       IDL> tbdelcol, h, tab, 'FLUX'              ;Delete Flux column
;       IDL> modfits,'test.fits',tab,h,/ext        ;Write back table
;
; PROCEDURES USED:
;       SXADDPAR, TBINFO, TBSIZE
; REVISION HISTORY:                                           
;       Written   W. Landsman        STX Co.     August, 1988
;       Use new structure returned by TBINFO,  August, 1997
;       Use SIZE(/TNAME) instead of DATATYPE()   October 2001
;       Use /NOSCALE in call to TBINFO, update TDISP   W. Landsman   March 2007
;- 
 compile_opt idl2
 On_error, 2

 if N_params() LT 3 then begin
     print,'Syntax - tbdelcol, h, tab, name'
     return
 endif

 s = size(name)

 tbsize, h, tab, ncol, nrows, tfields, allcols, allrows

; Make sure column exists

 tbinfo,h,tb_str,/NOSCALE

 case size(name,/TNAME) of
 'STRING': begin
      field = where(tb_str.ttype eq strupcase(name),nfound)
      if nfound eq 0 then $ 
         message,'Field '+strupcase(name) + ' not found in header'
      end
 'UNDEFINED':message,'Third parameter must be field name or number'
 ELSE: begin
      field = name-1
      if (field LT 0 ) or (field GT tfields) then $
            message,'Field number must be between 1 and ' +strtrim(tfields,2)
      end
 endcase

 fname = strtrim(strupcase(name),2)
 field = field[0]

; Eliminate relevant columns from TAB

 tcol = tb_str.tbcol[field] & w = tb_str.width[field]*tb_str.numval[field]

 case 1 of 
        tcol eq 0: tab = tab[w:*,*]                     ;First column
        tcol eq ncol-w: tab = tab[0:tcol-1,*]          ;Last column
        else: tab = [tab[0:tcol-1,*],tab[tcol+w:*,*]]  ;All other columns
 endcase

; Parse the header.  Remove specified keyword from header.  Lower
; the index of subsequent keywords.  Update the TBCOL*** index of
; subsequent keywords

 nlines = N_elements(h)
 field = field + 1
 hnew = strarr(nlines)
 j = 0
 for i = 0,nlines-1 DO BEGIN    ;Loop over each element in header

 key = strupcase(strmid(h[i],0,5))
 if (key eq 'TTYPE') OR (key eq 'TFORM') or (key eq 'TUNIT') or $
   (key eq 'TNULL') or (key EQ 'TDISP') then begin
        row = h[i]                    
        ifield = fix(strtrim(strmid(row,5,3)))    
        if ifield gt field then begin    ;Subsequent field?
                if ifield le 10 then fmt = "(I1,' ')" else fmt ='(I2)'
                strput,row,string(ifield-1,format=fmt),5
        endif 
        if ifield ne field then hnew[j] = row else j=j-1
  endif else hnew[j] = h[i]      

 j = j+1

 endfor  

 sxaddpar,hnew,'TFIELDS',tfields-1 ;Reduce number of fields by 1
 sxaddpar,hnew,'NAXIS1',ncol-w ;Reduce num. of columns by WIDTH

 h = hnew[0:j-1]

 message,'Field '+fname+' has been deleted from the FITS table',/INF

 return  
 end
