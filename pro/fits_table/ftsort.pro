pro ftsort,h,tab,hnew,tabnew,field, reverse = revers
;+
; NAME:
;      FTSORT
; PURPOSE:
;      Sort a FITS ASCII table according to a specified field
;
; CALLING SEQUENCE:
;      FTSORT,h,tab,[field, REVERSE = ]               ;Sort original table header and array
;               or
;      FTSORT,h,tab,hnew,tabnew,[field, REVERSE =]   ;Create new sorted header
;
; INPUTS:
;      H - FITS header (string array)
;      TAB - FITS table (byte array) associated with H.  If less than 4
;               parameters are supplied, then H and TAB will be updated to 
;               contain the sorted table
;
; OPTIONAL INPUTS:
;      FIELD - Field name(s) or number(s) used to sort the entire table.  
;              If FIELD is a vector then the first element is used for the 
;              primary sort, the second element is used for the secondary
;              sort, and so forth.   (A secondary sort only takes effect when
;              values in the primary sort  field are equal.)  Character fields
;              are sorted using the ASCII collating sequence.  If omitted,
;              the user will be prompted for the field name.
;
; OPTIONAL OUTPUTS:
;      HNEW,TABNEW - Header and table containing the sorted tables
;
; EXAMPLE:
;      Sort a FITS ASCII table by the 'DECLINATION' field in descending order
;      Assume that the table header htab, and array, tab, have already been
;      read (e.g. with READFITS or FITS_READ):

;      IDL> FTSORT, htab, tab,'DECLINATION',/REVERSE
; OPTIONAL INPUT KEYWORD:
;       REVERSE - If set then the table is sorted in reverse order (maximum
;              to minimum.    If FIELD is a vector, then REVERSE can also be
;              a vector.   For example, REVERSE = [1,0] indicates that the
;              primary sort should be in descending order, and the secondary
;              sort should be in ascending order.
;
; EXAMPLE:
; SIDE EFFECTS:
;       A HISTORY record is added to the table header.
; REVISION HISTORY:
;      Written W. Landsman                         June, 1988
;      Converted to IDL V5.0   W. Landsman   September 1997
;      New FTINFO calling sequence, added REVERSE keyword, allow secondary sorts
;                  W. Landsman   May 2000
;-
 On_error,2 
 npar = N_params()
 if npar lt 2 then begin
        print,'Syntax:  ftsort, h, tab, [ field ]'
        print,'    OR:  ftsort,h,tab,hnew,tabnew,[field]'
        return
 endif

 if npar eq 3 then field = hnew 

 nf = N_elements(field)
 nr = N_elements(revers)
 if nr EQ 0 then revers = bytarr(nf) else $
 if nr LT nf then revers = [revers,bytarr(nf-nr)]

 ftinfo,h,ft_str
 key = ftget(ft_str,tab, field[nf-1])
 index = sort(key)
 if revers[nf-1] then index = reverse(index)
 tabnew = tab[*,index]


 if nf GT 1 then begin 
 for i= nf-2,0 do begin
    key = ftget(ft_str,tabnew,field[i])
    index = bsort(key,reverse=revers[i])
    tabnew = tabnew[*,index]
 endfor
 endif

 str = strtrim(field[0],2)
 if nf GT 1 then begin
      for i = 1,nf-1 do str = str + ',' + strtrim( field[i],2) 
      str = 'Keywords: ' + str
 endif else str = 'Keyword: ' + str
 if npar ge 4 then begin
        hnew = h
        sxaddhist,'FTSORT: '+ systime() +' Sort ' + str,hnew
 endif else begin
        tab = tabnew
        sxaddhist,'FTSORT: '+ systime() +' Sort ' + str,h
 endelse

 return
 end
