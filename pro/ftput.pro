pro ftput,h,tab,field,row,values,nulls
;+
; NAME:
;       FTPUT
; PURPOSE:
;       Procedure to add or update a field in an FITS ASCII table
; CALLING SEQUENCE:
;       FTPUT, htab, tab, field, row, values, [ nulls ]
;
; INPUTS:
;       htab - FITS ASCII table header string array
;       tab - FITS ASCII table array (e.g. as read by READFITS)
;       field - string field name or integer field number
;       row -  either a non-negative integer scalar giving starting row to 
;               update, or a non-negative integer vector specifying rows to 
;               update.   FTPUT will append a new row to a table if the value 
;               of 'row' exceeds the number of rows in the tab array    
;       values - value(s) to add or update.   If row is a vector
;               then values must contain the same number of elements.
;
; OPTIONAL INPUT:
;       nulls - null value flag of same length as values.
;               It should be set to 1 at null value positions
;               and 0 elsewhere.
;
; OUTPUTS:
;       htab,tab will be updated as specified.
;
; EXAMPLE:
;       One has a NAME and RA  and Dec vectors for 500 stars with formats A6,
;       F9.5 and F9.5 respectively.   Write this information to an ASCII table 
;       named 'star.fits'.
;
;       IDL> FTCREATE,24,500,h,tab       ;Create table header and (empty) data
;       IDL> FTADDCOL,h,tab,'RA',8,'F9.5','DEGREES'   ;Explicity define the
;       IDL> FTADDCOL,h,tab,'DEC',8,'F9.5','DEGREES'  ;RA and Dec columns
;       IDL> FTPUT,h,tab,'RA',0,ra       ;Insert RA vector into table
;       IDL> FTPUT,h,tab,'DEC',0,dec       ;Insert DEC vector into table
;       IDL> FTPUT, h,tab, 'NAME',0,name   ;Insert NAME vector with default
;       IDL> WRITEFITS,'stars.fits',tab,h ;Write to a file
;   
;      Note that (1) explicit formatting has been supplied for the (numeric)
;      RA and Dec vectors, but was not needed for the NAME vector, (2) A width
;      of 24 was supplied in FTCREATE based on the expected formats (6+9+9),
;      though the FT* will adjust this value as necessary, and (3) WRITEFITS
;      will create a minimal primary header  
; NOTES:
;       (1) If the specified field is not already in the table, then FTPUT will
;       create a new column for that field using default formatting.   However,
;        FTADDCOL should be called prior to FTPUT for explicit formatting.
;
; PROCEDURES CALLED
;       FTADDCOL, FTINFO, FTSIZE, SXADDPAR, SXPAR()
; HISTORY:
;       version 1  D. Lindler July, 1987
;       Allow E format         W. Landsman          March 1992
;       Write in F format if E format will overflow    April 1994
;       Update documentation W. Landsman   January 1996
;       Allow 1 element vector  W. Landsman   March 1996
;       Adjust string length to maximum of input string array   June 1997
;       Work for more than 32767 elements August 1997
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Updated call to the new FTINFO   W. Landsman   May 2000
;       Fix case where header does not have any columns yet W.Landsman Sep 2002
;       Assume since V5.2, omit fstring() call  W. Landsman April 2006
;-
 On_error,2
 compile_opt idl2

 if N_params() LT 5 then begin
    print,'Syntax - FTPUT, htab, tab, field, row, values, [nulls]'
    return
 endif

 nrow = N_elements(row)        ;Number of elements in row vector

 nullflag = N_elements(nulls) GT 0         ;Null values supplied?

 ftsize,h,tab,ncols,nrows,tfields,allcols,allrows     ; Get size of table

; Make values a vector if scalar supplied

 s = size(values) & ndim = s[0] & type = s[ndim+1]

 if ndim gt 1 then $
        message,'Input values must be scalar or 1-D array'

 sz_row = size(row)
 scalar = sz_row[0] EQ 0

 v = values
 if nullflag then nullvals = nulls

; Get info on field specified

 ftinfo,h,ft_str, Count = tfields
 if tfields EQ 0 then ipos = -1 else begin
  if size(field,/TNAME) EQ 'STRING' then begin
    field = strupcase(strtrim(field,2))
    ttype = strtrim(ft_str.ttype,2)
    ipos = where(ttype EQ field, Npos)
 endif else ipos = field -1
 endelse

 if ipos[0] EQ -1 then begin            ;Does it exist?

; Add new column if it doesn't exist

          if type EQ 7 then type = (-max(strlen(v)))
          ftaddcol, h, tab, field, type
          ftinfo,h,ft_str
          ftsize,h,tab,ncols,nrows,tfields,allcols,allrows
          ipos = tfields-1
 endif 

 ipos = ipos[0]
 tbcol = ft_str.tbcol[ipos]-1                   ;IDL starts at zero not one.

; Convert input vector to string array

 n = N_elements(v)
 data = string(replicate(32b, ft_str.width[ipos], n ) )
 if nrow GT 1 then if (nrow NE n) then $
        message,'Number of specified rows must equal number of values'

 fmt = strupcase(strtrim(ft_str.tform[ipos],2))
 fmt1 = strmid(fmt,0,1)
 if (fmt1 EQ 'D') or (fmt1 EQ 'E') then begin  ;Need at least 6 chars for E fmt
        point = strpos(fmt,'.')
        wid = fix(strmid(fmt,1,point-1))
        decimal = fix(strmid(fmt,point+1,1000))
        if wid-decimal LT 6 then fmt = 'F' + strmid(fmt,1,1000)
 endif
 fmt = '(' + fmt + ')'
 data = string(v, FORMAT = fmt)

; insert null values

 if nullflag GT 5 then begin
        bad = where(nullvals, Nbad)
        if Nbad GT 0 then for i = 0L, Nbad-1 do data[bad[i]] = tnull
 end

;
; Do we need to increase the number of rows in the table?
;
if scalar then maxrow = row+n else maxrow = max(row) + 1
if maxrow GT allrows then begin         ;expand table size

    ;
    ;  Create a replacement table with the required number of rows.
    ;
    newtab = replicate(32b,allcols,maxrow)
    newtab[0,0] = tab

    ;
    ;  Move the new table into the old table.
    ;
    tab = newtab

end
 if maxrow GT nrows then sxaddpar,h,'naxis2',maxrow

;
;  Now insert into table.
;
  if scalar then tab[tbcol,row] = byte(data) $
  else for i = 0L,N_elements(row)-1 do tab[tbcol,row[i]] = byte(data[i])

;
;  Return to calling routine.
;
 return
 end
