pro tbprint,hdr_or_tbstr,tab,columns,rows,textout=textout,fmt=fmt, $
            num_header_lines=num_header_lines,nval_per_line=nval_per_line
;+
; NAME:
;       TBPRINT
;  PURPOSE:
;       Procedure to print specified columns & rows of a FITS binary table
;
; CALLING SEQUENCE:
;       TBPRINT, h, tab, columns, [ rows, TEXTOUT =, FMT=, NUM_HEADER= ]
;               or
;       TBPRINT,tb_str, tab, columns, [ rows, TEXTOUT =, FMT=, NUM_HEADER =  ]
;
; INPUTS:
;       h - FITS header for table, string array
;                       or
;       tb_str - IDL structure extracted from FITS header by TBINFO, useful 
;           when TBPRINT is called many times with the same header
;       tab - table array 
;       columns - string giving column names, or vector giving
;               column numbers (beginning with 1).  If string 
;               supplied then column names should be separated by comma's.
;               If set to '*' then all columns are printed in table format 
;               (1 row per line, binary tables only).
;       rows - (optional) vector of row numbers to print.  If
;               not supplied or set to scalar, -1, then all rows
;               are printed.
;
; OUTPUTS:
;       None
; OPTIONAL INPUT KEYWORDS:
;       FMT = Format string for print display.   If not supplied, then any 
;               formats in the TDISP keyword fields of the table will be
;               used, otherwise IDL default formats.   
;       NUM_HEADER_LINES - Number of lines to display the column headers 
;               default = 1).  By setting NUM_HEADER_LINES to an integer larger
;               than 1, one can avoid truncation of the column header labels.  
;               In addition, setting NUM_HEADER_LINES will display commented 
;               lines indicating a FORMAT for reading the data, and a 
;               suggested call to  readfmt.pro.
;       NVAL_PER_LINE - The maximum number of values displayed from a multivalued
;               column when printing in table format.   Default = 6
;       TEXTOUT - scalar number (0-7) or string (file name) determining
;               output device (see TEXTOPEN).  Default is TEXTOUT=1, output 
;               to the user's terminal    
; SYSTEM VARIABLES:
;       Uses nonstandard system variables !TEXTOUT and !TEXTOPEN
;       Set !TEXTOUT = 3 to direct output to a disk file.   The system
;       variable is overriden by the value of the keyword TEXTOUT
;
; EXAMPLES:
;       tab = readfits('test.fits',htab,/ext) ;Read first extension into vars
;       tbprint,h,tab,'STAR ID,RA,DEC'    ;print id,ra,dec for all stars
;       tbprint,h,tab,[2,3,4],indgen(100) ;print columns 2-4 for 
;                                          first 100 stars
;       tbprint,h,tab,text="stars.dat"    ;Convert entire FITS table to
;                                         ;an ASCII file named 'stars.dat'
;
; PROCEDURES USED:
;       GETTOK(), STRNUMBER(), TEXTOPEN, TEXTCLOSE, TBINFO
;
; RESTRICTIONS: 
;       (1) Program does not check whether output length exceeds output
;               device capacity (e.g. 80 or 132).
;       (2) Column heading may be truncated to fit in space defined by
;               the FORMAT specified for the column.    Use NUM_HEADER_LINES
;               to avoid truncation.
;       (3) Program does not check for null values
;       (4) Does not work with variable length columns
;       (5) Will only the display the first value of fields with multiple values
;        (unless there is one row each with the same number of mulitple values)
;        If printing in table format (column='*') then up to 6 values
;        can be printed per line.
;
; HISTORY:
;       version 1  D. Lindler Feb. 1987
;       Accept undefined values of rows,columns W. Landsman  August 1997
;       Use new structure returned by TBINFO    W. Landsman  August 1997
;       Made formatting more robust    W. Landsman   March 2000
;       Use STRSPLIT to parse string column listing W. Landsman July 2002
;       Wasn't always printing last row   W. Landsman  Feb. 2003
;       Better formatting (space between columns) W. Landsman Oct. 2005
;       Use case-insensitive match with TTYPE, use STRJOIN W.L. June 2006
;       Fixed check for multiple values W.L. August 2006
;       Fixed bad index value in August 2006 fix  W.L Aug 15 2006
;       Free-up pointers after calling TBINFO  W.L. Mar 2007
;       Add table format capability  W.L. Mar 2010
;       Add NUM_HEADER_LINE keyword  P. Broos Apr 2010
;-
 On_error,2
 compile_opt idl2

 if N_params() LT 2 then begin
   print,'Syntax -  TBPRINT, h, tab, [ columns, rows, device, '
   print,'              TEXTOUT= ,FMT=, NUM_HEADER_LINES= '
   return
 endif

; set default parameters

 if N_elements(columns) EQ 0 then columns = -1
 if N_elements(rows) EQ 0 then rows= -1
 if ~keyword_set(textout) then textout = 1
 if N_elements(nval_per_line) EQ 0 then $
     nval_per_line = 6     ;Number of  values that can be displayed in 'table' format
 
 nbytes = [1,2,4,4,8,8,1,0,16]
 fmt_def = ['','I4','I8','I12','G13.6','G16.8','','A','','','','']

; make sure rows is a vector

 sz = size(tab)
 nrows = sz[2]
 r = long(rows)
 if r[0] eq -1 then r = lindgen(nrows)          ;default
 n = N_elements(r)
 dotable = n EQ 1         ;Print in table format?

; Did user supply a FITS header, or a structure (output of tbinfo)?

 case  size(hdr_or_tbstr,/type) of 
 7: tbinfo,hdr_or_tbstr,tb_str
 8: tb_str = hdr_or_tbstr
 else: message,'ERROR - Invalid FITS header or structure supplied' 
 endcase 
 
 tfields = N_elements(tb_str.ttype)

; if columns is a string, change it to string array

 if size(columns,/tname) eq 'STRING' then begin
        if columns[0] EQ '*' then begin       
	colnum = indgen(tfields) + 1 
	numcol = tfields
	dotable = 1 
	endif else begin 
        colnames = strsplit(columns,',',/extract) 
        numcol = N_elements(colnames) 
        colnum = intarr(numcol)
        field = strupcase(colnames)
        for i = 0,numcol-1 do begin 
        colnum[i] = where(strupcase(tb_str.ttype) EQ field[i],nfound) + 1
        if nfound EQ 0 then $ 
           message,'Field '+ field[i] + ' not found in header'
       endfor
       endelse
   endif else begin                       ;user supplied vector
        colnum = fix(columns)           ;make sure it is integer
        if colnum[0] eq -1 then colnum = indgen(tfields) + 1 
        numcol = N_elements(colnum)     ;number of elements
 endelse

 if ~keyword_set(fmt) then form = tb_str.tdisp[colnum-1] else begin
        if N_elements(fmt) EQ 1 && (numcol GT 1) then begin
                temp = strupcase(strtrim(fmt,2))
                if strmid(temp,0,1) EQ '(' then $
                        temp = strmid(temp,1,strlen(temp)-2)
                        form = strarr(numcol)
                        ifmt = 0
                         while strtrim(temp,2) NE ''  do begin
                                tstform = gettok(temp,',')
                                ndup = 1
                                vtype = strmid(tstform,0,1)
                                if strnumber(vtype,val) then begin
                                        ndup = val
                                        tstform = strmid(tstform,1,100)
                                endif
                                if strpos(tstform,'X') LT 0 then begin
                                     form[ifmt:ifmt+ndup-1]=tstform
                                     ifmt += ndup
                                endif
                        endwhile
        endif else form = fmt
 endelse

 default = where(form EQ '',Ndef)
 if Ndef GT 0 then form[default] = fmt_def[ tb_str.idltype[colnum[default]-1] ]
  form = strtrim(form,2)
 row_format = strjoin(form,',1x,')

 num = where(tb_str.idltype[colnum-1] NE 7, Nnumeric)
 if Nnumeric GT 0 then minnumval = min(tb_str.numval[colnum[num]-1]) $
 else minnumval = 1

 if (minnumval GT 1) then begin 
        if rows[0] NE -1 then nrow1 = N_elements(rows)-1 else begin
                rows = lindgen(minnumval)
                nrow1 = minnumval-1
        endelse
        
 endif

 textopen,'TBPRINT', TEXTOUT = textout

 field = tb_str.ttype[colnum-1]
  fieldlen = strlen(field)

;Print in table format?
  dotable = dotable || (n EQ 1)  && (minnumval LE nval_per_line)   
  if dotable then begin 
  maxlen = max(fieldlen)
  
  for j = 0, n-1 do begin 
  printf,!TEXTUNIT,'ROW: ',r[j]
  for i = 0, numcol-1 do begin
      val =  tbget(tb_str,tab,colnum[i],r[j])
      nval = N_elements(val)
      if nval GT 1 then begin            ;Print up to 5 values
           val = strcompress(strjoin(val[0:(nval-1)< (nval_per_line-1)],' '))
	   if nval GT nval_per_line then val = val + '...'
      endif	   
      printf,!TEXTUNIT, colnum[i],') ', field[i],strtrim(string(val,/pr),2),$
          f='(i3,A,A-' + strtrim(maxlen+2,2) + ',A)'
  endfor
     printf,!TEXTUNIT, ' '
  endfor

  endif else begin     
 

 varname = 'v' + strtrim(sindgen(numcol)+1,2)
 len = lonarr(numcol)
 varstr = varname + '[0]'
 xform = '(' + form + ')'
 for i = 0,numcol-1 do begin
        result = execute(varname[i] + '= tbget(tb_str,tab,colnum[i],r)' )
        result = execute('len[i] = strlen(string(' + varstr[i] + ',f=xform[i]))')
 endfor
 
 
 if keyword_set(num_header_lines) then begin
   ;; Build a multi-line header showing the column names left-justified.
   header = strarr(num_header_lines+1)
   
; The printed data columns are separated by a space, so the column widths are actually (len+1).
   column_width = len + 1
   for ii=0,numcol-1 do begin
     header_ind = ii MOD num_header_lines
     
     ; Pad the start of the header lines as needed.
     if ((ii GT 0) && (ii LT num_header_lines)) then header[header_ind] += string(replicate(32B, total(column_width[0:ii-1], /INT)))
     
     if ((ii+num_header_lines) LT numcol) then begin
       ; The space we have to print this label is the width of the next num_header_lines columns, minus one space for the '|' separator..
       ; Put the label at the LEFT end of this space.
       label_length = total(column_width[ii : ii+num_header_lines-1], /INT) - 1
       label_format_code  = string(label_length, F='(%"|%%-%ds")')
     endif else begin
       ; We're at the end of the header line, so print this last label without truncation.
       label_format_code  = '|%s'
     endelse
     header[header_ind] += string(field[ii], F='(%"'+label_format_code+'")')
   endfor ; ii
   
   printf,!TEXTUNIT, "# FORMAT='" + row_format + "'"
   printf,!TEXTUNIT, 3+num_header_lines+1, strjoin(field,','), F='(%"# readfmt, ''table.txt'', SKIPLINE=%d, FORMAT, %s")' 
   printf,!TEXTUNIT, "#"

   header[num_header_lines] = string(replicate(byte('-'), max(strlen(header))))
   strput, header, '#', 0
   forprint, TEXTOUT=5, header, /NoComment
   
 endif else begin
   ;; Build a single-line header showing the column names centered on the columns.
   field = strtrim(tb_str.ttype[colnum-1],2)
   fieldlen = strlen(field)
   for i=0,numcol-1 do begin
          if fieldlen[i] LT len[i] then begin
            space = len[i] - fieldlen[i]
      if space EQ 1 then field[i] = field[i]+ ' ' else begin
                     pad = string(replicate(32b,space/2))
                     field[i] = pad + field[i] + pad
         if space mod 2 EQ 1 then field[i] = field[i] + ' '
      endelse   
          endif else field[i] = strmid(field[i],0,len[i])
   endfor
   printf,!TEXTUNIT,field
 endelse
 
 
 if size(hdr_or_tbstr,/TYPE) NE 8  then begin
       ptr_free, tb_str.tscal
       ptr_free, tb_str.tzero
 endif



; If there are multiple values then only print the first value....

  if minnumval EQ 1 then begin        
       index = replicate('[i]',numcol)
       g = where( tb_str.numval[colnum-1] GT 1,Ng) 
       if Ng GT 0 then index[g] = '[0,i]'  
       vstring  = strjoin(varname + index,',')
  endif else  vstring = strjoin(varname + '[i]',',') 

 row_format = '(' + row_format + ')'

 if minnumval EQ 1 then $
 result = execute('for i=0,n-1 do printf,!TEXTUNIT,' +  $
                   vstring + ',f=row_format') else $
 result = execute('for i=rows[0],rows[nrow1] do printf,!TEXTUNIT,' +  $
                   vstring + ',f=fmt') 
 endelse		   
 textclose, TEXTOUT = textout
 return
 end
