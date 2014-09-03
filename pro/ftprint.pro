pro ftprint,h,tab,columns,rows,textout=textout
;+
;  NAME:
;      FTPRINT
;  PURPOSE:
;       Procedure to print specified columns and rows of a FITS table
;
; CALLING SEQUENCE:
;       FTPRINT, h, tab, columns, [ rows, TEXTOUT = ]
;
; INPUTS:
;       h - Fits header for table, string array
;       tab - table array 
;       columns - string giving column names, or vector giving
;               column numbers (beginning with 1).  If string 
;               supplied then column names should be separated by comma's.
;       rows - (optional) vector of row numbers to print.  If
;               not supplied or set to scalar, -1, then all rows
;               are printed.
;
; OUTPUTS:
;       None
;
; OPTIONAL INPUT KEYWORDS:
;       TEXTOUT controls the output device; see the procedure TEXTOPEN
;
; SYSTEM VARIABLES:
;       Uses nonstandard system variables !TEXTOUT and !TEXTOPEN
;       These will be defined (using ASTROLIB) if not already present.
;       Set !TEXTOUT = 3 to direct output to a disk file.   The system
;       variable is overriden by the value of the keyword TEXTOUT
;
; EXAMPLES:
;
;       ftprint,h,tab,'STAR ID,RA,DEC'    ;print id,ra,dec for all stars
;       ftprint,h,tab,[2,3,4],indgen(100) ;print columns 2-4 for 
;                                         ;first 100 stars
;       ftprint,h,tab,text="stars.dat"    ;Convert entire FITS table to
;                                         ;an ASCII file named STARS.DAT
;
; PROCEDURES USED:
;       FTSIZE, FTINFO, TEXTOPEN, TEXTCLOSE
;
; RESTRICTIONS: 
;       (1) Program does not check whether output length exceeds output
;               device capacity (e.g. 80 or 132).
;       (2) Column heading may be truncated to fit in space defined by
;               the FORMAT specified for the column
;       (3) Program does not check for null values
;
; HISTORY:
;       version 1  D. Lindler Feb. 1987
;       Accept undefined values of rows, columns   W. Landsman August 1997
;       New FTINFO calling sequence    W. Landsman   May 2000
;       Parse scalar string with STRSPLIT   W. Landsman  July 2002
;       Fix format display of row number  W. Landsman March 2003
;       Fix format display of row number again  W. Landsman May 2003
;-
; On_error,2
  compile_opt idl2
;
; set defaulted parameters
;
 if N_params() LT 2 then begin
   print,'Syntax -  FTPRINT, h, tab, [ columns, rows, TEXTOUT= ]'
   return
 endif

 defsysv,'!textout',exists = i
 if i EQ 0 then astrolib
 
 if N_elements(columns) EQ 0 then columns = -1
 if N_elements(rows) EQ 0 then rows= -1
 if  not keyword_set(TEXTOUT)  then textout = !TEXTOUT

; make sure rows is a vector

 n = N_elements(rows)
 if n EQ 1 then r = [rows] else r = long(rows)
 ftsize,h,tab,ncols,nrows,tfields,allcols,allrows, ERRMSG = errmsg   ;table size
 if ERRMSG NE '' then message,errmsg
 if r[0] EQ -1 then r = lindgen(nrows)          ;default

 Nr = N_elements(r)
 good = where( (r GE 0) and (r LT nrows), Ngood)
 if Ngood NE Nr then begin
      if Ngood EQ 0 then message,'ERROR - No valid row numbers supplied'
      r = r[good]
 endif
;
; extract column info
;
 title1 = ''
 title2 = ''
 FTINFO,h,ft_str
 
;
; if columns is a string, change it to string array
;
 if size(columns,/TNAME) EQ 'STRING'  then begin 
         colnames = strsplit(columns,',',/EXTRACT) 
         numcol = N_elements(colnames)
        colnames = strupcase(strtrim(colnames,2))
        ttype = strtrim(ft_str.ttype,2)
        colnum = intarr(numcol)
        for i = 0,numcol-1 do begin
             icol = where(ttype EQ colnames[i], Nfound) 
             if Nfound EQ 0 then message, $
               'ERROR - Field ' + colnames[i] + ' not found in FITS ASCII table'
             colnum[i] = icol[0] 
       endfor
   end else begin                       ;user supplied vector
        colnum = fix(columns) -1                ;make sure it is integer
        numcol = N_elements(colnum)     ;number of elements
        if numcol EQ 1 then begin
            if colnum[0] LT 0 then begin 
              colnum = indgen(tfields) & numcol = tfields
        endif & endif
 end

 flen = ft_str.width[colnum]
 colpos = ft_str.tbcol[colnum]
 ttype = strtrim( ft_str.ttype[colnum],2)
 tunit = strtrim( ft_str.tunit[colnum],2)
;
; create header lines
;
  for i=0,numcol-1 do begin
        name = strn(ttype[i],padtype=2,len=flen[i] )
        unit = strn(tunit[i],padtype=2,len=flen[i] ) 
        title1 = title1 + ' ' + name
        title2 = title2 + ' ' + unit
  endfor
;
; open output file
;
 textopen,'FTPRINT',TEXTOUT=textout, MORE_SET = more_set

 ifmt = fix(alog10(max(r)+1)) > 3
 title1 = strn('ROW',padtype=2,len = ifmt) +  title1
 title2 = string(replicate(32b,ifmt+1)) + title2
 ifmt = strtrim(ifmt,2)
;
; loop on rows 
;
 printf,!TEXTUNIT,title1
 printf,!TEXTUNIT,title2
 printf,!TEXTUNIT,' '

  for i = 0, Nr-1 do begin
;
; loop on columns
;
        line = string(r[i],format='(i' + ifmt + ')')     ;print line
        for j = 0,numcol-1 do begin
                cpos=colpos[j]-1                        ;column number
                val = string(tab[cpos:cpos+flen[j]-1,r[i]])
                line = line+' '+ val
        endfor
        printf,!TEXTUNIT,line
        if more_set then if (!ERR EQ 1) then goto, DONE
 endfor
;
; done
;
DONE: 
 textclose,textout=textout

 return
 end
