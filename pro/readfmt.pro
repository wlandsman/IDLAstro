pro readfmt,name,fmt,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15, $
                 v16,v17,v18,v19,v20,v21,v22,v23,v24,v25, $
                 SILENT = silent, DEBUG = debug, SKIPLINE = skipline, $
                 NUMLINE = numline
;+
; NAME:
;     READFMT
; PURPOSE:
;       Quickly read a fixed format ASCII data file into IDL variables. 
; EXPLANATION:
;       Lines of data not meeting the specified format (e.g. comments) are
;       ignored.  
;      
;       To read a free format ASCII data file use the procedures 
;       READCOL or RDFLOAT.     To print (formatted or free) columns of data
;       use the procedure FORPRINT.   
;
; CALLING SEQUENCE:
;       READFMT, name, fmt, v1,[ v2, v3, v4, ..., v25 , 
;                          /SILENT, /DEBUG, SKIPLINE= , NUMLINE =]
;
; INPUTS:
;       NAME - Name of ASCII data file.  An extension of .DAT is assumed,
;               if not supplied.
;       FMT - scalar string containing a valid FORTRAN read format.
;               Must include a field length specification.   Cannot include
;               internal parenthesis.  A format field must be included for 
;               each output vector.   Multiple format fields are allowed, but
;               the repetition factor must be less than 100, (.i.e. 19X is 
;               allowed but 117X is illegal) 
;
;       Examples of valid FMT values are
;               FMT = 'A7,3X,2I4'  or FMT = '1H ,5I7,2A7'
;       Examples of INVALID FMT values are
;               FMT = 'A7,B3'           ;'B' is not a valid FORTRAN format
;               FMT = 'A7,2(I3,F5.1)'   ;Internal parenthesis not allowed
;               FMT = 'A7,F,I'          ;Field length not included
;
; OUTPUTS:
;       V1,V2,V3,V4... - IDL vectors to contain columns of data.
;               Up to 25 output vectors may be read.  The type of the output 
;               vectors are specified by FMT.
;
; OPTIONAL KEYWORD INPUTS:
;       /SILENT - If this keyword is set and non-zero, then certain terminal
;               output is suppressed while reading the file
;       /DEBUG - Set this keyword to display additional information while
;               reading the file.
;       SKIPLINE - Scalar specifying number of lines to skip at the top of
;               file before reading. Default is to start at first line
;       NUMLINE - Scalar specifying number of lines in the file to read.
;               Default is to read the entire file 
;
; EXAMPLES:
;       Each row in a fixed-format file POSITION.DAT contains a 5 character 
;       star name  and 6 columns of data giving an RA and Dec in sexagesimal 
;       format.   A possible format for such data might be
;
;       IDL> FMT = 'A5,2I3,F5.1,2x,3I3'    
;       and the file could be quickly read with
;
;       IDL> READFMT,'POSITION', fmt, name, hr, min, sec, deg, dmin, dsec 
;    
;       NAME will be a string vector,SEC will be a floating point vector, and
;       the other vectors will be of integer type.
;
; RESTRICTIONS:
;       This procedure is designed for generality and not for speed.
;       If a large ASCII file is to be read repeatedly, it may be worth
;       writing a specialized reader.
;
; NOTES:
;       When reading a field with an integer format I<n>, the output vector is
;               byte  - if n = 1
;               integer*2 - if 1 < n < 5
;               integer*4  - in all other cases
;       Octal ('O') and hexadecimal ('Z') formats are read into longwords
;
; PROCEDURE CALLS:
;       GETTOK(), REMCHAR, ZPARCHECK
;
; REVISION HISTORY:
;       Written         W. Landsman                 November, 1988
;       Added SKIPLINE and NUMLINE keywords         March 92
;       Allow up to 25 columns to be read           June 92
;       Call NUMLINES() function                    Feb 1996
;       Recognize 'O' and 'Z' formats  W. Landsman   September 1997
;       Recognize 'G' format, use SKIP_LUN   W. Landsman  May 2010
;-
  On_error,2
  compile_opt idl2

  if N_params() LT 3 then begin
      print,'Syntax - readfmt, name, fmt, v1,[ v2, v3, v4...v25, '
      print,'         /SILENT, /DEBUG, SKIPLINE =, NUMLINE = ]'
      return
  endif

  zparcheck, 'READFMT', fmt, 2, 7, 0, 'FORMAT string'

; Get number of lines in file 

   nlines = FILE_LINES( name )
 
  if ~keyword_set( SKIPLINE ) then skipline = 0
  if keyword_set( NUMLINE) then nlines = numline < nlines else $
               nlines = nlines - skipline
 
  if nlines LE 0 then begin
        message,'ERROR - File ' + name+' contains no valid data',/CON
	return
   endif   
  ncol = N_params() - 2           ;Number of columns of data expected
  ii = strtrim(indgen(ncol)+1,2)
  frmt = strtrim( strupcase(fmt), 2 )      ;Working FORMAT string

; If format string is of the form "$(...)"  then remove dollar sign and
; parenthesis 

  remchar, frmt, '$'                      ;Remove dollar sign
  if strmid(frmt,0,1) EQ '(' then $
          frmt = strmid( frmt,1,strlen(frmt)-1 )

  if strmid(frmt,strlen(frmt)-1,1) EQ ')' then $
          frmt = strmid(frmt,0,strlen(frmt)-1 )

  fmt1 = '(' + frmt + ')'              ;Now make a valid read format


; Create output arrays according to specified formats

   k = 0L                             ;Loop over output columns
  REPEAT BEGIN

    fmt_1 = gettok(frmt,',')
    vtype = strmid( fmt_1, 0, 1)
    ndup = 1
    if (strnumber(vtype,val) EQ 1) then begin   ;Test for multiple format

        ndup = val
        vtype = strmid(fmt_1,1,1)

        if (strnumber(vtype,val) EQ 1) then begin

               ndup = 10*ndup+ val
               vtype = strmid(fmt_1,2,1)

        endif

        if vtype EQ '(' then $
           message,'Parenthesis within format string not allowed'

    endif   
  
    for j = 1L,ndup do begin
     CASE vtype OF 

     'A':  begin

        tst = strnumber(strmid(fmt_1,1, strlen(fmt_1)-1), nfield)
        if (tst EQ 0) or (strlen(fmt_1) LT 2) then $ 
             message,'String format must include a field length'
    
         nfield = fix(nfield)
         idltype = 7
         end

   'D':  idltype = 5

   'E':  idltype = 4

   'F':  idltype = 4
   
   'G':  idltype = 4

   'I':  begin                       ;Decide whether BYTE, INTEGER or LONG

         pos = strpos(fmt_1,vtype)
         len = fix(strmid( fmt_1, pos+1, strlen(fmt_1)-pos-1))
         if len EQ 1 then idltype = 1 $
           else if len LT 5 then idltype = 2 $
                            else idltype = 3

         end

   'H':  goto, NO_VAR 
   
   'O':  idltype = 3
   
   'Z':  idltype = 3

   'X':  goto, NO_VAR               ;No variable declaration needed

   ELSE: message,'ERROR - Illegal format '+fmt_1 +' in field ' + strtrim(k,2)

 endcase

; Define output arrays

   st = 'v'+ ii[k] +'= make_array(nlines, type = idltype)'  
   tst = execute(st)
   st = 'x'+ ii[k] +'= make_array(1,type = idltype)'  
   tst = execute(st)
   k = k+1
   if k EQ ncol then goto, DONE          ;Normal exit
  endfor
NO_VAR:  

  ENDREP until frmt EQ ''

  message,'ERROR - ' + strtrim(ncol,2)+ ' output vectors supplied but only ' + $
         strtrim(k,2) + ' FORMAT fields specified'

DONE: 
  
  openr, LUN, name, /get_lun
  ngood = 0L
  skip_lun,lun,skipline,/lines

  On_IOerror, BAD_LINE  


  for j = 0L,nlines-1 do begin

   badline = 1

   case ncol of                  ;Can't use ON_IOERROR with EXECUTE statement
;                                 so have to list all the possibilities 
   1:   readf,LUN,f = fmt1,x1        
   2:   readf,LUN,f = fmt1,x1,x2
   3:   readf,LUN,f = fmt1,x1,x2,x3
   4:   readf,LUN,f = fmt1,x1,x2,x3,x4
   5:   readf,LUN,f = fmt1,x1,x2,x3,x4,x5
   6:   readf,LUN,f = fmt1,x1,x2,x3,x4,x5,x6
   7:   readf,LUN,f = fmt1,x1,x2,x3,x4,x5,x6,x7
   8:   readf,LUN,f = fmt1,x1,x2,x3,x4,x5,x6,x7,x8
   9:   readf,LUN,f = fmt1,x1,x2,x3,x4,x5,x6,x7,x8,x9
   10:  readf,LUN,f = fmt1,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10
   11:  readf,LUN,f = fmt1,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11
   12:  readf,LUN,f = fmt1,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12
   13:  readf,LUN,f = fmt1,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13
   14:  readf,LUN,f = fmt1,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14
   15:  readf,LUN,f = fmt1,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15
   16:  readf,LUN,f = fmt1,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,$
                      x16
   17:  readf,LUN,f = fmt1,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,$
                      x16,x17
   18:  readf,LUN,f = fmt1,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15, $
                   x16,x17,x18
   19:  readf,LUN,f = fmt1,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15, $
                   x16,x17,x18,x19 
   20:  readf,LUN,f = fmt1,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15, $
                   x16,x17,x18,x19,x20 
   21:  readf,LUN,f = fmt1,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15, $
                   x16,x17,x18,x19,x20,x21 
   22:  readf,LUN,f = fmt1,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15, $
                   x16,x17,x18,x19,x20,x21,x22 
   23:  readf,LUN,f = fmt1,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15, $
                   x16,x17,x18,x19,x20,x21,x22,x23
   24:  readf,LUN,f = fmt1,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15, $
                   x16,x17,x18,x19,x20,x21,x22,x23,x24 
   25:  readf,LUN,f = fmt1,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15, $
                   x16,x17,x18,x19,x20,x21,x22,x23,x24,x25 

  ENDCASE 

    for i = 0L, ncol-1 do begin

        st ='v' + ii[i] + '[ngood] = x'+ii[i]
        tst = execute(st)

     endfor

     ngood = ngood + 1
     badline = 0
BAD_LINE: 
     if badline then if ~keyword_set(SILENT) then $
                 message,'Error reading line ' + strtrim(skipline+ j+1,2),/CON
  endfor
  free_lun, LUN

  if ngood EQ 0L then message, $
                'ERROR - No valid lines found with specified format'
  if ~keyword_set( SILENT)  then $
          message, strtrim(ngood,2) + ' valid lines read',/INF

; Compress arrays to match actual number of valid lines

  for i = 0L, ncol-1 do begin 

      var ='v'+ii[i]
      tst = execute(var + '='+ var+ '[0:ngood-1]')

  endfor

  return
  end
