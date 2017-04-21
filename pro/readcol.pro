pro readcol,name,v1,V2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15, $
            v16,v17,v18,v19,v20,v21,v22,v23,v24,v25,v26,v27,v28,v29,v30,$
            v31,v32,v33,v34,v35,v36,v37,v38,v39,v40,v41,v42,v43,v44,v45, $
	    v46,v47,v48,v49,v50, COMMENT = comment, $
            FORMAT = fmt, DEBUG=debug, SILENT=silent, SKIPLINE = skipline, $
            NUMLINE = numline, DELIMITER = delimiter, NAN = NaN, $
            PRESERVE_NULL = preserve_null, COUNT=ngood, NLINES=nlines, $
            STRINGSKIP = skipstart, QUICK = quick, COMPRESS = compress
;+
; NAME:
;       READCOL
; PURPOSE:
;       Read a free-format ASCII file with columns of data into IDL vectors 
; EXPLANATION:
;       Lines of data not meeting the specified format (e.g. comments) are 
;       ignored.  By default, columns may be separated by commas or spaces.
;
;       Use READFMT to read a fixed-format ASCII file.   Use RDFLOAT for
;       much faster I/O (but less flexibility).    Use FORPRINT to write 
;       columns of data (inverse of READCOL).   
;
;       If you sure that all lines meet the specified format (excluding 
;       commented and SKIPed lines) then the speed for reading large files
;       can be significantly improved by setting the /QUICK keyword.
;
; CALLING SEQUENCE:
;       READCOL, name, v1, [ v2, v3, v4, v5, ...  v50 , COMMENT=, /NAN
;           DELIMITER= ,FORMAT = , /DEBUG ,  /SILENT , SKIPLINE = , NUMLINE = 
;           COUNT =, STRINGSKIP= 
;
; INPUTS:
;       NAME - Name of ASCII data file, scalar string.  
;
; OPTIONAL INPUT KEYWORDS:
;       FORMAT - scalar string containing a letter specifying an IDL type
;               for each column of data to be read.  Allowed letters are 
;               A - string data, B - byte, D - double precision, F- floating 
;               point, I - short integer, L - longword, LL - 64 bit integer, 
;               U - unsigned short integer, UL - unsigned long integer 
;               Z - longword hexadecimal, and X - skip a column.
;
;               Columns without a specified format are assumed to be floating 
;               point.  Examples of valid values of FMT are
;
;       'A,B,I'        ;First column to read as a character string, then 
;                       1 column of byte data, 1 column integer data
;       'L,L,L,L'       ;Four columns will be read as longword arrays.
;       ' '             ;All columns are floating point
;
;       If a FORMAT keyword string is not supplied, then all columns are 
;       assumed to be floating point.
;
;       /SILENT - Normally, READCOL will display each line that it skips over.
;               If SILENT is set and non-zero then these messages will be 
;               suppressed.
;       /DEBUG - If this keyword is non-zero, then additional information is
;                printed as READCOL attempts to read and interpret the file.
;       COMMENT - single character specifying comment character.   Any line 
;                beginning with this character will be skipped.   Default is
;                no comment lines.
;       /COMPRESS - If set, then the file is assumed to be gzip compressed.
;                The file is assumed to be compressed if it ends in '.gz'
;       DELIMITER - Character(s) specifying delimiter used to separate 
;                columns.   Usually a single character but, e.g. delimiter=':,'
;                specifies that either a colon or comma as a delimiter. 
;                Set DELIM = string(9b) to read tab separated data
;                The default delimiter is either a comma or a blank.
;       /NAN - if set, then an empty field will be read into a floating or 
;                double numeric variable as NaN; by default an empty field is 
;                converted to 0.0.
;       /PRESERVE_NULL - If set, then spaces are considered to be valid fields,
;                useful if the columns contain missing data.   Note that between
;                April and December 2006, /PRESERVE_NULL was the default.
;       /QUICK -  If set, then READCOL does not check that each individual line
;                matches the supplied format.     This makes READCOL less 
;                flexible but can provide a significant speed improvement when
;                reading large files.                       
;       SKIPLINE - Scalar specifying number of lines to skip at the top of file
;               before reading.   Default is to start at the first line.
;       NUMLINE - Scalar specifying number of lines in the file to read.  
;               Default is to read the entire file
;       STRINGSKIP - will skip all lines that begin with the specified string.
;               (Unlike COMMENT this can be more than 1 character.) Useful to 
;               skip over comment lines.
;
; OUTPUTS:
;       V1,V2,V3,...V50 - IDL vectors to contain columns of data.
;               Up to 50 columns may be read.  The type of the output vectors
;               are as specified by FORMAT.
;
; OPTIONAL OUTPUT KEYWORDS:
;       COUNT - integer giving the number of valid lines actually read
;       NLINES - integer giving the total number of lines in the file 
;                (as returned by FILE_LINES)
;
; EXAMPLES:
;       Each row in a file position.dat contains a star name and 6 columns
;       of data giving an RA and Dec in sexagesimal format.   Read into IDL 
;       variables.   (NOTE: The star names must not include the delimiter 
;       as a part of the name, no spaces or commas as default.)
;
;       IDL> FMT = 'A,I,I,F,I,I,F'
;       IDL> READCOL,'position.dat',F=FMT,name,hr,min,sec,deg,dmin,dsec  
;
;       The HR,MIN,DEG, and DMIN variables will be integer vectors.
;
;       Alternatively, all except the first column could be specified as
;       floating point.
;
;       IDL> READCOL,'position.dat',F='A',name,hr,min,sec,deg,dmin,dsec 
;
;       To read just the variables HR,MIN,SEC
;       IDL> READCOL,'position.dat',F='X,I,I,F',HR,MIN,SEC
;
; RESTRICTIONS:
;       This procedure is designed for generality and not for speed.
;       If a large ASCII file is to be read repeatedly, it may be worth
;       writing a specialized reader.
;
;       Columns to be read as strings must not contain the delimiter character
;       (i.e. commas or spaces by default).   Either change the default 
;       delimiter with the DELIMITER keyword, or use READFMT to read such files.
;
;       Numeric values are converted to specified format.  For example,
;       the value 0.13 read with an 'I' format will be converted to 0.
;
; PROCEDURES CALLED
;       GETTOK(), STRNUMBER()
;       The version of STRNUMBER() must be after August 2006.
; REVISION HISTORY:
;       Written         W. Landsman                 November, 1988
;        Added DELIMITER keyword  W. Landsman          Nov. 1999
;       Hexadecimal support added.  MRG, RITSS, 15 March 2000.
;       Default is comma or space delimiters as advertised   W.L. July 2001
;       Faster algorithm, use STRSPLIT if V5.3 or later  W.L.  May 2002
;       Accept null strings separated by delimiter ,e.g. ',,,'
;       Use SCOPE_VARFETCH instead of EXECUTE() for >V6.1  W.L. Jun 2005
;       Added compile_opt idl2   W. L.  July 2005
;       Added the NaN keyword   W. L      August 2006
;       Added /PRESERVE_NULL keyword  W.L.  January 2007
;       Assume since V5.6 (FILE_LINES available ) W.L. Nov 2007
;       Added COUNT output keyword  W.L.  Aug 2008
;       Added NLINES output keyword W.L.   Nov 2008
;       Added SKIPSTART keyword  Stephane Beland January 2008
;       Renamed SKIPSTART to STRINGSKIP to keep meaning of SKIP W.L. Feb 2008
;       Assume since V6.1, SCOPE_VARFETCH available W.L. July 2009
;       Read up to 40 columns W.L. Aug 2009
;       Use pointers instead of SCOPE_VARFETCH. Fixes bug with
;       IDL Workbench and runs 20% faster Douglas J. Marshall/W.L. Nov 2009
;       Recognize  LL, UL, and ULL data types, don't use 'val' output from 
;           STRNUMBER()   W.L.  Feb 2010
;       Graceful return even if no valid lines are present D. Sahnow April 2010
;       Ability to read tab separated data WL April 2010
;       Free memory used by pointers  WL  July 2010
;       Added /QUICK keyword  WL  Sep 2010
;       Accept normal FORTRAN formats (e.g. F5.1) P. Noterdaeme/W.L Jan 2011
;       Add COMPRESS keyword, IDL 6 notation W. Landsman/J. Bailin   Feb 2011
;       Allow filename to be 1 element array W.Landsman/S.Antonille Apr 2011
;       Feb 2010 change caused errors when reading blanks as numbers. 
;                          W.L. July 2012
;       Read up to 50 columns W.L.  March 2013
;       Assume a compressed file if it ends in '.gz'  W.L.  Oct 2015
;       Avoid error if more format codes than output variables W.L. April 2017
;-

  compile_opt idl2

  if N_params() lt 2 then begin
    print,'Syntax - READCOL, name, v1, [ v2, v3,...v50, /NAN, DELIMITER=,/QUICK'
    print,'        FORMAT= ,/SILENT  ,SKIPLINE =, NUMLINE = , /DEBUG, COUNT=]'
     return
  endif

  Catch, theError
  if theError NE 0 then begin
       Catch,/Cancel
       void = cgErrorMsg(/quiet)
  return
  endif
; Get number of lines in file

  ngood = 0L                 ;Number of good lines
  if N_elements(compress) EQ 0 then $
        compress = strmid(name,2,3,/reverse) EQ '.gz'
  nlines = FILE_LINES( name, COMPRESS=compress[0] )
  

  if keyword_set(DEBUG) then $
     message,'File ' + name+' contains ' + strtrim(nlines,2) + ' lines',/INF

  if N_elements( SKIPLINE ) EQ 0 then skipline = 0
  nlines = nlines - skipline
  if nlines LE 0 then begin
     message,'ERROR - File ' + name+' contains no data',/CON
     return
  endif     
  if N_elements( NUMLINE) GT 0 then nlines = numline < nlines

  if N_elements( SKIPSTART ) EQ 0 then begin
     skipstart_flg=0 
  endif else begin
     skipstart_flg=1
     nskipstart = strlen(skipstart)
  endelse

  ncol = N_params() - 1         ;Number of columns of data expected
  vv = 'v' + strtrim( indgen(ncol)+1, 2)
  nskip = 0

  if N_elements(fmt) GT 0 then begin ;FORMAT string supplied?

     if size(fmt,/tname) NE 'STRING' then $
        message,'ERROR - Supplied FORMAT keyword must be a scalar string'
;   Remove blanks from format string
     frmt = strupcase(strcompress(fmt,/REMOVE))   
     remchar, frmt, '('         ;Remove parenthesis from format
     remchar, frmt, ')'           

;   Determine number of columns to skip ('X' format)
     pos = strpos(frmt, 'X', 0)

     while pos NE -1 do begin
        pos = strpos( frmt, 'X', pos+1)
        nskip++
     endwhile

  endif else begin              ;Read everything as floating point

     frmt = 'F'
     if ncol GT 1 then for i = 1,ncol-1 do frmt += ',F'
     if ~keyword_set( SILENT ) then message, $
        'Format keyword not supplied - All columns assumed floating point',/INF

  endelse

  nfmt = ncol + nskip
  idltype = intarr(nfmt)
  bigarr = ptrarr(ncol)

; Create output arrays according to specified formats

  k = 0L                        ;Loop over output columns
  hex = bytarr(nfmt)
  for i = 0L, nfmt-1 do begin

     fmt1 = gettok( frmt, ',' )
     if fmt1 EQ '' then fmt1 = 'F' ;Default is F format
     case strmid(fmt1,0,1) of 
        'A':  idltype[i] = 7          
        'D':  idltype[i] = 5
        'F':  idltype[i] = 4
        'I':  idltype[i] = 2
        'B':  idltype[i] = 1
        'L':  idltype[i] = strmid(fmt1,0,2) EQ 'LL' ? 14 : 3 
	'U':  if strmid(fmt1,1,1) NE 'L' then idltype[i] = 12 else $
	      idltype[i] = strmid(fmt1,2,1) EQ 'L' ? 15 : 13
        'Z':  begin 
           idltype[i] = 3       ;Hexadecimal
           hex[i] = 1b
        end
        'X':  idltype[i] = 0    ;IDL type of 0 ==> to skip column
        ELSE:  message,'Illegal format ' + fmt1 + ' in field ' + strtrim(i,2)
     endcase

; Define output arrays

     if idltype[i] GT 0 then begin
        bigarr[k] = ptr_new(make_array(nlines,type=idltype[i]))
        k++
        if k GE ncol then break
     endif

  endfor
  goodcol = where(idltype)
  idltype = idltype[goodcol]
  check_numeric = (idltype NE 7)
  check_comment = N_elements(comment) GT 0
  openr, lun, name, /get_lun, compress=compress[0]

  temp = ' '
  skip_lun,lun,skipline, /lines

  if ~keyword_set(delimiter) then delimiter = ' ,'
  
  for j = 0L, nlines[0]-1 do begin
     readf, lun, temp
     if skipstart_flg then begin
                                ; requested to skip lines starting with specifc string
        if strmid(temp,0,nskipstart) eq skipstart then begin
           ngood--
           goto, BADLINE
        endif
     endif

     if strlen(temp) LT ncol then begin ;Need at least 1 chr per output line
        ngood--
        if ~keyword_set(SILENT) then $
           message,'Skipping Line (strlen) ' + strtrim(skipline+j+1,2),/INF
        goto, BADLINE 
     endif

     temp = strtrim(temp,1)     ;Remove leading spaces
     if check_comment then if strmid(temp,0,1) EQ comment then begin
        ngood--
        if keyword_set(DEBUG) then $
           message,'Skipping Comment Line ' + strtrim(skipline+j+1,2),/INF
        goto, BADLINE 
     endif

       var = delimiter EQ string(9b) ?  $
        strsplit(  temp,delimiter,/extract, preserve=preserve_null) $
       :strsplit(strcompress(temp) ,delimiter,/extract, preserve=preserve_null) 
     if N_elements(var) LT nfmt then begin 
        if ~keyword_set(SILENT) then $ 
           message,'Skipping Line (n_elements) ' + strtrim(skipline+j+1,2),/INF
        ngood--            
        goto, BADLINE           ;Enough columns?
     endif
     var = var[goodcol]

     k = 0
     if keyword_set(quick) then $      ;Don't check for valid numeric values
     
         for i = 0L,ncol-1 do (*bigarr[i])[ngood] = var[i]   $
    
    else begin 
     
     
     for i = 0L,ncol-1 do begin
        
        if check_numeric[i] then begin                      ;Check for valid numeric data
           tst = strnumber(var[i],val,hex=hex[i],NAN=nan)   ;Valid number?
           if ~tst  then begin                           ;If not, skip this line
              if ~keyword_set(SILENT) then $ 
                 message,'Skipping Line (check_numeric) ' + strtrim(skipline+j+1,2),/INF
              ngood--
              goto, BADLINE 
           endif
        endif 
	if strlen(strtrim(var[i],2)) Eq 0 then begin
	   if keyword_set(NAN) then (*bigarr[k])[ngood] = !VALUES.F_NAN else $
	                            (*bigarr[k])[ngood] = 0 
        endif else (*bigarr[k])[ngood] = var[i]
        k++

     endfor

endelse
     BADLINE:  ngood++

  endfor

  free_lun,lun
  if ngood EQ 0 then begin 
     message,'ERROR - No valid lines found for specified format',/INFORM
     return
  endif

  if ~keyword_set(SILENT) then $
     message,strtrim(ngood,2) + ' valid lines read', /INFORM  

; Compress arrays to match actual number of valid lines
  if ngood lt Nlines then for i=0,ncol-1 do $
     (*bigarr[i]) = (*bigarr[i])[0:ngood-1]

; Use SCOPE_VARFETCH to place into output variables..
   for i=0,ncol-1 do $
         (SCOPE_VARFETCH(vv[i],LEVEL=0)) = reform(*bigarr[i])
    ptr_free, bigarr	 
  return
end
