pro rdfloat,name,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17, $
            v18,v19, SKIPLINE = skipline, NUMLINE = numline, DOUBLE=double, $
            SILENT = silent, COLUMNS = columns
;+
; NAME:
;      RDFLOAT
; PURPOSE:
;      Quickly read a numeric ASCII data file into IDL floating/double vectors.
; EXPLANATION:
;      Columns of data may be separated by tabs or spaces.      This 
;      program is fast but is restricted to data files where all columns can 
;      be read as floating point (or all double precision).   
;
;      Use READCOL if  greater flexibility is desired.   Use READFMT to read a 
;      fixed-format ASCII file.   Use FORPRINT to print columns of data.
;
; CALLING SEQUENCE:
;      RDFLOAT, name, v1, [ v2, v3, v4, v5, ...  v19] 
;                         COLUMNS, /DOUBLE, SKIPLINE = , NUMLINE = ]
;
; INPUTS:
;      NAME - Name of ASCII data file, scalar string. 
;
; OPTIONAL INPUT KEYWORDS:
;      COLUMNS - Numeric scalar or vector specifying which columns in the file
;               to read.    For example, if COLUMNS = [3,7,11] then the first
;               output variable (v1) would contain column 3, the second would
;               contain column 7 and the third would contain column 11.   If
;               the number of elements in the COLUMNS vector is less than the
;               number of output parameters, then consecutive columns are 
;               implied.    For example, if 3 output parameters are supplied
;               (v1,v2,v3) and COLUMNS = 3, then columns 3,4, and 5 will be
;               read.   
;      SKIPLINE - Integer scalar specifying number of lines to skip at the top
;              of file before reading.   Default is to start at the first line.
;      NUMLINE - Integer scalar specifying number of lines in the file to read.  
;             Default is to read the entire file
;      /DOUBLE - If this keyword is set, then all variables are read in as
;              double precision.
;      /SILENT - Set this keyword to suppress any informative messages
;
; OUTPUTS:
;      V1,V2,V3,...V19 - IDL vectors to contain columns of data.
;               Up to 19 columns may be read.  All output vectors are of type
;               float, unless the /DOUBLE keyword is set, 
;
; EXAMPLES:
;      Each row in a file 'position.dat' contains a star number and 6 columns
;      of data giving an RA and Dec in sexagesimal format.   Read into IDL 
;      variables.     
;
;       IDL> rdfloat,'position.dat',ID,hr,min,sec,deg,dmin,dsec  
;
;       All output vectors will be floating point.    To only read the 
;       declination vectors (Deg,dmin,dsec)
;
;       IDL> rdfloat,'position.dat',deg,dmin,dsec,col=4
;
; RESTRICTIONS:
;      (1) All rows in the file must be formatted identically (except for 
;          those skipped by SKIPLINE).    RDFLOAT reads the first line of 
;          the data (after SKIPLINE) to determine the number of columns of 
;          data.
;      (2) Cannot be used to read strings
; PROCEDURES USED:
;      None.
; REVISION HISTORY:
;      Written         W. Landsman                 September 1995
;      Call NUMLINES() function                    February 1996
;      Read up to 19 columns                       August 1997
;      Allow to skip more than 32767 lines  W. Landsman  June 2001
;      Added /SILENT keyword   W. Landsman         March 2002
;      Added COLUMNS keyword, use STRSPLIT    W. Landsman May 2002
;      Use SKIP_LUN if V5.6 or later          W. Landsman Nov 2002
;      V5.6 version, use FILE_LINES()         W. Landsman Dec 2002
;		Use Catch rather than On_ERROR,2      W. Landsman Jan 2018
;-
  compile_opt idl2

  if N_params() lt 2 then begin
     print,'Syntax - RDFLOAT, name, v1, [ v2, v3,...v19 '
     print,'                    COLUMNS = ,/DOUBLE, SKIPLINE =, NUMLINE = ]'
     return
  endif

  Catch, theError
  if theError NE 0 then begin
       Catch,/Cancel
       void = cgErrorMsg(/quiet)
  return
  endif

; Get number of lines in file

   nlines = FILE_LINES( name )
   if nlines LE 0 then begin
        message,'ERROR - File ' + name+' contains no data',/CON
	return
   endif     

 
   if ~keyword_set( SKIPLINE ) then skipline = 0
   nlines = nlines - skipline
   if keyword_set( NUMLINE) then nlines = numline < nlines

;Read first line, and determine number of columns of data

   openr, lun, name, /GET_LUN
   temp = ''
   if skipline GT 0 then $
        skip_lun, lun, skipline, /lines
   readf,lun,temp
      
   colval = strsplit(temp, count=ncol)         ;Determine number of columns
 
;Create big output array and read entire file into the array

   bigarr = keyword_set(DOUBLE) ? dblarr(ncol, nlines, /NOZERO):  $
                                  fltarr(ncol, nlines, /NOZERO) 

   close,lun
   openr, lun, name
   if skipline GT 0 then skip_lun, lun, skipline, /lines 

   readf, lun, bigarr
   free_lun, lun

   if ~keyword_set(SILENT) then $
       message, strtrim(nlines,2) + ' lines of data read',/INF

   Nvector = (N_params()-1) < ncol
   if N_elements(columns) EQ 0 then c = indgen(nvector) else c = columns - 1
   Nc = N_elements(c)
   if Nc LT nvector then c = [c,indgen(nvector-nc) + c[nc-1] +1 ] 
   v1 = reform( bigarr[c[0],*])
 
   if Nvector GT 1 then v2 = reform( bigarr[c[1],*]) else return
   if Nvector GT 2 then v3 = reform( bigarr[c[2],*]) else return
   if Nvector GT 3 then v4 = reform( bigarr[c[3],*]) else return
   if Nvector GT 4 then v5 = reform( bigarr[c[4],*]) else return
   if Nvector GT 5 then v6 = reform( bigarr[c[5],*]) else return
   if Nvector GT 6 then v7 = reform( bigarr[c[6],*]) else return
   if Nvector GT 7 then v8 = reform( bigarr[c[7],*]) else return
   if Nvector GT 8 then v9 = reform( bigarr[c[8],*]) else return
   if Nvector GT 9 then v10 = reform( bigarr[c[9],*]) else return
   if Nvector GT 10 then v11 = reform( bigarr[c[10],*]) else return
   if Nvector GT 11 then v12 = reform( bigarr[c[11],*]) else return
   if Nvector GT 12 then v13 = reform( bigarr[c[12],*]) else return
   if Nvector GT 13 then v14 = reform( bigarr[c[13],*]) else return
   if Nvector GT 14 then v15 = reform( bigarr[c[14],*]) else return
   if Nvector GT 15 then v16 = reform( bigarr[c[15],*]) else return
   if Nvector GT 16 then v17 = reform( bigarr[c[16],*]) else return
   if Nvector GT 17 then v18 = reform( bigarr[c[17],*]) else return
   if Nvector GT 18 then v19 = reform( bigarr[c[18],*]) 

  return
  end
