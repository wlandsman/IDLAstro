pro sxhread, name, header
;+
; NAME:
;       SXHREAD
; PURPOSE:
;       Procedure to read a STSDAS header from disk.
; EXPLANATION:
;       This version of SXHREAD can read two types of disk files
;       (1)  Unix stream files with a CR after every 80 bytes
;       (2)  Variable length record files 
;       (3)  Fixed length (80 byte) record files
;
; CALLING SEQUENCE:
;       sxhread, name, header
;
; INPUT:
;       name - file name, scalar string.  An extension of .hhh is appended
;               if not already supplied.   (Note STSDAS headers are required
;               to have a 3 letter extension ending in 'h'.)   gzip extensions
;               .gz will be recognized as compressed.
; OUTPUT:
;       header - STSDAS header, string array
; NOTES:
;       SXHREAD  does not do any checking to see if the file is a valid
;       STSDAS header.    It simply reads the file into a string array with
;       80 byte elements
;
; HISTORY:
;       Version 1  D. Lindler  July, 1987
;       Version 2  M. Greason, August 1990
;       Use READU for certain ST VAX GEIS files   W. Landsman January, 1992
;       Read variable length Unix files  E. Deutsch/W. Landsman November, 1994
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Updated by E. Artigau to handle gzipped fits  August 2004
;       Remove VMS support, W. Lnadsman September 2006
;-
;--------------------------------------------------------------------
 compile_opt idl2
 On_error,2                              ;Return to caller

 if N_params() LT 2 then begin
     print,'Syntax - SXHREAD, name, header'
     return
 endif

; Add extension name if needed

 hname = strtrim(name,2)
 if strpos(hname,'.',strpos(hname,']') ) EQ -1 then hname = hname + '.hhh'
 compress =  (strmid(name,strlen(name)-2,2) eq 'gz') 
 openr, unit, hname, /GET_LUN, ERROR = err,COMPRESS = compress

 if err LT 0 then goto, BADFILE

 len = 80  & ai = 99                    ;Usual header length is 80 bytes
    ;but Unix files may have an
                                        ;embedded carriage returns to make
   atmp = assoc(unit,bytarr(85))           ;header length 81 bytes
   a=atmp[0] & ai=0
   while (a[ai] ne 10) and (a[ai] ne 13) and (ai lt 84) do ai=ai+1
   if (ai EQ 80) then len=81
   Point_lun, unit, 0            ;Back to the beginning of the file



; Get the number of lines in the header

 status = fstat(unit)
 nlines = status.size/len                      ;Number of lines in file
 if (ai lt 80) then goto,VAR_LENGTH

; Read header

 header =  bytarr(len,nlines ,/NOZERO)
 On_ioerror, VAR_LENGTH        ;READU cannot be used on variable length records
 readu, unit, header
 header = string(header)
 On_ioerror,NULL

 free_lun,unit             ;Close and free file unit

; Trim to the END line, and delete carriage returns if necessary

 endline = where( strmid(header,0,8) EQ 'END     ',nfound)
 if nfound gt 0 then header = header[0:endline[0]] else $
     message,'WARNING: No END statement found in header',/inform
 if len EQ 81 then header = strmid(header,0,80)
 return

VAR_LENGTH:                 ;Now try to read as variable length records

 Point_lun, unit, 0          ;Back to the beginning of file
 h = ''  & header = strarr( nlines)
 i = 0

 On_ioerror,NOEND            ;Can't use EOF function on certain GEIS files
 while ( strtrim( strmid(h,0,8), 2) NE 'END') do begin
    readf, unit, h
    if (strlen(h) LT 80) then h=h+string(replicate(32b,80-strlen(h)))
    header[i] = h                  ;Swapped with line above 95-Aug
    i = i + 1
    if i EQ nlines then begin
            header = [header,strarr(100)]
            nlines = nlines + 100
     endif
 endwhile
 header = header[0:i-1]
 free_lun,unit
 return

NOEND:
   message,'WARNING - No END statement found in header', /INFORM
   free_lun,unit
   return

BADFILE:
   message,'Error opening file ' + ' ' + hname
   return

end
