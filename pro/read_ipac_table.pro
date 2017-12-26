FUNCTION read_ipac_table, filename, table_col_info=table_col_info, table_hdr=table_hdr, change_null=change_null, debug=debug

;+
; NAME: 
;   READ_IPAC_TABLE
;
; PURPOSE: 
;   Read an IPAC ascii table from a file into IDL structures
;
; EXPLANATION:
;   Reads an IPAC ascii table from a file into IDL structures.  The
;   definition of an IPAC-format table is currently here:
;      https://irsa.ipac.caltech.edu/applications/DDGEN/Doc/ipac_tbl.html
;
; CALLING SEQUENCE: 
;      info = read_ipac_table(filename, table_col_info=table_col_info,
;             table_hdr=table_hdr, [change_null=change_null, /debug])
;
; INPUTS:  
;      FILENAME -- string giving the file with the input IPAC ascii table
;
; OPTIONAL INPUT:
;      CHANGE_NULL -- an integer value to be used when the IPAC table
;                     has a non-numeric string for null values in an 
;                     integer column.  The default is -9999.  (For
;                     real numbers, the nulls will go automatically to
;                     'NaN'.)
;
;      DEBUG -- enables some debugging statements
;
; OUTPUTS: 
;      info - IDL array structure with the data.  The structure
;           tag names are taken from the column names, with possible
;           changes needed by IDL.  
;
;           If the table is not valid, or contains no data, the function returns a value of -1
;
;       table_col_info - A structure with table column headers
;           in tags starting with "HEADER": HEADER_Col_Names, 
;           HEADER_Col_Names_Orig, HEADER_Col_Types_Orig, and, 
;           if present, HEADER_Data_Units and HEADER_Null_Values.
;
;       table_hdr - A string array with whatever comment and keyword
;                   lines precede the column headers.
;
; PROCEDURES USED:
;   VALID_NUM
;
; MODIFICATION HISTORY:
;      Written by H. Teplitz, IPAC September 2010 
;      Allow long integer, convert blanks in numeric fields to null
;      value - T. Brooke, IPAC May 2011
;      Allow 64bit long; use valid_num to check - TYB June 2013
;      Report readfile, free luns, default null str to "null" - TYB Jan 2016
;      Re-do structures to separate out the data - TYB Aug 2017 
;-

;Copyright © 2013, California Institute of Technology
;All rights reserved. Based on Government Sponsored Research NAS7-03001 and NNN12AA01C.
;
;
;Redistribution and use in source and binary forms, with or without
;modification, are permitted provided that the following conditions
;are met:
;
; *  Redistributions of source code must retain the above copyright
;    notice, this list of conditions and the following disclaimer.
;
; *  Redistributions in binary form must reproduce the above copyright
;    notice, this list of conditions and the following disclaimer in
;    the documentation and/or other materials provided with the
;    distribution.
;
; *  Neither the name of the California Institute of Technology
;    (Caltech) nor the names of its contributors may be used to
;    endorse or promote products derived from this software without
;    specific prior written permission.
;
;THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
;OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
;AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
;WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;POSSIBILITY OF SUCH DAMAGE.
;

on_error,2
compile_opt idl2

IF N_params() lt 1 THEN BEGIN
   print,'Syntax - info = read_ipac_table(filename, table_col_info=table_col_info, table_hdr=table_hdr, [change_null=change_null, /debug])'
   return, -1
ENDIF

file = filename
n_lines = file_lines(file)

print, 'Reading ', file

IF keyword_set(change_null) THEN BEGIN
  IF ( NOT(valid_num(change_null,/integer)) ) THEN BEGIN
    print, 'ERROR: change null value must be integer.'
    return,-1
  ENDIF ELSE BEGIN
    null_num = change_null
  ENDELSE
ENDIF ELSE null_num = -9999

line=''
inline=''

already_read = 0
lines_read = 0

openr, lun, file, /get_lun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; create the header string array
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

igotone = 0
firstchar = '\'
WHILE firstchar NE '|' DO BEGIN 
   readf, lun, inline
   lines_read = lines_read+1
   IF EOF(lun) THEN BEGIN
      print, 'ERROR:  Invalid IPAC table - no header lines'
      close, lun
      free_lun, lun
      return, -1
   ENDIF
   firstchar = strmid(inline,0,1)
   IF firstchar EQ '\' THEN BEGIN
      IF igotone eq 0 THEN BEGIN
        table_hdr = inline
        igotone = 1
      ENDIF ELSE BEGIN
        table_hdr = [table_hdr, inline]
     ENDELSE
   ENDIF
ENDWHILE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; use first line with '|' to find indices between columns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

line = inline
len = strlen(line)

;;;; check for trailing spaces after last |

pos = strpos(line,'|',/reverse_search)
IF (pos lt 2) THEN BEGIN
  print,'ERROR: invalid table column header'
  close, lun
  free_lun, lun
  return, -1
ENDIF ELSE BEGIN
  len = pos + 1
  line = strmid(line,0,len)
ENDELSE

name_line_length = len
subline = line

strput, subline, 'x', 0
delim_idx = [0]
eol=0
WHILE NOT(eol) DO BEGIN 
   char = strpos(subline,'|')
   IF char NE -1 THEN begin
      strput, subline, 'x', char
      delim_idx = [delim_idx, char]
   ENDIF 
   IF char EQ len-1 THEN eol=1
ENDWHILE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; check for at least 1 column
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

IF n_elements(delim_idx) le 1 THEN BEGIN 
   print, 'ERROR: invalid table header'
   close, lun
   free_lun, lun 
   return, -1
ENDIF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; get column names and put into a strarr
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ncol = n_elements(delim_idx)-1
col_names = strarr(ncol)
col_names_orig = strarr(ncol)
col_width = intarr(ncol)
FOR i = 0, ncol-1 DO BEGIN 
   col_width[i] = delim_idx[i+1]-delim_idx[i]-1
   col_names[i] = strtrim(strmid(line, delim_idx[i]+1, col_width[i]),2)
   col_names_orig[i] = col_names[i]
ENDFOR

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; check for duplicate column names, add "_idl_[i]"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

cntr = intarr(ncol)*0 + 1
FOR ik = 0, ncol-2 DO BEGIN
   FOR ij = ik+1, ncol-1 DO BEGIN
     IF (strcmp(col_names[ij],col_names[ik],/fold_case)) THEN BEGIN
       col_names[ij] = col_names[ij] + '_idl_' + strn(cntr[ik])
       cntr[ik] = cntr[ik] + 1
       print,'WARNING: Duplicate column names, replacing occured'
     ENDIF
   ENDFOR
ENDFOR

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; next line must be data types
;;;; need error check if it isn't....
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

readf, lun, inline
lines_read = lines_read+1

;;;; check for no data after types line 
IF EOF(lun) THEN BEGIN 
   print, 'ERROR: invalid table; no data'
   close, lun
   free_lun, lun 
   return, -1
ENDIF

line=inline

IF strmid(line, 0, 1) NE '|' THEN BEGIN 
   print, 'ERROR: invalid or missing data types line'
   close, lun
   free_lun, lun 
   return, -1
ENDIF

col_type_string = strarr(ncol)
col_types_orig = strarr(ncol)
col_type_code = intarr(ncol)

FOR i = 0, ncol-1 DO BEGIN
   ;;; strip spaces from data type and convert to all upper case
   col_type_string[i] = strupcase(strtrim(strmid(line, delim_idx[i]+1, col_width[i]),2))
   col_types_orig[i] = strtrim(strmid(line, delim_idx[i]+1, col_width[i]),2)
   check = strmid(line,delim_idx[i+1],1)
   IF check NE '|' THEN BEGIN 
      print, 'ERROR: missing pipe in data types line'
      close, lun
      free_lun, lun
      IF keyword_set(debug) then stop
      return, -1
   ENDIF   
   
;;; convert data types to    
   
   CASE col_type_string[i] OF 
      'INTEGER':  BEGIN
         col_type_code[i] = 3      
         print, 'Data type INTEGER is used.  For full compatibility with all IPAC services, please use INT, IN or I'
      END
      'INT':  col_type_code[i] = 3
      'IN':  col_type_code[i] = 3
      'I':  col_type_code[i] = 3
      'LONG':  col_type_code[i] = 14
      'LON':  col_type_code[i] = 14
      'LO':  col_type_code[i] = 14
      'L':  col_type_code[i] = 14
      'FLOAT':   col_type_code[i] = 4
      'FLOA':   col_type_code[i] = 4
      'FLO':   col_type_code[i] = 4
      'FL':   col_type_code[i] = 4      
      'F':   col_type_code[i] = 4      
      'REAL':   col_type_code[i] = 4
      'REA':   col_type_code[i] = 4
      'RE':   col_type_code[i] = 4
      'R':   col_type_code[i] = 4
      'DOUBLE':   col_type_code[i] = 5
      'DOUBL':   col_type_code[i] = 5
      'DOUB':   col_type_code[i] = 5
      'DOU':   col_type_code[i] = 5
      'DO':   col_type_code[i] = 5
      'D':   col_type_code[i] = 5
      'CHAR':  col_type_code[i] = 7
      'CHA':  col_type_code[i] = 7
      'CH':  col_type_code[i] = 7
      'C':  col_type_code[i] = 7
      'DATE':  col_type_code[i] = 7
      'DAT':  col_type_code[i] = 7
      'DA':  col_type_code[i] = 7
      ELSE: BEGIN 
         print, 'ERROR:  invalid data type = '+col_type_string[i]
         close, lun
         free_lun, lun
         IF keyword_set(debug) then stop
         return,-1
      ENDELSE     
   ENDCASE
     
ENDFOR

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; create the column info structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

table_col_info = create_struct('HEADER_Col_Names', IDL_VALIDNAME(col_names, /convert_all))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Save the original column names and column types.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

current = table_col_info
table_col_info = create_struct(current, 'HEADER_Col_Names_Orig', col_names_orig)
current = table_col_info
table_col_info = create_struct(current, 'HEADER_Col_Types_Orig', col_types_orig)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Read next line.  If it starts with a pipe, it should be the units line.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

readf, lun, inline

line=inline

IF strmid(inline,0,1) EQ '|' THEN BEGIN 
   lines_read = lines_read+1
   data_units_string = strarr(ncol)
   FOR i = 0, ncol-1 DO BEGIN
   ;;; strip spaces from units
      data_units_string[i] = strtrim(strmid(line, delim_idx[i]+1, col_width[i]),2)
      check = strmid(line,delim_idx[i+1],1)
      IF check NE '|' THEN BEGIN 
         print, 'ERROR: missing pipe in units line'
         close, lun
         free_lun, lun
         IF keyword_set(debug) then stop
         return, -1
      ENDIF   
   endfor
   current = table_col_info
   table_col_info = create_struct(current, 'HEADER_Data_Units', data_units_string)
;   remember to increment lines_read
ENDIF $
ELSE already_read = 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; If the line was data units then read next line.  
;;;;; If it starts with a pipe, it should be the nulls line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


IF NOT(already_read) THEN BEGIN 
   readf, lun, inline
   line=inline

   IF strmid(inline,0,1) EQ '|' THEN BEGIN 
      lines_read = lines_read+1
      null_value_string = strarr(ncol)
      new_null_value_string = strarr(ncol)
      FOR i = 0, ncol-1 DO BEGIN
;;; strip spaces from nulls
         null_value_string[i] = strtrim(strmid(line, delim_idx[i]+1, col_width[i]),2)
         check = strmid(line,delim_idx[i+1],1)
         IF check NE '|' THEN BEGIN 
            print, 'ERROR: missing pipe in nulls line'
            close, lun
            free_lun, lun
            IF keyword_set(debug) then stop
            return, -1
         ENDIF

         IF (col_type_code[i] ne 7) THEN BEGIN
            IF ( (col_type_code[i] eq 4) or (col_type_code[i] eq 5) ) THEN BEGIN
               check_num = valid_num(null_value_string[i])
               IF (check_num eq 0) THEN BEGIN
                 new_null_value_string[i] = 'NaN'
               ENDIF ELSE BEGIN
                 new_null_value_string[i] = null_value_string[i]
               ENDELSE
            ENDIF ELSE BEGIN
               check_num = valid_num(null_value_string[i], /integer)
               IF (check_num eq 0) THEN BEGIN 
                 new_null_value_string[i] = strn(null_num)
               ENDIF ELSE BEGIN
                 new_null_value_string[i] = null_value_string[i]
               ENDELSE
            ENDELSE
         ENDIF ELSE new_null_value_string[i] = null_value_string[i]
      ENDFOR 
   ENDIF ELSE BEGIN 
      null_value_string = strarr(ncol)+'null'
      new_null_value_string = null_value_string
      iwant = where ( ( (col_type_code eq 4) or (col_type_code eq 5) ),nwant)
      if (nwant gt 0) then new_null_value_string[iwant] = 'NaN'
      iwant = where ( ( (col_type_code eq 3) or (col_type_code eq 14) ),nwant)
      if (nwant gt 0) then new_null_value_string[iwant] = strn(null_num)
      already_read = 1
   ENDELSE
ENDIF ELSE BEGIN 
   null_value_string = strarr(ncol)+'null'   
   new_null_value_string = null_value_string
   iwant = where ( ( (col_type_code eq 4) or (col_type_code eq 5) ),nwant)
   if (nwant gt 0) then new_null_value_string[iwant] = 'NaN'
   iwant = where ( ( (col_type_code eq 3) or (col_type_code eq 14) ),nwant)
   if (nwant gt 0) then new_null_value_string[iwant] = strn(null_num)
ENDELSE 

current = table_col_info
table_col_info = create_struct(current, 'HEADER_Null_Values', new_null_value_string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; set up data structure.  length of vectors is number of lines in 
;;;;; file minus lines read so far
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ndata = n_lines - lines_read

IF ndata LE 0 THEN BEGIN
   print, 'QUIT:  No data found.'
   close, lun
   free_lun, lun
   return, -1
ENDIF

FOR i = 0, ncol-1 DO BEGIN
  case col_type_code[i] of
    7: cval = ' '
    3: cval = 0L
   14: cval = 0LL
    4: cval = 0.0
    5: cval = 0.0d
  endcase     
  if (i eq 0) then info = create_struct(IDL_VALIDNAME(col_names[0],/convert_all), cval) else begin
    current = info  
    info = create_struct(current, IDL_VALIDNAME(col_names[i],/convert_all), cval)
  endelse                     
ENDFOR
info = replicate(info, ndata)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; read data lines to put into structure
;;;;; and pad the line if it isn't long enough for all columns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   

lmax = 2.0d^63 - 1.0d
lmin = -2.0d^63
lmaxi = 2.0d^31 - 1.0d
lmini = -2.0d^31
   
FOR j = 0, ndata-1 DO BEGIN 
  
   IF NOT(already_read) THEN readf, lun, inline

;;;; check for non-printable characters
   IF ( (stregex(inline,string(9b)) ne -1) or $
        (stregex(inline,string(7b)) ne -1) or $
        (stregex(inline,string(8b)) ne -1) or $
        (stregex(inline,string(10b)) ne -1) or $
        (stregex(inline,string(11b)) ne -1) or $
        (stregex(inline,string(12b)) ne -1) or $
        (stregex(inline,string(13b)) ne -1) or $
        (stregex(inline,string(27b)) ne -1) ) THEN BEGIN
     print,'Non-printable character in data row = ',j
     close, lun
     free_lun, lun
     return,-1
   ENDIF
  
   cur_len = strlen(inline)
   IF cur_len LT name_line_length THEN BEGIN 
      padlen = name_line_length - cur_len
      pad = strjoin(replicate(' ', padlen))
      line = inline+pad
   ENDIF ELSE line=inline
   
   FOR i = 0, ncol-1 DO BEGIN
      data_string = strtrim(strmid(line, delim_idx[i]+1, col_width[i]),2)
      check = strmid(line,delim_idx[i],1)
      IF check NE ' ' THEN BEGIN 
         print, 'ERROR: misaligned columns (data under pipe)'
         print, 'ERROR: data row, column = ',j,' , ',i
         close, lun
         free_lun, lun
         IF keyword_set(debug) THEN stop
         return, -1
      ENDIF   
      IF (col_type_code[i] ne 7) THEN BEGIN
         IF ( (col_type_code[i] eq 4) or (col_type_code[i] eq 5) ) THEN BEGIN
            check_num = valid_num(data_string)
            IF (check_num eq 0) THEN BEGIN
               IF (data_string ne null_value_string[i]) THEN BEGIN
                 data_string = new_null_value_string[i]
                 print,'WARNING: Invalid data entry replaced by null value in row, column = ',j,', ',i
               ENDIF ELSE data_string = new_null_value_string[i]
            ENDIF
;;;; Check floating point limits
            IF (check_num ne 0) THEN BEGIN
               check_lim = fix(data_string, type=5)
               IF (finite(check_lim)) THEN BEGIN
                  IF (col_type_code[i] eq 4) THEN BEGIN
                     check_lim = fix(data_string, type=4)
                     IF ( NOT(finite(check_lim)) ) THEN BEGIN
                        data_string = new_null_value_string[i]
                        print,'WARNING: Float overflow replaced by null value in row, column = ',j,', ',i
                     ENDIF
                  ENDIF
               ENDIF ELSE BEGIN
                  data_string = new_null_value_string[i]
                  print,'WARNING: Double overflow replaced by null value in row, column = ',j,', ',i
               ENDELSE
            ENDIF 
         ENDIF ELSE BEGIN
            check_num = valid_num(data_string,/integer)
            IF (check_num eq 0) THEN BEGIN
               IF (data_string ne null_value_string[i]) THEN BEGIN
                 data_string = new_null_value_string[i]
                 print,'WARNING: Invalid data entry replaced by null value in row, column = ',j,', ',i
               ENDIF ELSE data_string = new_null_value_string[i]
           ENDIF
;;;; Check integer limits
           IF (check_num ne 0) THEN BEGIN
              check_lim = fix(data_string, type=5)
              IF ( (check_lim gt lmin) and (check_lim lt lmax) ) THEN BEGIN
                 IF (col_type_code[i] eq 3) THEN BEGIN
                    IF ( (check_lim le lmini) or (check_lim ge lmaxi) ) THEN BEGIN
                       data_string = new_null_value_string[i]
                       print,'WARNING: Integer overflow replaced by null value in row, column = ',j,', ',i
                    ENDIF
                 ENDIF
              ENDIF ELSE BEGIN
                 data_string = new_null_value_string[i]
                 print,'WARNING: Long overflow replaced by null value in row, column = ',j,', ',i
              ENDELSE
           ENDIF 
         ENDELSE
      ENDIF
      info[j].(i) = data_string
   ENDFOR   
   already_read=0
ENDFOR

close, lun
free_lun, lun

return, info

END


   
