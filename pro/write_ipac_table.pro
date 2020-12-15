PRO write_ipac_table, in_struct, outfile, table_col_info=table_col_info, table_hdr=table_hdr, select_columns=select_columns, short_format=short_format, exact_format=exact_format, format=format

;+
; NAME: 
;   WRITE_IPAC_TABLE
;
; PURPOSE:  
;   Write an IPAC table from IDL structures.
;
; EXPLANATION:  
;   Writes an IPAC table to a file from IDL structures.  If
;   the table_col_info structure has certain pre-defined tag names 
;   (see below), those column headers will be written to the table.
;   Allows selection of columns.
;
; CALLING SEQUENCE: 
;   write_ipac_table, in_struct, outfile, [table_col_info=table_col_info,
;   table_hdr=table_hdr, select_columns=select_columns, /short_format, 
;   /exact_format, format=format]
;
; INPUTS: 
;   IN_STRUCT -- an IDL structure containing the data.  
;
;   OUTFILE -- string containing the name of the output file
;
; OPTIONAL INPUT:
;
;   TABLE_COL_INFO - A structure with table column headers
;           in tags starting with "HEADER": HEADER_Col_Names, 
;           HEADER_Col_Names_Orig, HEADER_Col_Types_Orig, and, 
;           if present, HEADER_Data_Units and HEADER_Null_Values.
;
;   TABLE_HDR - A string array with whatever comment and keyword
;           lines precede the column headers.
;
;   SELECT_COLUMNS - A string array with the data tags of select
;           columns to output, from tag_names(in_struct).
;           NOTE: user-selected FORMAT keys must match number selected.
;
;   /SHORT_FORMAT -- if set, uses IDL "print" formats.
;
;   /EXACT_FORMAT -- if set, floating point and double precision
;                     data are written with formats of (e16.9) and
;                     (e24.17), respectively.  This option takes
;                     precedence over the /SHORT_FORMAT option.
;
;   FORMAT -- A user supplied format statement that will
;                     override the other formating options.  This
;                     is given in the usual IDL form, e.g. '(f13.6)'.
;     
; OUTPUTS:
;    On completion, an ascii table will be written to the outfile.
;
; PROCEDURES USED:  
;    GET_DATE
;
; NOTES:  
;    The default format is IDL's '(f)', ('d'), etc.
;    The procedure will write out comment lines (starting with
;    "\") if TABLE_HDR is present. Will write out data unit and null
;    value lines if the TABLE_COL_INFO structure has tag names
;    "HEADER_DATA_UNITS" and "HEADER_NULL_VALUES" respectively.  If 
;    "HEADER_COL_NAMES_ORIG" and "HEADER_COL_TYPES_ORIG" are present, 
;    these will be the column names and types.  Currently forces 
;    "RA" and "DEC" to lower case.
;
; EXAMPLE:
;    Write a table with 2 real columns selected with a custom format:
;
;      write_ipac_table,info,'out.txt',table_col_info=table_col_info,
;        table_hdr=table_hdr,select_columns=['RA','DEC'],
;        format='(f12.6,f12.6)'
;
; MODIFICATION HISTORY:
;       Initial version - H. Teplitz, IPAC September 2010 
;       Output original column names/types if present - T. Brooke, IPAC Jun 2013
;       Print out filename - TYB Jan 2016
;       Re-do structures to separate out data; allow selection of
;       columns - TYB Aug 2017
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

IF NOT(keyword_set(in_struct)) OR NOT(keyword_set(outfile)) THEN BEGIN
   print, 'Syntax - write_ipac_table, in_struct, outfile,'
   print, '         [table_col_info=table_col_info, table_hdr=table_hdr,'
   print, '          select_columns=select_columns, /short, /exact, format=]'
   return
ENDIF

print,'Output filename: ', outfile

n_data_rows = n_elements(in_struct)

IF n_data_rows lt 1 THEN BEGIN 
  print, 'QUIT: No data found.'
  return
ENDIF

;;;; find the header tags

units_idx = -1
null_idx = -1
orig_names_idx = -1
orig_types_idx = -1
IF keyword_set(table_col_info) THEN BEGIN
  tag_names_string = tag_names(table_col_info)
  units_idx = where(strmatch(strupcase(tag_names_string), 'HEADER_DATA_UNITS'))
  null_idx = where(strmatch(strupcase(tag_names_string), 'HEADER_NULL_VALUES'))
  orig_names_idx = where(strmatch(strupcase(tag_names_string), 'HEADER_COL_NAMES_ORIG'))
  orig_types_idx = where(strmatch(strupcase(tag_names_string), 'HEADER_COL_TYPES_ORIG'))
ENDIF

;;;; if no data units but null values, fill units with empty strings

IF (units_idx lt 0 and null_idx ge 0) THEN BEGIN
  dum_units=replicate(' ', n_elements(table_col_info.(null_idx)))
  n_old_tags = n_elements (tag_names(table_col_info))
  table_col_info = create_struct(table_col_info, 'HEADER_Data_Units', dum_units)
  units_idx = n_old_tags
ENDIF  

;;;; find the data tags

data_tag_names_string = tag_names(in_struct)
n_data = n_elements(data_tag_names_string)

IF n_data lt 1 THEN BEGIN 
  print, 'ERROR:  Insufficient column information'
  return
ENDIF

igot_data_tag = intarr(n_data) + 1
igot_select_tag = igot_data_tag*0 - 1

;;;; check for select_columns

IF keyword_set(select_columns) THEN BEGIN
  igot_hit = 0
  n_select = n_elements(select_columns)
  IF ( (n_select lt 1) OR (n_select gt n_data) ) THEN BEGIN 
    print, 'ERROR:  Incorrect select column information.'
    return
  ENDIF
  igot_data_tag = igot_data_tag*0
  FOR i = 0, n_data-1 DO BEGIN
    FOR j = 0, n_select-1 DO BEGIN
      IF (strcmp(data_tag_names_string[i],select_columns[j],/FOLD_CASE) EQ 1) THEN BEGIN 
        igot_data_tag[i] = 1
        igot_select_tag[i] = j
        igot_hit = 1
      ENDIF
    ENDFOR
  ENDFOR
  IF igot_hit eq 0 THEN BEGIN 
    print, 'ERROR:  No matches to select columns.'
    return
  ENDIF
ENDIF

;;;; now can replace column names by original names if desired

IF orig_names_idx ge 0 THEN BEGIN
   IF ( n_data eq n_elements(table_col_info.HEADER_Col_Names_Orig) ) THEN BEGIN
     data_tag_names_string = table_col_info.HEADER_Col_Names_Orig
   ENDIF
ENDIF

;;;; ra and dec will be written as lowercase, so find them and change 

ra_idx = where(strcmp(data_tag_names_string,'RA',/FOLD_CASE) EQ 1, nra)
dec_idx = where(strcmp(data_tag_names_string,'DEC',/FOLD_CASE) EQ 1, ndec)

radec_idx = intarr(n_elements(data_tag_names_string))

IF nra GT 0 THEN BEGIN 
   radec_idx[ra_idx] = 1
   data_tag_names_string[ra_idx] = 'ra'
ENDIF

IF ndec GT 0 THEN BEGIN 
   radec_idx[dec_idx] = 1
   data_tag_names_string[dec_idx] = 'dec'
ENDIF

;;;;;; parse the format string to find delimeters

IF keyword_set(short_format) THEN short_fmt = 1 ELSE short_fmt = 0 

IF keyword_set(exact_format) THEN exact_fmt =1 ELSE exact_fmt = 0

n_check = n_data
IF keyword_set(format) THEN BEGIN 
   
   len = strlen(format)
   user_fmt = 1 
   IF strmid(format,0, 1) EQ '(' THEN strput, format, ' ', 0 ELSE format=' '+format
   len = strlen(format)
   IF strmid(format,len-1, 1) EQ ')' THEN strput, format, ' ', len-1 ELSE format=format+' '
   
   len = strlen(format)
   fmt_line_length = len
   subline= format
   
   delim_idx = [0]
   eol=0
   WHILE NOT(eol) DO BEGIN 
      char = strpos(subline,',')
      IF char NE -1 THEN begin
         strput, subline, 'x', char
         delim_idx = [delim_idx, char]
      ENDIF $
      ELSE eol=1
   ENDWHILE
   IF keyword_set(select_columns) THEN n_check = n_select 
   IF n_elements(delim_idx) NE n_check THEN BEGIN 
      print, 'ERROR:  Format statement has the wrong number of elements'
      return
   ENDIF
   delim_idx = [delim_idx, len-1]
ENDIF ELSE BEGIN 
   user_fmt = 0 
ENDELSE

;;;;;  create format array

fmt_arr = strarr(n_data)
data_type_string = strarr(n_data)

FOR i = 0, n_data-1 DO BEGIN 
   IF (igot_data_tag[i] EQ 1) THEN BEGIN
     IF NOT(user_fmt) THEN BEGIN
        type = size(in_struct.(i),/type)
        CASE type OF 
           3: fmt_arr[i]='i'
           4: BEGIN 
             IF exact_fmt THEN fmt_arr[i]='e16.9' $
             ELSE IF short_fmt then fmt_arr[i]='' ELSE fmt_arr[i]='f' 
             IF short_fmt AND radec_idx[i] THEN fmt_arr[i]='f13.6'
           END
           5: BEGIN 
             IF exact_fmt THEN fmt_arr[i]='e24.17' $
             ELSE IF short_fmt THEN fmt_arr[i]='' ELSE fmt_arr[i]='d' 
             IF short_fmt AND radec_idx[i] THEN fmt_arr[i]='f13.6'
           END
           7: fmt_arr[i]='a'  
          14: fmt_arr[i]='i'  
          ELSE: stop
        ENDCASE
     ENDIF ELSE BEGIN
       j = igot_select_tag[i]
       between_delim = delim_idx[j+1]-delim_idx[j]-1
       fmt_arr[i] = strmid(format,delim_idx[j]+1,between_delim)
     ENDELSE   
  ENDIF
ENDFOR

;;;; find width of each column

max_len_arr = intarr(n_data)

FOR i = 0, n_data-1 DO BEGIN
   IF (igot_data_tag[i] EQ 1) THEN BEGIN
     IF fmt_arr[i] NE '' THEN curr_fmt = '('+fmt_arr[i]+')' ELSE curr_fmt = ''
     tmp_string = string(in_struct.(i),format=curr_fmt)
     data_len = max(strlen(tmp_string))
     IF units_idx GE 0 THEN units_len = strlen(table_col_info.(units_idx)[i]) ELSE units_len = 0
     IF null_idx GE 0 THEN null_len = strlen(table_col_info.(null_idx)[i]) ELSE null_len = 0
     IF (orig_types_idx ge 0) THEN BEGIN
       type_len = strlen(table_col_info.HEADER_Col_Types_Orig[i])
     ENDIF ELSE BEGIN
       sz = size(in_struct[0].(i), /tname)
       IF sz EQ 'STRING' THEN sz = 'CHAR'
       IF sz EQ 'LONG' THEN sz = 'INT'
       IF sz EQ 'LONG64' THEN sz = 'LONG'
       type_len = strlen(sz)
     ENDELSE
     tag_len = strlen(data_tag_names_string[i])
     len_arr = [tag_len, type_len, units_len, null_len, data_len]
     max_len_arr[i] = max(len_arr)
  ENDIF      
ENDFOR   

;;;;  construct the header rows

name_row = '|'
type_row = '|'
units_row = '|'
null_row = '|'

include_orig = 0
include_units = 0
include_null = 0
IF (orig_types_idx ge 0) THEN BEGIN
  IF (n_data eq n_elements(table_col_info.(orig_types_idx))) THEN include_orig = 1
ENDIF
IF (units_idx ge 0) THEN BEGIN
  IF (n_data eq n_elements(table_col_info.(units_idx))) THEN include_units = 1
ENDIF
IF (null_idx ge 0) THEN BEGIN
  IF (n_data eq n_elements(table_col_info.(null_idx))) THEN include_null = 1
ENDIF

FOR i = 0, n_data-1 DO BEGIN
  IF (igot_data_tag[i] EQ 1) THEN BEGIN
     name_row = name_row + ' '+strn(data_tag_names_string[i],len=max_len_arr[i])+' |'
     IF (include_orig eq 1) THEN BEGIN
       type_row = type_row + ' '+strn(table_col_info.HEADER_Col_Types_Orig[i],len=max_len_arr[i])+' |'
     ENDIF ELSE BEGIN
       sz = size(in_struct[0].(i), /tname)
       IF sz EQ 'STRING' THEN sz = 'CHAR'
       IF sz EQ 'LONG' THEN sz = 'INT'
       IF sz EQ 'LONG64' THEN sz = 'LONG'
       type_row = type_row + ' '+strn(sz,len=max_len_arr[i])+' |'
     ENDELSE
     IF (include_units eq 1) THEN units_row = units_row + ' '+strn(table_col_info.(units_idx)[i],len=max_len_arr[i])+' |'
     IF (include_null eq 1) THEN null_row = null_row + ' '+strn(table_col_info.(null_idx)[i],len=max_len_arr[i])+' |'
  ENDIF
ENDFOR

openw, lun, outfile, /get_lun

;;;;  write out the data

get_date, dte, /time
printf, lun, '\created '+ string(dte)

IF keyword_set(table_hdr) THEN BEGIN 
   n_table_hdr = n_elements(table_hdr)
   IF n_table_hdr gt 0 THEN BEGIN
     FOR i = 0, n_table_hdr - 1 DO printf, lun, table_hdr[i]
   ENDIF
ENDIF

printf, lun, name_row
IF (type_row ne '|') THEN printf, lun, type_row
IF (include_units eq 1 and units_row ne '|') THEN printf, lun, units_row
IF (include_null eq 1 and null_row ne '|') THEN printf, lun, null_row

FOR j = 0, n_data_rows-1 DO BEGIN 
   out_string = ' '
   FOR i = 0, n_data-1 DO BEGIN
     IF (igot_data_tag[i] EQ 1) THEN BEGIN
       IF fmt_arr[i] NE '' THEN curr_fmt = '('+fmt_arr[i]+')' ELSE curr_fmt = ''      
       data_string = strn(in_struct[j].(i),format=curr_fmt,len=max_len_arr[i])
       out_string = out_string + '  ' + data_string + ' '
     ENDIF
   ENDFOR
   printf, lun, out_string
ENDFOR

close, lun
free_lun, lun

end
