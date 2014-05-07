PRO write_ipac_table, in_struct, outfile, short_format=short_format, exact_format=exact_format, format=format

;+
; NAME: 
;   WRITE_IPAC_TABLE
;
; PURPOSE:  
;   Write an IPAC table from an IDL structure.
;
; EXPLANATION:  
;   Writes an IPAC table to a file from an IDL structure.  If the
;   structure has certain pre-defined tag names (see below), the 
;   header information will be written to the table.
;
; CALLING SEQUENCE: 
;   write_ipac_table, in_struct, outfile, [/short_format, /exact_format, format=format]
;
; INPUTS: 
;   IN_STRUCT -- an IDL structure containing the table to be written to the output
;           ascii file.  Header information must be in the keywords, HEADER_TABLE_HEADER,
;           HEADER_DATA_UNITS, and HEADER_NULL_VALUES.  
;
;   OUTFILE -- string containing the name of the output file
;
; OPTIONAL INPUT:
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
;    The procedure will write out header lines (lines starting with "\"), data unit and null
;    value lines if the structure has tag names "HEADER_TABLE_HEADER",
;    "HEADER_DATA_UNITS" and "HEADER_NULL_VALUES"
;    respectively.  If "HEADER_COL_NAMES_ORIG" and
;    "HEADER_COL_TYPES_ORIG" are present, these will
;    be the column names and types.  Currently forces "RA" and "DEC" to lower case.
;
; MODIFICATION HISTORY:
;       Initial version - H. Teplitz, IPAC September 2010 
;       Output original column names/types if present - T. Brooke, IPAC June 2013
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
   print, '         [/short, /exact, format=]'
   return
ENDIF

;;;; find the header tags

tag_names_string = tag_names(in_struct)

header_tag_idx = where(strmatch(tag_names_string, 'HEADER*') EQ 1, n_header)

units_idx = where(strmatch(strupcase(tag_names_string), '*DATA_UNITS*'))
null_idx = where(strmatch(strupcase(tag_names_string), '*NULL_VALUES*'))
input_header_idx = where(strmatch(strupcase(tag_names_string), '*TABLE_HEADER*'))

data_tag_idx = where(strmatch(tag_names_string, 'HEADER*') EQ 0, n_data)

orig_names_idx = where(strmatch(strupcase(tag_names_string), '*COL_NAMES_ORIG') eq 1, n_orig)

orig_types_idx = where(strmatch(strupcase(tag_names_string), '*COL_TYPES_ORIG') eq 1, n_t_orig)

IF (n_orig gt 0) THEN BEGIN
   tag_names_string[data_tag_idx] = in_struct.HEADER_COL_NAMES_ORIG
ENDIF

n_data_rows = n_elements(in_struct.(data_tag_idx[0]))

;;;; ra and dec will be written as lowercase, so find them and change 

ra_idx = where(strcmp(tag_names_string,'RA',/FOLD_CASE) EQ 1, nra)
dec_idx = where(strcmp(tag_names_string,'DEC',/FOLD_CASE) EQ 1, ndec)

radec_idx = intarr(n_elements(tag_names_string))

IF nra GT 0 THEN BEGIN 
   radec_idx[ra_idx] = 1
   tag_names_string[ra_idx] = 'ra'
ENDIF

IF ndec GT 0 THEN BEGIN 
   radec_idx[dec_idx] = 1
   tag_names_string[dec_idx] = 'dec'
ENDIF

;;;;;; parse the format string to find delimeters

IF keyword_set(short_format) THEN short_fmt = 1 ELSE short_fmt = 0 

IF keyword_set(exact_format) THEN exact_fmt =1 ELSE exact_fmt = 0

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
   IF n_elements(delim_idx) NE n_data THEN BEGIN 
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
   IF NOT(user_fmt) THEN BEGIN
      type = size(in_struct.(data_tag_idx[i]),/type)
      CASE type OF 
         3: fmt_arr[i]='i'
         4: BEGIN 
            IF exact_fmt THEN fmt_arr[i]='e16.9' $
            ELSE IF short_fmt then fmt_arr[i]='' ELSE fmt_arr[i]='f' 
            IF short_fmt AND radec_idx[data_tag_idx[i]] THEN fmt_arr[i]='f13.6'
         END
         
         5: BEGIN 
            IF exact_fmt THEN fmt_arr[i]='e24.17' $
            ELSE IF short_fmt THEN fmt_arr[i]='' ELSE fmt_arr[i]='d' 
            IF short_fmt AND radec_idx[data_tag_idx[i]] THEN fmt_arr[i]='f13.6'
         END
         
         7: fmt_arr[i]='a'  
         14: fmt_arr[i]='i'  
         ELSE: stop
      ENDCASE
     
   ENDIF $
   ELSE BEGIN
      between_delim = delim_idx[i+1]-delim_idx[i]-1
      fmt_arr[i] = strmid(format,delim_idx[i]+1,between_delim)
   ENDELSE   
ENDFOR

;;;; find width of each column

max_len_arr = intarr(n_data)

FOR i = 0, n_data-1 DO BEGIN
   IF fmt_arr[i] NE '' THEN curr_fmt = '('+fmt_arr[i]+')' ELSE curr_fmt = ''
   tmp_string = string(in_struct.(data_tag_idx[i]),format=curr_fmt)
   data_len = max(strlen(tmp_string))
   IF units_idx GE 0 THEN units_len = strlen(in_struct.(header_tag_idx[units_idx])[i]) ELSE units_len = 0
   IF null_idx GE 0 THEN null_len = strlen(in_struct.(header_tag_idx[null_idx])[i]) ELSE null_len = 0
   IF (n_t_orig gt 0) THEN BEGIN
     type_len = strlen(in_struct.header_col_types_orig[i])
   ENDIF ELSE BEGIN
     sz = size(in_struct.(data_tag_idx[i]), /tname)
     IF sz EQ 'STRING' THEN sz = 'CHAR'
     IF sz EQ 'LONG' THEN sz = 'INT'
     IF sz EQ 'LONG64' THEN sz = 'LONG'
     type_len = strlen(sz)
   ENDELSE
   tag_len = strlen(tag_names_string[data_tag_idx[i]])
   len_arr = [tag_len, type_len, units_len, null_len, data_len]
   max_len_arr[i] = max(len_arr)      
endfor   

;;;;  construct the header rows

name_row = '|'
type_row = '|'
units_row = '|'
null_row = '|'

FOR i = 0, n_data-1 DO BEGIN 
   name_row = name_row + ' '+strn(tag_names_string[data_tag_idx[i]],len=max_len_arr[i])+' |'
   IF (n_t_orig gt 0) THEN BEGIN
     type_row = type_row + ' '+strn(in_struct.header_col_types_orig[i],len=max_len_arr[i])+' |'
   ENDIF ELSE BEGIN
     sz = size(in_struct.(data_tag_idx[i]), /tname)
     IF sz EQ 'STRING' THEN sz = 'CHAR'
     IF sz EQ 'LONG' THEN sz = 'INT'
     IF sz EQ 'LONG64' THEN sz = 'LONG'
     type_row = type_row + ' '+strn(sz,len=max_len_arr[i])+' |'
   ENDELSE
   IF units_idx GE 0 THEN $
      units_row = units_row + ' '+strn(in_struct.(header_tag_idx[units_idx])[i],len=max_len_arr[i])+' |'
   IF null_idx GE 0 THEN $
      null_row = null_row + ' '+strn(in_struct.(header_tag_idx[null_idx])[i],len=max_len_arr[i])+' |'
ENDFOR

openw, lun, outfile, /get_lun

;;;;  write out the data rows
get_date, dte, /time
printf, lun, '\created '+string(dte)

IF input_header_idx GE 0 THEN BEGIN 
   n_input_header = n_elements(in_struct.(header_tag_idx[input_header_idx]))
   FOR i = 0, n_input_header - 1 DO printf, lun, in_struct.(header_tag_idx[input_header_idx])[i]
endif

printf, lun, name_row
printf, lun, type_row
IF units_idx GE 0 THEN printf, lun, units_row
IF null_idx GE 0 THEN printf, lun, null_row

FOR j = 0, n_data_rows-1 DO BEGIN 
   out_string = ' '
   FOR i = 0, n_data-1 DO BEGIN 
   IF fmt_arr[i] NE '' THEN curr_fmt = '('+fmt_arr[i]+')' ELSE curr_fmt = ''      
      data_string = strn(in_struct.(data_tag_idx[i])[j],format=curr_fmt,len=max_len_arr[i])
      out_string = out_string + '  ' + data_string+' '
   ENDFOR
   printf, lun, out_string
ENDFOR

close, lun
free_lun, lun

end
