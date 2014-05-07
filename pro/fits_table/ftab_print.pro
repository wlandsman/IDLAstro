pro ftab_print,filename,columns,rows, TEXTOUT = textout, FMT = fmt, $
        EXTEN_NO = exten_no, num_header_lines=num_header_lines, $
	nval_per_line=nval_per_line
;+
; NAME:
;       FTAB_PRINT
; PURPOSE:
;       Print the contents of a FITS (binary or ASCII) table extension.
; EXPLANATION:
;       User can specify which rows or columns to print
;
; CALLING SEQUENCE:
;       FTAB_PRINT, filename, columns, rows, 
;                   [ TEXTOUT=, FMT=, EXTEN_NO= NUM_HEADER_LINES ]
;
; INPUTS:
;       filename - scalar string giving name of a FITS file containing a 
;               binary or ASCII table
;       columns - string giving column names, or vector giving
;               column numbers (beginning with 1).  If a string 
;               supplied then column names should be separated by comma's.
;               if not supplied, then all columns are printed.
;               If set to '*' then all columns are printed in table format 
;               (1 row per line, binary tables only).
;       rows - (optional) vector of row numbers to print (beginning with 0).  
;               If not supplied or set to scalar, -1, then all rows
;               are printed.
; OPTIONAL KEYWORD INPUT:
;       EXTEN_NO - Extension number to read.   If not set, then the first 
;               extension is printed (EXTEN_NO=1)
;       FMT = Format string for print display (binary tables only).   If not
;               supplied, then any formats in the TDISP keyword fields will be
;               used, otherwise IDL default formats.    For ASCII tables, the
;               format used is always as stored in the FITS table.
;       NUM_HEADER_LINES - Number of lines to display the column headers (default
;               = 1).  By setting NUM_HEADER_LINES to an integer larger than 1,
;               one can avoid truncation of the headers.   In addition, setting 
;               NUM_HEADER_LINES will display commented lines indicating
;               a FORMAT for reading the data, and a suggested call to 
;              readfmt.pro.    Works for binary tables only
;       NVAL_PER_LINE - The maximum number of values displayed from a 
;               multivalued column when printing in table format.   Default = 6
;       TEXTOUT - scalar number (0-7) or string (file name) determining
;               output device (see TEXTOPEN).  Default is TEXTOUT=1, output 
;               to the user's terminal    
; EXAMPLE:
;       (1) Print all rows of the first 5 columns of the first extension of the
;       file 'wfpc.fits'
;               IDL> ftab_print,'vizier.fits',indgen(5)+1
; 
;       (2) Print all columns of the first row to a file 'vizier.dat' in 
;       'table' format
;               IDL> ftab_print,'vizier.fits',t='vizier.dat','*',0     
; SYSTEM VARIABLES:
;       Uses the non-standard system variables !TEXTOUT and !TEXTUNIT
;       which must be defined (e.g. with ASTROLIB) prior to compilation.
; PROCEDURES USED:
;       FITS_CLOSE, FITS_OPEN, FITS_READ, FTPRINT, TBPRINT
; HISTORY:
;       version 1  W. Landsman    August 1997
;       Check whether data exists   W. Landsman    Feb 2007
;       Check whether extension exists  W. Landsman  Mar 2010
;       Added NUM_HEADER_LINES, NVAL_PER_LINE keywords for binary tables 
;                  W. Landsman Apr 2010
;-
;----------------------------------------------------------------------
 On_error,2
 compile_opt idl2
 if N_params() LT 1 then begin
        print,'Syntax - ftab_print, filename, columns, rows,' 
        print,'              [EXTEN_NO=, FMT= , TEXTOUT=  ]'
        return
 endif

 if not keyword_set(exten_no) then exten_no = 1

 fits_open,filename,fcb
 if fcb.nextend LT exten_no then begin
     message,/CON, $
       'ERROR - Extension ' + strtrim(exten_no,2) + ' not present in FITS file'
     return
 endif      
 
 if fcb.axis[1,exten_no] EQ 0 then begin
     message,/CON, $
      'ERROR - Extension ' + strtrim(exten_no,2) + ' contains no data'
     return
 endif    
 fits_read,fcb,tab,htab,exten_no=exten_no
 fits_close,fcb

 ext_type = fcb.xtension[exten_no]

 case ext_type of
 'A3DTABLE': binary = 1b
 'BINTABLE': binary = 1b
 'TABLE': binary = 0b
 else: message,'ERROR - Extension type of ' + $
                ext_type + ' is not a FITS table format'
 endcase

 if binary then tbprint,htab,tab,columns,rows, TEXTOUT = textout,fmt=fmt, $
                   num_header_lines=num_header_lines,  $
		   nval_per_line=nval_per_line         $
           else ftprint,htab,tab,columns,rows, TEXTOUT = textout
 return
 end
