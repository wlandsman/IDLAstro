FUNCTION TO_HEX, D, NCHAR
;+
; NAME:
;       TO_HEX
; PURPOSE:
;       Translate a non-negative decimal integer to a hexadecimal string
; CALLING SEQUENCE:
;       HEX = TO_HEX( D, [ NCHAR ] )
; INPUTS:
;       D - non-negative decimal integer, scalar or vector.  If input as a
;           string, (e.g. '32') then all leading blanks are removed.
;
; OPTIONAL INPUT:
;       NCHAR - number of characters in the output hexadecimal string.
;               If not supplied, then the hex string will contain no 
;               leading zeros.
;
; OUTPUT:
;       HEX - hexadecimal translation of input integer, string
;
; EXAMPLES:
;       IDL> A = TO_HEX([11,16])    ==>   A = ['B','10']
;       IDL> A = TO_HEX(100,3) ==>   A = '064'
;
; METHOD:
;       The hexadecimal format code '(Z)' is used to convert.  No parameter
;       checking is done.
; PROCEDURES CALLED:
;       None.
; REVISION HISTORY:
;       Written   W. Landsman         November, 1990
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Use FSTRING() for more than 1024 values      March 2000 
;       Assume since  V5.4, omit FSTRING() call      April 2006
;-

  if N_elements(nchar) EQ 0 then format = '(Z)' else begin
      ch = strtrim( nchar, 2 ) 
      format = '(Z' + ch + '.' + ch + ')'
  endelse

  return, strtrim( string(d, FORM = format), 2)

  end
