FUNCTION ordinal,num
;+
; NAME:
;	ORDINAL
; PURPOSE:
;	Convert an integer to a correct English ordinal string:
; EXPLANATION:
;	The first four ordinal strings are "1st", "2nd", "3rd", "4th" ....
;
; CALLING SEQUENCE:
;	result = ordinal( num )
;
; INPUT PARAMETERS:
;	num = number to be made an ordinal.  If float, will be FIXed.
;
; OUTPUT PARAMETERS:
;	result = string such as '1st' '3rd' '164th' '87th', etc.
;
; MODIFICATION HISTORY:  
;	Written by R. S. Hill, STX, 8 Aug. 1991
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
On_error,2
num = fix(num)
CASE num MOD 100 OF
   11:  suffix = 'th'
   12:  suffix = 'th'
   13:  suffix = 'th'
   ELSE:  CASE num MOD 10 OF
          1:  suffix = 'st'
          2:  suffix = 'nd'
          3:  suffix = 'rd'
          ELSE: suffix = 'th'
          ENDCASE
ENDCASE
RETURN,strtrim(string(num),2)+suffix
END
