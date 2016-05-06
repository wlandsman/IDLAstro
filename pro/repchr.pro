;+
; NAME:
;       REPCHR()
; PURPOSE:
;       Replace all occurrences of one character with another in a string.
;
; CALLING SEQUENCE:
;       New_String = repchr( In_string, OldChar, [NewChar])
; INPUTS:
;       in_string = original text string, scalar or array
;       OldChar = character to replace.     If the OldChar contains
;            more than 1 character, only the first character is used.
; OPTIONAL INPUT:
;       newchar = single character to replace it with.
;                The default is a single space
; OUTPUTS:
;       new_string = same as in_string, but with all occurrences of old
;               replaced  by newchar
; EXAMPLE:
;       in_string = ['lettuce, tomato, grape']
;       print, repchr( in_string, ',')   ;replace comma with space
;            'lettuce tomato grape'
; NOTES: 
;       Use REPSTR() to replace words rather than a single character
;
;	    For a more sophisticated routine that allows regular expressions look
;	    at MG_STRREPLACE() http://docs.idldev.com/idllib/strings/mg_streplace.html
;
;       Since IDL 8.4 one can use the .REPLACE() method for string variables
;
;       Note that REPCHR() is the fastest (though least versatile) of these routines, 
;       because the length of the string never changes, allowing direct manipulation of 
;       byte values.
; MODIFICATION HISTORY:
;       Written W. Landsman   April 2016
;       Adapted from similar code by  R. Sterner JHUAPL Oct, 1986
;-


	function repchr, In_String, OldChar, NewChar
 
	if N_params() LT 2 then begin
	  print,' Replace all occurrences of one character with another '+$
	    'in a text string.'
	  print,' new_string = repchr(In_String, OldChar, [NewChar])'
	  return, -1
	endif
 
	bString = byte(In_String)			   ; convert string to a byte array.
	b_OldChar = byte(OldChar)			   ; convert OldChar to byte.

	g = where(bString EQ b_OldChar[0],Ng)  ; find occurrences of char 1.
	IF Ng EQ 0 then return,In_string	   ; if none, return input string.

    if N_elements(NewChar) EQ 0 then NewChar = ' '   ;Default new char is a space
    b_NewChar = byte(NewChar)              ;Convert NewChar to byte
	bstring[g] = b_NewChar[0]			   ; replace oldchar by newchar.

	return, STRING(bString)		   ; return new string.
	END
