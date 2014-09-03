FUNCTION STR_INDEX, str, substr, offset
;+
; NAME:
;       STR_INDEX()
;
; PURPOSE:
;       Get indices of a substring (SUBSTR) in string.
;
; EXPLANATION:
;       The IDL intrinsic function STRPOS returns only the index of the first
;       occurrence of a substring. This routine calls itself recursively to get
;       indices of the remaining occurrences.
;
; CALLING SEQUENCE:
;       result= STR_INDEX(str, substr [, offset])
;
; INPUTS:
;       STR    -- The string in which the substring is searched for
;       SUBSTR -- The substring to be searched for within STR
;
; OPTIONAL INPUTS:
;       OFFSET -- The character position at which the search is begun. If
;                 omitted or being negative, the search begins at the first
;                 character (character position 0).
;
; OUTPUTS:
;       RESULT -- Integer scalar or vector containing the indices of SUBSTR
;                 within STR. If no substring is found, it is -1.
;
; CALLS:
;       DELVARX
;
; COMMON BLOCKS:
;       STR_INDEX -- internal common block. The variable save in the block is
;                    deleted upon final exit of this routine.
;
; CATEGORY:
;       Utility, string
;
; MODIFICATION HISTORY:
;       Written January 3, 1995, Liyun Wang, GSFC/ARC
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Use size(/TNAME) instead of DATATYPE()   W. Landsman   October 2001
;       
;-
;
   ON_ERROR, 2
   COMMON str_index, idx

   IF N_PARAMS() LT 2 THEN MESSAGE,'Syntax: str_index, str, substr [,offset]'

   IF size(str,/TNAME) NE 'STRING' OR size(substr,/TNAME) NE 'STRING' THEN $
      MESSAGE, 'The first two input parameters must be of string type.'

   IF N_ELEMENTS(offset) EQ 0 THEN pos = 0 ELSE pos = offset
   aa = STRPOS(str,substr,pos)
   IF aa NE -1 THEN BEGIN
      IF N_ELEMENTS(idx) EQ 0 THEN idx = aa ELSE idx = [idx,aa]
      bb = str_index(str,substr,aa+1)
      RETURN, bb
   ENDIF ELSE BEGIN
      IF N_ELEMENTS(idx) NE 0 THEN BEGIN
         result = idx
         delvarx, idx
      ENDIF ELSE result = -1
      RETURN, result
   ENDELSE
END
