	FUNCTION DETABIFY, CHAR_STR
;+
; NAME:
;	DETABIFY
; PURPOSE:
;	Replaces tabs in character strings with appropriate number of spaces
; EXPLANATION:
;	The number of space characters inserted is calculated to space
;	out to the next effective tab stop, each of which is eight characters
;	apart.
;
; CALLING SEQUENCE:
;	Result = DETABIFY( CHAR_STR )
;
; INPUT PARAMETERS:
;	CHAR_STR = Character string variable (or array) to remove tabs from.
;
; OUTPUT:
;	Result of function is CHAR_STR with tabs replaced by spaces.
;
; RESTRICTIONS:
;	CHAR_STR must be a character string variable.
;
; MODIFICATION HISTORY:
;	William Thompson, Feb. 1992.
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
;
	ON_ERROR, 2
;
;  Check the number of parameters.
;
	IF N_PARAMS() NE 1 THEN MESSAGE,'Syntax:  Result = DETABIFY(CHAR_STR)'
;
;  Make sure CHAR_STR is of type string.
;
	SZ = SIZE(CHAR_STR)
	IF SZ[SZ[0]+1] NE 7 THEN BEGIN
		MESSAGE,/INFORMATIONAL,'CHAR_STR must be of type string'
		RETURN, CHAR_STR
	ENDIF
;
;  Step through each element of CHAR_STR.
;
	STR = CHAR_STR
	FOR I = 0,N_ELEMENTS(STR)-1 DO BEGIN
;
;  Keep looking for tabs until there aren't any more.
;
		REPEAT BEGIN
			TAB = STRPOS(STR[I],STRING(9B))
			IF TAB GE 0 THEN BEGIN
				NBLANK = 8 - (TAB MOD 8)
				STR[I] = STRMID(STR[I],0,TAB) +		$
					STRING(REPLICATE(32B,NBLANK)) +	$
					STRMID(STR[I],TAB+1,STRLEN(STR[I])-TAB-1)
			ENDIF
		ENDREP UNTIL TAB LT 0
	ENDFOR
;
	RETURN, STR
	END
