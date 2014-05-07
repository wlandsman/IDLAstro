	FUNCTION FXBTDIM, TDIM_KEYWORD
;+
; NAME: 
;	FXBTDIM()
; Purpose     : 
;	Parse TDIM-like kwywords.
; Explanation : 
;	Parses the value of a TDIM-like keyword (e.g. TDIMnnn, TDESC, etc.) to
;	return the separate elements contained within.
; Use         : 
;	Result = FXBTDIM( TDIM_KEYWORD )
; Inputs      : 
;	TDIM_KEYWORD	= The value of a TDIM-like keyword.  Must be a
;			  character string of the form "(value1,value2,...)".
;			  If the parentheses characters are missing, then the
;			  string is simply returned as is, without any further
;			  processing.
; Opt. Inputs : 
;	None.
; Outputs     : 
;	The result of the function is a character string array containing the
;	values contained within the keyword parameter.  If a numerical result
;	is desired, then simply call, e.g.
;
;		Result = FIX( FXBTDIM( TDIM_KEYWORD ))
;
; Opt. Outputs: 
;	None.
; Keywords    : 
;	None.
; Calls       : 
;	GETTOK
; Common      : 
;	None.
; Restrictions: 
;	The input parameter must have the proper format.  The separate values
;	must not contain the comma character.  TDIM_KEYWORD must not be an
;	array.
; Side effects: 
;	None.
; Category    : 
;	Data Handling, I/O, FITS, Generic.
; Prev. Hist. : 
;	William Thompson, Jan. 1992.
;	William Thompson, Jan. 1993, renamed to be compatible with DOS
;		limitations.
; Written     : 
;	William Thompson, GSFC, January 1992.
; Modified    : 
;	Version 1, William Thompson, GSFC, 12 April 1993.
;		Incorporated into CDS library.
; Version     : 
;	Version 1, 12 April 1993.
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
;
	ON_ERROR,2
;
;  Make sure TDIM_KEYWORD is not an array.
;
	IF N_ELEMENTS(TDIM_KEYWORD) NE 1 THEN MESSAGE,	$
		'TDIM_KEYWORD must be a scalar'
;
;  Remove any leading or trailing blanks from the keyword.
;
	TDIM = STRTRIM(TDIM_KEYWORD,2)
;
;  The first and last characters should be "(" and ")".  If they are not, then
;  simply return the string as is.
;
	FIRST = STRMID(TDIM,0,1)
	LAST  = STRMID(TDIM,STRLEN(TDIM)-1,1)
	IF (FIRST NE "(") OR (LAST NE ")") THEN RETURN,TDIM
;
;  Otherwise, remove the parentheses characters.
;
	TDIM = STRMID(TDIM,1,STRLEN(TDIM)-2)
;
;  Get the first value.
;
	VALUE = GETTOK(TDIM,',')
;
;  Get all the rest of the values.
;
	WHILE TDIM NE '' DO VALUE = [VALUE,GETTOK(TDIM,',')]
;
;  Return the (string) array of values.
;
	RETURN,VALUE
	END
