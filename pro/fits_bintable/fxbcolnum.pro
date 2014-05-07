	FUNCTION FXBCOLNUM, UNIT, COL, ERRMSG=ERRMSG
;+
; NAME: 
;	FXBCOLNUM()
; Purpose     : 
;	Returns a binary table column number.
; Explanation : 
;	Given a column specified either by number or name, this routine will
;	return the appropriate column number.
; Use         : 
;	Result = FXBCOLNUM( UNIT, COL )
; Inputs      : 
;	UNIT	= Logical unit number corresponding to the file containing the
;		  binary table.
;	COL	= Column in the binary table, given either as a character
;		  string containing a column label (TTYPE), or as a numerical
;		  column index starting from column one.
; Opt. Inputs : 
;	None.
; Outputs     : 
;	The result of the function is the number of the column specified, or
;	zero if no column is found (when passed by name).
; Opt. Outputs: 
;	None.
; Keywords    : 
;	ERRMSG	  = If defined and passed, then any error messages will be
;		    returned to the user in this parameter rather than
;		    depending on the MESSAGE routine in IDL.  If no errors are
;		    encountered, then a null string is returned.  In order to
;		    use this feature, ERRMSG must be defined first, e.g.
;
;			ERRMSG = ''
;			Result = FXBCOLNUM( ERRMSG=ERRMSG, ... )
;			IF ERRMSG NE '' THEN ...
;
; Calls       : 
;	None.
; Common      : 
;	Uses common block FXBINTABLE--see "fxbintable.pro" for more
;	information.
; Restrictions: 
;	The binary table file must have been opened with FXBOPEN.
;
;	If COL is passed as a number, rather than as a name, then it must be
;	consistent with the number of columns in the table.
;
; Side effects: 
;	None.
; Category    : 
;	Data Handling, I/O, FITS, Generic.
; Prev. Hist. : 
;	None.
; Written     : 
;	William Thompson, GSFC, 2 July 1993.
; Modified    : 
;	Version 1, William Thompson, GSFC, 2 July 1993.
;	Version 2, William Thompson, GSFC, 29 October 1993.
;		Added error message for not finding column by name.
;	Version 3, William Thompson, GSFC, 21 June 1994
;		Added ERRMSG keyword.
;       Version 4, William Thompson, GSFC, 23 June 1994
;               Modified so that ERRMSG is not touched if not defined.
; Version     :
;       Version 4, 23 June 1994
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
;
@fxbintable
	ON_ERROR, 2
;
;  Check the number of parameters.
;
	IF N_PARAMS() NE 2 THEN BEGIN
		MESSAGE = 'Syntax:  Result = FXBCOLNUM( UNIT, COL )'
		IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
			ERRMSG = MESSAGE
			RETURN, 0
		END ELSE MESSAGE, MESSAGE
	ENDIF
;
;  Find the logical unit number in the FXBINTABLE common block.
;
	ILUN = WHERE(LUN EQ UNIT,NLUN)
	ILUN = ILUN[0]
	IF NLUN EQ 0 THEN BEGIN
		MESSAGE = 'Unit ' + STRTRIM(UNIT,2) + ' not opened properly'
		IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
			ERRMSG = MESSAGE
			RETURN, 0
		END ELSE MESSAGE, MESSAGE
	ENDIF
;
;  If COL is of type string, then search for a column with that label.
;
	SC = SIZE(COL)
	IF SC[SC[0]+1] EQ 7 THEN BEGIN
		SCOL = STRUPCASE(STRTRIM(COL,2))
		ICOL = WHERE(TTYPE[*,ILUN] EQ SCOL, NCOL)
		ICOL = ICOL[0]
		IF ICOL LT 0 THEN BEGIN
			MESSAGE = 'Column "' + SCOL + '" not found'
			IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
				ERRMSG = MESSAGE
				RETURN, 0
			END ELSE MESSAGE, MESSAGE
		ENDIF
;
;  Otherwise, a numerical column was passed.  Check its value.
;
	END ELSE ICOL = LONG(COL) - 1
	IF (ICOL LT 0) OR (ICOL GE TFIELDS[ILUN]) THEN BEGIN
		MESSAGE= 'COL must be between 1 and ' +	$
			STRTRIM(TFIELDS[ILUN],2)
		IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
			ERRMSG = MESSAGE
			RETURN, 0
		END ELSE MESSAGE, MESSAGE
	ENDIF
;
;  Return ICOL as a number between 1 and N.
;
	IF N_ELEMENTS(ERRMSG) NE 0 THEN ERRMSG = ''
	RETURN, ICOL + 1
	END
