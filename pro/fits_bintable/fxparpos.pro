	FUNCTION FXPARPOS, KEYWRD, IEND, BEFORE=BEFORE, AFTER=AFTER
;+
; NAME: 
;	FXPARPOS()
; Purpose     : 
;	Finds position to insert record into FITS header.
; Explanation : 
;	Finds the position to insert a record into a FITS header.  Called from
;	FXADDPAR.
; Use         : 
;	Result = FXPARPOS(KEYWRD, IEND  [, BEFORE=BEFORE ]  [, AFTER=AFTER ])
; Inputs      : 
;	KEYWRD	= Array of eight-character keywords in header.
;	IEND	= Position of END keyword.
; Opt. Inputs : 
;	None.
; Outputs     : 
;	Result of function is position to insert record.
; Opt. Outputs: 
;	None.
; Keywords    : 
;	BEFORE	= Keyword string name.  The parameter will be placed before the
;		  location of this keyword.  For example, if BEFORE='HISTORY'
;		  then the parameter will be placed before the first history
;		  location.  This applies only when adding a new keyword;
;		  keywords already in the header are kept in the same position.
;
;	AFTER	= Same as BEFORE, but the parameter will be placed after the
;		  location of this keyword.  This keyword takes precedence over
;		  BEFORE.
;
;	If neither BEFORE or AFTER keywords are passed, then IEND is returned.
;
; Calls       : 
;	None.
; Common      : 
;	None.
; Restrictions: 
;	KEYWRD and IEND must be consistent with the relevant FITS header.
; Side effects: 
;	None.
; Category    : 
;	Data Handling, I/O, FITS, Generic.
; Prev. Hist. : 
;	William Thompson, Jan 1992.
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
	ON_ERROR,2				;Return to caller
;
;  Check the number of parameters.
;
	IF N_PARAMS() NE 2 THEN MESSAGE,	$
		'Required parameters are KEYWRD and IEND'
;
;  If the AFTER keyword has been entered, then find the location.
;
	IF N_ELEMENTS(AFTER) EQ 1 THEN BEGIN
		KEY_AFTER = STRING(REPLICATE(32B,8))
		STRPUT,KEY_AFTER,STRUPCASE(STRTRIM(AFTER,2)),0
		ILOC = WHERE(KEYWRD EQ KEY_AFTER,NLOC)
		IF NLOC GT 0 THEN RETURN, (MAX(ILOC)+1) < IEND
	ENDIF
;
;  If AFTER wasn't entered or found, and if the BEFORE keyword has been
;  entered, then find the location.
;
	IF N_ELEMENTS(BEFORE) EQ 1 THEN BEGIN
		KEY_BEFORE = STRING(REPLICATE(32B,8))
		STRPUT,KEY_BEFORE,STRUPCASE(STRTRIM(BEFORE,2)),0
		ILOC = WHERE(KEYWRD EQ KEY_BEFORE,NLOC)
		IF NLOC GT 0 THEN RETURN,ILOC[0]
	ENDIF
;
;  Otherwise, simply return IEND.
;
	RETURN,IEND
	END
