	FUNCTION FXPARPOS, KEYWRD, IEND, BEFORE=BEFORE, AFTER=AFTER, LAST=LAST
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
;       LAST    = The parameter will be placed just after the last keyword
;                 which is not a blank, COMMENT, or HISTORY record.  Both the
;                 BEFORE and AFTER keywords take precedence over LAST.
;
;	If none of the BEFORE, AFTER, or LAST keywords are passed, then IEND is
;	returned.
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
;       Version 3, 15-Mar-2017, William Thompson, GSFC
;               Test for continue lines when using AFTER option.
;       Version 4, 16-Mar-2017, William Thompson, GSFC, added LAST keyword
;       Version 5, 30-Mar-2017, William Thompson, GSFC, fix bug if AFTER=''
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
            IF STRTRIM(AFTER) NE '' THEN BEGIN
                KEY_AFTER = STRING(REPLICATE(32B,8))
                STRPUT,KEY_AFTER,STRUPCASE(STRTRIM(AFTER,2)),0
                ILOC = WHERE(KEYWRD EQ KEY_AFTER,NLOC)
;
;  Check to see if the keyword is continued.
;
                IF NLOC GT 0 THEN BEGIN
                    IRETURN = (MAX(ILOC)+1)
                    WHILE (IRETURN LT IEND) AND (KEYWRD[IRETURN] EQ 'CONTINUE') $
                    DO IRETURN = IRETURN + 1
                    RETURN, IRETURN
                ENDIF
            ENDIF
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
;  If the LAST keyword was passed, then look for the position of the last
;  keyword which isn't blank, COMMENT, or HISTORY.
;
        IF KEYWORD_SET(LAST) THEN BEGIN
            ILOC = MAX(WHERE((KEYWRD NE '        ') AND $
                             (KEYWRD NE 'COMMENT ') AND $
                             (KEYWRD NE 'HISTORY ') AND $
                             (KEYWRD NE 'END     ') AND (KEYWRD NE '')))
            IF ILOC GT 0 THEN RETURN, ILOC+1
        ENDIF
;
;  Otherwise, simply return IEND.
;
	RETURN,IEND
	END
