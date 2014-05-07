	PRO FXBHMAKE,HEADER,NROWS,EXTNAME,COMMENT,DATE=DATE,	$
		INITIALIZE=INITIALIZE,EXTVER=EXTVER,EXTLEVEL=EXTLEVEL,	$
		ERRMSG=ERRMSG
;+
; NAME: 
;	FXBHMAKE
; Purpose     : 
;	Create basic FITS binary table extension (BINTABLE) header.
; Explanation : 
;	Creates a basic header array with all the required keywords, but with
;	none of the table columns defined.  This defines a basic structure
;	which can then be added to or modified by other routines.
; Use         : 
;	FXBHMAKE, HEADER, NROWS  [, EXTNAME  [, COMMENT ]]
; Inputs      : 
;	NROWS	= Number of rows in the binary table.
; Opt. Inputs : 
;	EXTNAME	= If passed, then the EXTNAME record is added with this value.
;	COMMENT = Comment to go along with EXTNAME.
; Outputs     : 
;	HEADER = String array containing FITS extension header.
; Opt. Outputs: 
;	None.
; Keywords    : 
;	INITIALIZE = If set, then the header is completely initialized, and any
;		     previous entries are lost.
;	DATE	   = If set, then the DATE keyword is added to the header.
;	EXTVER	   = Extension version number (integer).
;	EXTLEVEL   = Extension level number (integer).
;	ERRMSG	   = If defined and passed, then any error messages will be
;		     returned to the user in this parameter rather than
;		     depending on the MESSAGE routine in IDL.  If no errors are
;		     encountered, then a null string is returned.  In order to
;		     use this feature, ERRMSG must be defined first, e.g.
;
;			ERRMSG = ''
;			FXBHMAKE, ERRMSG=ERRMSG, ...
;			IF ERRMSG NE '' THEN ...
;
; Calls       : 
;	GET_DATE, FXADDPAR, FXHCLEAN
; Common      : 
;	None.
; Restrictions: 
;	Warning:  No checking is done of any of the parameters.
; Side effects: 
;	None.
; Category    : 
;	Data Handling, I/O, FITS, Generic.
; Prev. Hist. : 
;	William Thompson, Jan 1992.
;	William Thompson, Sep 1992, added EXTVER and EXTLEVEL keywords.
; Written     : 
;	William Thompson, GSFC, January 1992.
; Modified    : 
;	Version 1, William Thompson, GSFC, 12 April 1993.
;		Incorporated into CDS library.
;	Version 2, William Thompson, GSFC, 21 June 1994
;		Added ERRMSG keyword.
;       Version 3, William Thompson, GSFC, 23 June 1994
;               Modified so that ERRMSG is not touched if not defined.
; Version     :
;       Version 3, 23 June 1994
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
;
	ON_ERROR,2
;
;  Check the number of parameters first.
;
	IF N_PARAMS() LT 2 THEN BEGIN
		MESSAGE = 'Calling sequence:  FXBHMAKE, HEADER, NROWS ' + $
			'[, EXTNAME [, COMMENT ]]'
		IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
			ERRMSG = MESSAGE
			RETURN
		END ELSE MESSAGE, MESSAGE
	ENDIF
;
;  If requested, then initialize the header.
;
	IF KEYWORD_SET(INITIALIZE) THEN BEGIN
		HEADER = STRARR(36)
		HEADER[0] = 'END' + STRING(REPLICATE(32B,77))
;
;  Else, if undefined, then initialize the header.
;
	END ELSE IF N_ELEMENTS(HEADER) EQ 0 THEN BEGIN
		HEADER = STRARR(36)
		HEADER[0] = 'END' + STRING(REPLICATE(32B,77))
;
;  Otherwise, make sure that HEADER is a string array, and remove any keywords
;  that describe the format of the file.
;
	END ELSE BEGIN
		SZ = SIZE(HEADER)
		IF (SZ[0] NE 1) OR (SZ[2] NE 7) THEN BEGIN
			MESSAGE = 'HEADER must be a (one-dimensional) ' + $
				'string array'
			IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
				ERRMSG = MESSAGE
				RETURN
			END ELSE MESSAGE, MESSAGE
		ENDIF
		IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
			FXHCLEAN,HEADER,ERRMSG=ERRMSG
			IF ERRMSG EQ '' THEN RETURN
		END ELSE FXHCLEAN,HEADER
	ENDELSE
;
;  Add the required keywords.  Start out with a completely blank table, with no
;  columns.
;
	FXADDPAR,HEADER,'XTENSION','BINTABLE','Written by IDL:  '+ SYSTIME()
	FXADDPAR,HEADER,'BITPIX',8
	FXADDPAR,HEADER,'NAXIS',2,'Binary table'
	FXADDPAR,HEADER,'NAXIS1',0,'Number of bytes per row'
	FXADDPAR,HEADER,'NAXIS2',LONG(NROWS),'Number of rows'
	FXADDPAR,HEADER,'PCOUNT',0,'Random parameter count'
	FXADDPAR,HEADER,'GCOUNT',1,'Group count'
	FXADDPAR,HEADER,'TFIELDS',0,'Number of columns'
;
;  If requested, add the EXTNAME keyword to the header.
;
	IF N_PARAMS() GE 3 THEN BEGIN
		IF N_PARAMS() EQ 3 THEN COMMENT = 'Extension name'
		FXADDPAR,HEADER,'EXTNAME',EXTNAME,COMMENT
	ENDIF
;
;  If requested, add the EXTVER keyword to the header.
;
	IF N_ELEMENTS(EXTVER) EQ 1 THEN		$
		FXADDPAR,HEADER,'EXTVER',LONG(EXTVER),'Extension version'
;
;  If requested, add the EXTLEVEL keyword to the header.
;
	IF N_ELEMENTS(EXTLEVEL) EQ 1 THEN	$
		FXADDPAR,HEADER,'EXTLEVEL',LONG(EXTLEVEL),'Extension level'
;
;  If requested, add the DATE keyword to the header, containing the current
;  date.
;
	IF KEYWORD_SET(DATE) THEN BEGIN
	        GET_DATE,DTE                    ;Get current date as CCYY-MM-DD
        	FXADDPAR,HEADER,'DATE',DTE,'Creation date'
	ENDIF
;
	IF N_ELEMENTS(ERRMSG) NE 0 THEN ERRMSG = ''
	RETURN
	END
