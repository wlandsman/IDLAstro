	PRO FXBFINISH, UNIT, ERRMSG=ERRMSG
;+
; NAME: 
;	FXBFINISH
; Purpose     : 
;	Close a FITS binary table extension file opened for write.
; Explanation : 
;	Closes a FITS binary table extension file that had been opened for
;	write by FXBCREATE.
; Use         : 
;	FXBFINISH, UNIT
; Inputs      : 
;	UNIT	= Logical unit number of the file.
; Opt. Inputs : 
;	None.
; Outputs     : 
;	None.
; Opt. Outputs: 
;	None.
; Keywords    : 
;	ERRMSG	= If defined and passed, then any error messages will be
;		  returned to the user in this parameter rather than
;		  depending on the MESSAGE routine in IDL.  If no errors are
;		  encountered, then a null string is returned.  In order to
;		  use this feature, ERRMSG must be defined first, e.g.
;
;			ERRMSG = ''
;			FXBFINISH, ERRMSG=ERRMSG, ...
;			IF ERRMSG NE '' THEN ...
;
; Calls       : 
;	None.
; Common      : 
;	Uses common block FXBINTABLE--see "fxbintable.pro" for more
;	information.
; Restrictions: 
;	The file must have been opened with FXBCREATE, and written with
;	FXBWRITE.
; Side effects: 
;	Any bytes needed to pad the file out to an integral multiple of 2880
;	bytes are written out to the file.  Then, the file is closed.
; Category    : 
;	Data Handling, I/O, FITS, Generic.
; Prev. Hist. : 
;	W. Thompson, Jan 1992.
;	W. Thompson, Feb 1992, modified to support variable length arrays.
;	W. Thompson, Feb 1992, removed all references to temporary files.
; Written     : 
;	William Thompson, GSFC, January 1992.
; Modified    : 
;	Version 1, William Thompson, GSFC, 12 April 1993.
;		Incorporated into CDS library.
;	Version 2, William Thompson, GSFC, 21 July 1993.
;		Fixed bug with variable length arrays.
;	Version 3, William Thompson, GSFC, 31 May 1994
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
	IF N_PARAMS() NE 1 THEN BEGIN
		MESSAGE = 'Syntax:  FXBFINISH, UNIT'
		IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
			ERRMSG = MESSAGE
			RETURN
		END ELSE MESSAGE, MESSAGE
	ENDIF
;
;  Find the index of the file.
;
	ILUN = WHERE(LUN EQ UNIT,NLUN)
	ILUN = ILUN[0]
	IF NLUN EQ 0 THEN BEGIN
		MESSAGE = 'Unit ' + STRTRIM(UNIT,2) +	$
			' not opened properly'
		IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
			ERRMSG = MESSAGE
			RETURN
		END ELSE MESSAGE, MESSAGE
	ENDIF
;
;  Make sure the file was opened for write access.
;
	IF STATE[ILUN] NE 2 THEN BEGIN
		MESSAGE = 'Unit ' + STRTRIM(UNIT,2) +	$
			' not opened for write access'
		IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
			ERRMSG = MESSAGE
			RETURN
		END ELSE MESSAGE, MESSAGE
	ENDIF
;
;  Calculate how many bytes are needed to pad out the file.
;
	OFFSET = NHEADER[ILUN] + HEAP[ILUN] + DHEAP[ILUN]
	NPAD = OFFSET MOD 2880
	IF NPAD NE 0 THEN BEGIN
		NPAD = 2880 - NPAD
		POINT_LUN,UNIT,OFFSET
		WRITEU,UNIT,BYTARR(NPAD)
	ENDIF
;
;  If variable sized arrays were written out to the file, then the PCOUNT value
;  must be updated.  It is taken for granted that PCOUNT is the sixth keyword
;  down, and the value is inserted right justified to column 30.
;
	PCOUNT = HEAP[ILUN] + DHEAP[ILUN] - NAXIS1[ILUN]*NAXIS2[ILUN]
	IF PCOUNT GT 0 THEN BEGIN
		PCOUNT = STRTRIM(PCOUNT,2)
		POINT_LUN,UNIT,MHEADER[ILUN] + 430 - STRLEN(PCOUNT)
		WRITEU,UNIT,PCOUNT
	ENDIF
;	
;  Close the file, mark it as closed, and return.
;
	FREE_LUN,UNIT
	STATE[ILUN] = 0
;
	IF N_ELEMENTS(ERRMSG) NE 0 THEN ERRMSG = ''
	RETURN
	END
