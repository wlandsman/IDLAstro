	PRO FXHCLEAN,HEADER,ERRMSG=ERRMSG
;+
; NAME: 
;	FXHCLEAN
; Purpose     : 
;	Removes required keywords from FITS header.
; Explanation : 
;	Removes any keywords relevant to array structure from a FITS header,
;	preparatory to recreating it with the proper values.
; Use         : 
;	FXHCLEAN, HEADER
; Inputs      : 
;	HEADER	= FITS header to be cleaned.
; Opt. Inputs : 
;	None.
; Outputs     : 
;	HEADER	= The cleaned FITS header is returned in place of the input
;		  array.
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
;			FXHCLEAN, ERRMSG=ERRMSG, ...
;			IF ERRMSG NE '' THEN ...
;
; Calls       : 
;	SXDELPAR, FXPAR
; Common      : 
;	None.
; Restrictions: 
;	HEADER must be a string array containing a properly formatted FITS
;	header.
; Side effects: 
;	Warning:  when cleaning a binary table extension header, not all of the
;	keywords pertaining to columns in the table may be removed.
; Category    : 
;	Data Handling, I/O, FITS, Generic.
; Prev. Hist. : 
;	William Thompson, Jan 1992.
; Written     : 
;	William Thompson, GSFC, January 1992.
; Modified    : 
;	Version 1, William Thompson, GSFC, 12 April 1993.
;		Incorporated into CDS library.
;	Version 2, William Thompson, GSFC, 31 May 1994
;		Added ERRMSG keyword.
;       Version 3, William Thompson, GSFC, 23 June 1994
;               Modified so that ERRMSG is not touched if not defined.
;	Version 4, William Thompson, GSFC, 30 December 1994
;		Added TCUNIn to list of column keywords to be removed.
; Version     :
;       Version 4, 30 December 1994
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
;
	ON_ERROR, 2
;
;  Check the number of input parameters.
;
	IF N_PARAMS() NE 1 THEN BEGIN
		MESSAGE = 'Syntax:  FXHCLEAN, HEADER'
		IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
			ERRMSG = MESSAGE
			RETURN
		END ELSE MESSAGE, MESSAGE
	ENDIF
;
;  Check the type of HEADER.
;
	S = SIZE(HEADER)
	IF (S[0] NE 1) OR (S[2] NE 7) THEN BEGIN
		MESSAGE = 'HEADER must be a (one-dimensional) string array'
		IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
			ERRMSG = MESSAGE
			RETURN
		END ELSE MESSAGE, MESSAGE
	ENDIF
;
;  Start removing the various keywords relative to the structure of the FITS
;  file.
;
	SXDELPAR,HEADER,['SIMPLE','EXTEND','XTENSION','BITPIX','PCOUNT', $
		'GCOUNT','THEAP']
;
;  Get the number of axes as stored in the header.  Then, remove it, and any
;  NAXISnnn keywords implied by it.
;
	NAXIS = FXPAR(HEADER,'NAXIS')
	SXDELPAR,HEADER,'NAXIS'
	IF NAXIS GT 0 THEN FOR I=1,NAXIS DO	$
		SXDELPAR,HEADER,'NAXIS'+STRTRIM(I,2)
;
;  Get the number of columns in a binary table.  Remove any column definitions.
;
	TFIELDS = FXPAR(HEADER,'TFIELDS')
	SXDELPAR,HEADER,'TFIELDS'
	IF TFIELDS GT 0 THEN FOR I=1,TFIELDS DO SXDELPAR,HEADER,	$
		['TFORM','TTYPE','TDIM','TUNIT','TSCAL','TZERO',	$
		'TNULL','TDISP','TDMIN','TDMAX','TDESC','TROTA',	$
		'TRPIX','TRVAL','TDELT','TCUNI']  + STRTRIM(I,2)
;
	IF N_ELEMENTS(ERRMSG) NE 0 THEN ERRMSG = ''
	RETURN
	END
