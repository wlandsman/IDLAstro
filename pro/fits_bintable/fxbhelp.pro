	PRO FXBHELP,UNIT
;+
; NAME: 
;	FXBHELP
; Purpose     : 
;	Prints short description of columns in a FITS binary table.
; Explanation : 
;	Prints a short description of the columns in a FITS binary table to the
;	terminal screen.
; Use         : 
;	FXBHELP, UNIT
; Inputs      : 
;	UNIT	= Logical unit number of file opened by FXBOPEN.
; Opt. Inputs : 
;	None.
; Outputs     : 
;	None.
; Opt. Outputs: 
;	None.
; Keywords    : 
;	None.
; Calls       : 
;	FXBFIND, FXBFINDLUN, FXPAR
; Common      : 
;	Uses common block FXBINTABLE--see "fxbintable.pro" for more
;	information.
; Restrictions: 
;	The file must have been opened with FXBOPEN.
; Side effects: 
;	Certain fields may be truncated in the display.
; Category    : 
;	Data Handling, I/O, FITS, Generic.
; Prev. Hist. : 
;	William Thompson, Feb. 1992, from TBHELP by W. Landsman.
; Written     : 
;	William Thompson, GSFC, February 1992.
; Modified    : 
;	Version 1, William Thompson, GSFC, 12 April 1993.
;		Incorporated into CDS library.
;	Version 2, William Thompson, GSFC, 12 May 1993.
;		Modified to not write to a logical unit number assigned to the
;		terminal.  This makes it compatible with IDL for Windows.
;       Version 3, Wayne Landsman GSFC April 2010
;                Remove use of obsolete !ERR system variable
; Version     : 
;	Version 3, April 2010.
;-
;
@fxbintable
	ON_ERROR,2
	COMPILE_OPT IDL2
;
;  Check the number of parameters.
;
	IF N_PARAMS() LT 1 THEN MESSAGE,'Syntax:  FXBHELP, UNIT'
;
;  Get the header.
;
	ILUN = FXBFINDLUN(UNIT)
	HEADER = HEAD[*,ILUN]
;
;  Get the extension name.
;
	EXTNAME = FXPAR(HEADER,'EXTNAME', COUNT=N_EXTNAME)
	IF N_EXTNAME LE 0 THEN EXTNAME = ''
;
;  Print the labels.
;
	PRINT,' '
	PRINT,'FITS Binary Table:  ' + EXTNAME
	PRINT,'Table contains ' + STRTRIM(TFIELDS[ILUN],2) +	$
		' columns, by ' + STRTRIM(NAXIS2[ILUN],2) + ' rows'
	PRINT,' '
	T_FORMAT = 26	;Starting column for Format/Size
	T_UNITS = 46	;Starting column for Units
	T_NULL = 58	;Starting column for Null
	PRINT,FORMAT="('Col',2X,'Name',T" + STRTRIM(T_FORMAT,2) +	$
		",'Type Size',T" + STRTRIM(T_UNITS,2) + ",'Units',T" + $
		STRTRIM(T_NULL,2) + ",6X,'Null')"
	PRINT,' '
;
;  Get the values of the information to be printed.
;
	FXBFIND,HEADER,'TDIM', COL,TDIM0, N_FOUND,''
	FXBFIND,HEADER,'TUNIT',COL,TUNIT0,N_FOUND,''
;
	FXBFIND,HEADER,'TNULL',COL,TNULL0,N_FOUND
	SNULL = STRARR(TFIELDS[ILUN])
	IF N_FOUND GT 0 THEN FOR I = 0,N_ELEMENTS(COL)-1 DO	$
		SNULL[COL[I]-1] = STRTRIM(TNULL0[I],2)
;
;  Print the column information.
;
	FOR ICOL = 0,TFIELDS[ILUN]-1 DO BEGIN
		CASE FORMAT[ICOL,ILUN] OF
			'L':  TYPE0 = 'Log'
			'A':  TYPE0 = 'Asc'
			'B':  TYPE0 = 'Byt'
			'I':  TYPE0 = 'Int'
			'J':  TYPE0 = 'Lng'
			'E':  TYPE0 = 'Flt'
			'D':  TYPE0 = 'Dbl'
			'C':  TYPE0 = 'Cmp'
			'M':  TYPE0 = 'DbC'
			'X':  TYPE0 = 'Bit'
		ENDCASE			
		IF MAXVAL[ICOL,ILUN] GT 0 THEN BEGIN
			ELEM = MAXVAL[ICOL,ILUN]
			IF FORMAT[ICOL,ILUN] EQ 'M' THEN ELEM = ELEM/2
			ELEM = "< " + STRTRIM(ELEM,2)
		END ELSE IF TDIM0[ICOL] NE '' THEN BEGIN
			ELEM = TDIM0[ICOL]
		END ELSE BEGIN
			ELEM = N_ELEM[ICOL,ILUN]
			IF FORMAT[ICOL,ILUN] EQ 'M' THEN ELEM = ELEM/2
			ELEM = STRTRIM(ELEM,2)
		ENDELSE
		PRINT,ICOL+1,TTYPE[ICOL,ILUN],TYPE0,ELEM,		$
			TUNIT0[ICOL],SNULL[ICOL], FORMAT='(I3,2X,A,T' +	$
			STRTRIM(T_FORMAT-2,2) + ',2X,A3,2X,A,T' +	$
			STRTRIM(T_UNITS-2,2) + ',2X,A,T' +		$
			STRTRIM(T_NULL-2,2) + ',2X,A10)'
	ENDFOR
	PRINT,' '
;
	RETURN
	END

