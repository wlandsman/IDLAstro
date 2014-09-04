	PRO FXBTFORM,HEADER,TBCOL,IDLTYPE,FORMAT,NUMVAL,MAXVAL,ERRMSG=ERRMSG
;+
; NAME: 
;	FXBTFORM
; PURPOSE     : 
;	Returns information about FITS binary table columns.
; EXPLANATION : 
;	Procedure to return information about the format of the various columns
;	in a FITS binary table.
; Use         : 
;	FXBTFORM,HEADER,TBCOL,IDLTYPE,FORMAT,NUMVAL,MAXVAL
; Inputs      : 
;	HEADER	= Fits binary table header.
; Opt. Inputs : 
;	None.
; Outputs     : 
;	TBCOL	= Array of starting column positions in bytes.
;	IDLTYPE	= IDL data types of columns.
;	FORMAT	= Character code defining the data types of the columns.
;	NUMVAL	= Number of elements of the data arrays in the columns.
;	MAXVAL	= Maximum number of elements for columns containing variable
;		  length arrays, or zero otherwise.
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
;			FXBTFORM, ERRMSG=ERRMSG, ...
;			IF ERRMSG NE '' THEN ...
;
; Calls       : 
;	FXPAR
; Common      : 
;	None.
; Restrictions: 
;	None.
; Side effects: 
;	None.
; Category    : 
;	Data Handling, I/O, FITS, Generic.
; Prev. Hist. : 
;	W. Thompson, Feb. 1992, from TBINFO by D. Lindler.
;	W. Thompson, Jan. 1993, renamed to be compatible with DOS limitations.
; Written     : 
;	William Thompson, GSFC, February 1992.
; Modified    : 
;	Version 1, William Thompson, GSFC, 12 April 1993.
;		Incorporated into CDS library.
;	Version 2, William Thompson, GSFC, 21 June 1994
;		Added ERRMSG keyword.
;       Version 3, William Thompson, GSFC, 23 June 1994
;               Modified so that ERRMSG is not touched if not defined.
;	Version 4, William Thompson, GSFC, 9 April 1997
;		Modified so that variable length arrays can be read, even if
;		the maximum array size is not in the header.
;	Version 5  Wayne Landsman, GSFC, August 1997
;		Recognize double complex array type if since IDL version 4.0
;       Version 6  Optimized FXPAR call, CM 1999 Nov 18
;       Version 7: Wayne Landsman, GSFC Feb 2006
;               Added support for 64bit integer K format
; Version:
;       Version 8: Wayne Landsman GSFC Apr 2010
;               Remove use of obsolete !ERR variable
;-
;
	ON_ERROR,2
        COMPILE_OPT IDL2
;
;  Check the number of parameters.
;
	IF N_PARAMS() LT 1 THEN BEGIN
		MESSAGE = 'Syntax:  FXBTFORM,HEADER,TBCOL,IDLTYPE,FORMAT,' + $
			'NUMVAL,MAXVAL'
		IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
			ERRMSG = MESSAGE
			RETURN
		END ELSE MESSAGE, MESSAGE
	ENDIF
;
;  Get the number of fields.
;
	TFIELDS = FXPAR(HEADER,'TFIELDS', START=0L, COUNT=N_TFIELDS)
	IF N_TFIELDS LE 0 THEN BEGIN
		MESSAGE = 'Invalid FITS header -- keyword TFIELDS is missing'
		IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
			ERRMSG = MESSAGE
			RETURN
		END ELSE MESSAGE, MESSAGE
	END ELSE IF TFIELDS EQ 0 THEN BEGIN
		MESSAGE = 'FIT binary table has no columns'
		IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
			ERRMSG = MESSAGE
			RETURN
		END ELSE MESSAGE, MESSAGE
	ENDIF
;
;  Initialize the arrays.
;
	WIDTH	= INTARR(TFIELDS)
	IDLTYPE	= INTARR(TFIELDS)
	TBCOL	= LONARR(TFIELDS)
	FORMAT	= STRARR(TFIELDS)
	NUMVAL	= LONARR(TFIELDS)
	MAXVAL	= LONARR(TFIELDS)
;
;  Get the column formats.
;
	TFORM = FXPAR(HEADER,'TFORM*', COUNT=N_TFORM)
	IF N_TFORM LE 0 THEN BEGIN
		MESSAGE = 'Invalid FITS table header -- keyword TFORM ' + $
			'not present'
		IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
			ERRMSG = MESSAGE
			RETURN
		END ELSE MESSAGE, MESSAGE
	ENDIF
	TFORM =  STRUPCASE(STRTRIM(TFORM,2))
;
;  Parse the values of the TFORM keywords.
;
	LEN = STRLEN(TFORM)
	FOR I = 0,N_ELEMENTS(TFORM)-1 DO BEGIN
;
;  Step through each character in the format, until a non-numerical character
;  is encountered.
;
		ICHAR = 0
NEXT_CHAR:
		IF ICHAR GE LEN[I] THEN BEGIN
			MESSAGE = 'Invalid format specification for ' +	$
				'keyword TFORM ' + STRTRIM(I+1)
			IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
				ERRMSG = MESSAGE
				RETURN
			END ELSE MESSAGE, MESSAGE
		ENDIF
		CHAR = STRUPCASE(STRMID(TFORM[I],ICHAR,1))
		IF ((CHAR GE '0') AND (CHAR LE '9')) THEN BEGIN
			ICHAR = ICHAR + 1
			GOTO, NEXT_CHAR
		ENDIF
;
;  Get the number of elements.
;
		IF ICHAR EQ 0 THEN NUMVAL[I] = 1 ELSE	$
			NUMVAL[I] = LONG(STRMID(TFORM[I],0,ICHAR))
;
;  If the character is "P" then the next character is the actual data type,
;  followed by the maximum number of elements surrounded by quotes.
;
		IF CHAR EQ "P" THEN BEGIN
			CHAR = STRUPCASE(STRMID(TFORM[I],ICHAR+1,1))
			MAXVAL[I] = LONG(STRMID(TFORM[I],ICHAR+3,	$
				LEN[I]-ICHAR-4))
			IF MAXVAL[I] EQ 0 THEN MAXVAL[I] = 1
		ENDIF
;
;  Get the IDL data type, and the size of an element.
;
		FORMAT[I] = CHAR
		CASE CHAR OF
			'L':  BEGIN & IDLTYPE[I] = 1 & WIDTH[I] = 1 & END
			'A':  BEGIN & IDLTYPE[I] = 7 & WIDTH[I] = 1 & END
			'B':  BEGIN & IDLTYPE[I] = 1 & WIDTH[I] = 1 & END
			'I':  BEGIN & IDLTYPE[I] = 2 & WIDTH[I] = 2 & END
			'J':  BEGIN & IDLTYPE[I] = 3 & WIDTH[I] = 4 & END
			'E':  BEGIN & IDLTYPE[I] = 4 & WIDTH[I] = 4 & END
			'D':  BEGIN & IDLTYPE[I] = 5 & WIDTH[I] = 8 & END
			'C':  BEGIN & IDLTYPE[I] = 6 & WIDTH[I] = 8 & END
			'M':  BEGIN & IDLTYPE[I] = 9 & WIDTH[I] =16 & END 
			'K':  BEGIN & IDLTYPE[I] =14 & WIDTH[I] = 8 & END 
;
;
;  Treat bit arrays as byte arrays with 1/8 the number of elements.
;
			'X':  BEGIN
				IDLTYPE[I] = 1
				WIDTH[I] = 1
				IF MAXVAL[I] GT 0 THEN BEGIN
					MAXVAL[I] = LONG((MAXVAL[I]+7)/8)
				END ELSE BEGIN
					NUMVAL[I] = LONG((NUMVAL[I]+7)/8)
				ENDELSE
				END

			ELSE:  BEGIN
				MESSAGE = 'Invalid format specification ' + $
					'for keyword TFORM' + STRTRIM(I+1,2)
				IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
					ERRMSG = MESSAGE
					RETURN
				END ELSE MESSAGE, MESSAGE
				END
		ENDCASE
;
;  Variable length array pointers always take up eight bytes.
;
		IF MAXVAL[I] GT 0 THEN WIDTH[I] = 8
;
;  Calculate the starting byte for each column.
;
		IF I GE 1 THEN TBCOL[I] = TBCOL[I-1] + WIDTH[I-1]*NUMVAL[I-1]
	ENDFOR
;
        IF N_ELEMENTS(ERRMSG) NE 0 THEN ERRMSG = ''
	RETURN
	END
