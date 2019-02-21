	PRO FXBREAD, UNIT, DATA, COL, ROW, NOSCALE=NOSCALE, VIRTUAL=VIR, $
		DIMENSIONS=DIMS0, NANVALUE=NANVALUE, ERRMSG=ERRMSG, $
                NOIEEE=NOIEEE
;+
; NAME: 
;	FXBREAD
; Purpose     : 
;	Read a data array from a disk FITS binary table file.
; Explanation : 
;	Each call to FXBREAD will read the data from one column and one row
;	from the FITS data file, which should already have been opened by
;	FXBOPEN.  One needs to call this routine for every column and every row
;	in the binary table.  FXBCLOSE will then close the FITS data file.
; Use         : 
;	FXBREAD, UNIT, DATA, COL  [, ROW ]
; Inputs      : 
;	UNIT	= Logical unit number corresponding to the file containing the
;		  binary table.
;	COL	= Column in the binary table to read data from, either as a
;		  character string containing a column label (TTYPE), or as a
;		  numerical column index starting from column one.
; Opt. Inputs : 
;	ROW	= Either row number in the binary table to read data from,
;		  starting from row one, or a two element array containing a
;		  range of row numbers to read.  If not passed, then the entire
;		  column is read in.
;
;		  Row must be passed for variable length arrays.
;
; Outputs     : 
;	DATA	= IDL data array to be read from the file.
; Opt. Outputs: 
;	None.
; Keywords    : 
;	NOSCALE	= If set, then the output data will not be scaled using the
;		  optional TSCAL and TZERO keywords in the FITS header.
;		  Default is to scale.
;       NOIEEE  = If set, then the output data is not byte-swapped to 
;                 machine order.  NOIEEE implies NOSCALE.
;                 Default is to perform the byte-swap.
;	VIRTUAL	= If set, and COL is passed as a name rather than a number,
;		  then if the program can't find a column with that name, it
;		  will then look for a keyword with that name in the header.
;		  Such a keyword would then act as a "virtual column", with the
;		  same value for every row.
;	DIMENSIONS = Vector array containing the dimensions to be used to read
;		  in the data.  Bypasses any dimensioning information stored in
;		  the header.  Ignored for bit arrays.  If the data type is
;		  double-precision complex, then an extra dimension of 2 is
;		  prepended to the dimensions passed by the user.
;	NANVALUE= Value signalling data dropout.  All points corresponding to
;		  IEEE NaN (not-a-number) are converted to this number.
;		  Ignored unless DATA is of type float, double-precision or
;		  complex.
;	ERRMSG	= If defined and passed, then any error messages will be
;		  returned to the user in this parameter rather than
;		  depending on the MESSAGE routine in IDL.  If no errors are
;		  encountered, then a null string is returned.  In order to
;		  use this feature, ERRMSG must be defined first, e.g.
;
;			ERRMSG = ''
;			FXBREAD, ERRMSG=ERRMSG, ...
;			IF ERRMSG NE '' THEN ...
;
; Calls       : 
;	FXPAR, WHERE_NEGZERO, WHERENAN
; Common      : 
;	Uses common block FXBINTABLE--see "fxbintable.pro" for more
;	information.
; Restrictions: 
;	The binary table file must have been opened with FXBOPEN.
;
;	The data must be consistent with the column definition in the binary
;	table header.
;
;	The row number must be consistent with the number of rows stored in the
;	binary table header.
;
;	The number of elements implied by the dimensions keyword must not
;	exceed the number of elements stored in the file.
;
; Side effects: 
;	If the DIMENSIONS keyword is used, then the number of data points read
;	in may be less than the number of points stored in the table.
;
;	If there are no elements to read in (the number of elements is zero),
;	then the program sets !ERR to -1, and DATA is unmodified.
;
; Category    : 
;	Data Handling, I/O, FITS, Generic.
; Prev. Hist. : 
;	W. Thompson, Jan 1992.
;	W. Thompson, Feb 1992, modified to support variable length arrays.
;	W. Thompson, Jun 1992, modified way that row ranges are read in.  No
;			       longer works reiteratively.
;	W. Thompson, Jun 1992, fixed bug where NANVALUE would be modified by
;			       TSCAL and TZERO keywords.
;	W. Thompson, Jun 1992, fixed bug when reading character strings.
;			       Treats dimensions better when reading multiple
;			       rows.
; Written     : 
;	William Thompson, GSFC, January 1992.
; Modified    : 
;	Version 1, William Thompson, GSFC, 12 April 1993.
;		Incorporated into CDS library.
;	Version 2, William Thompson, GSFC, 30 June 1993.
;		Added overwrite keyword to REFORM call to speed up.
;	Version 3, William Thompson, GSFC, 21 July 1993.
;		Fixed bug with variable length arrays.
;	Version 4, William Thompson, GSFC, 29 October 1993.
;		Added error message for not finding column by name.
;	Version 5, William Thompson, GSFC, 31 May 1994
;		Added ERRMSG keyword.
;       Version 6, William Thompson, GSFC, 23 June 1994
;               Modified so that ERRMSG is not touched if not defined.
;	Version 7, William Thompson, GSFC, 29 December 1994
;		Fixed bug where single element dimensions were lost.
;	Version 8, William Thompson, GSFC, 20 March 1995
;		Fixed bug introduced in version 7.
;	Version 9, Wayne Landsman, GSFC, 3 July 1996
;		Fixed bug involving use of virtual keyword.
;	Version 10, William Thompson, GSFC, 31-Jan-1997
;		Added call to WHERE_NEGZERO.
;	Version 11, Wayne Landsman, GSFC, 12 Aug, 1997
;		Use IDL dcomplex datatype if needed
;	Version 12, Wayne Landmsan, GSFC, 20 Feb, 1998
;		Remove call to WHERE_NEGZERO (now part of IEEE_TO_HOST)
;	Version 13, 18 Nov 1999, CM, Add NOIEEE keyword
;	Version 14, 21 Aug 2000, William Thompson, GSFC
;		Catch I/O errors
;       Version 15, W. Landsman GSFC 10 Dec 2009
;                Fix Dimension keyword, remove  IEEE_TO_HOST
;       Version 16, William Thompson, 18-May-2016, change POINTER to ULONG
;       Version 17, William Thompson/Terje Fredvik, 30-Aug-2018, preserve
;               original dimensionality
;       Version 18, William Thompson, 31-Aug-2018, correction to v17
; Version     :
;       Version 18, 31-Aug-2018
;-
;
@fxbintable
	ON_ERROR, 2
	ON_IOERROR, HANDLE_IO_ERROR
;
;  Check the number of parameters.
;
	IF N_PARAMS() LT 3 THEN BEGIN
		MESSAGE = 'Syntax:  FXBREAD, UNIT, DATA, COL  [, ROW ]'
		GOTO, HANDLE_ERROR
	ENDIF
;
;  Find the logical unit number in the FXBINTABLE common block.
;
	ILUN = WHERE(LUN EQ UNIT,NLUN)
	ILUN = ILUN[0]
	IF NLUN EQ 0 THEN BEGIN
		MESSAGE = 'Unit ' + STRTRIM(UNIT,2) + ' not opened properly'
		GOTO, HANDLE_ERROR
	ENDIF
;
;  If COL is of type string, then search for a column with that label.
;
	SC = SIZE(COL)
	VIRTUAL = 0
	IF SC[SC[0]+1] EQ 7 THEN BEGIN
		SCOL = STRUPCASE(STRTRIM(COL,2))
		ICOL = WHERE(TTYPE[*,ILUN] EQ SCOL, NCOL)
		ICOL = ICOL[0]
		IF (ICOL LT 0) AND (NOT KEYWORD_SET(VIR)) THEN BEGIN
			MESSAGE = 'Column "' + SCOL + '" not found'
			GOTO, HANDLE_ERROR
		ENDIF
;
;  If the column was not found, and VIRTUAL was set, then search for a keyword
;  by that name.
;
		IF NCOL EQ 0 THEN BEGIN
			IF KEYWORD_SET(VIR) THEN BEGIN
				HEADER = HEAD[*,ILUN]
				VALUE = FXPAR(HEADER,SCOL,COUNT=CC)
				IF CC GT 0 THEN BEGIN
					DATA = VALUE
					VIRTUAL = 1
					GOTO, CHECK_ROW
				ENDIF
			ENDIF
			MESSAGE = 'Column "' + SCOL + '" not found'
			GOTO, HANDLE_ERROR
		ENDIF
;
;  Otherwise, a numerical column was passed.  Check its value.
;
	END ELSE ICOL = LONG(COL) - 1
	IF (ICOL LT 0) OR (ICOL GE TFIELDS[ILUN]) THEN BEGIN
		MESSAGE = 'COL must be between 1 and ' +	$
			STRTRIM(TFIELDS[ILUN],2)
		GOTO, HANDLE_ERROR
	ENDIF
;
;  If there are no elements in the array, then set !ERR to -1.
;
	IF N_ELEM[ICOL,ILUN] EQ 0 THEN BEGIN
		MESSAGE,'Number of elements to read in is zero',/INFORMATIONAL
		!ERR = -1
		RETURN
	ENDIF
;
;  If ROW was not passed, then set it equal to the entire range.  Otherwise,
;  extract the range.
;
CHECK_ROW:
	IF N_PARAMS() EQ 3 THEN ROW = [1,NAXIS2[ILUN]]
	CASE N_ELEMENTS(ROW) OF
		1:  ROW2 = LONG(ROW[0])
		2:  ROW2 = LONG(ROW[1])
		ELSE:  BEGIN
			MESSAGE = 'ROW must have one or two elements'
			GOTO, HANDLE_ERROR
			END
	ENDCASE
	ROW1 = LONG(ROW[0])
;
;  If ROW represents a range, then make sure that the row range is legal, and
;  that reading row ranges is allowed (i.e., the column is not variable length.
;
	IF ROW1 NE ROW2 THEN BEGIN
		MAXROW = NAXIS2[ILUN]
		IF (ROW1 LT 1) OR (ROW1 GT MAXROW) THEN BEGIN
			MESSAGE = 'ROW[0] must be between 1 and ' +	$
				STRTRIM(MAXROW,2)
			GOTO, HANDLE_ERROR
		END ELSE IF (ROW2 LT ROW1) OR (ROW2 GT MAXROW) THEN BEGIN
			MESSAGE = 'ROW[1] must be between ' +	$
				STRTRIM(ROW1,2) + ' and ' + STRTRIM(MAXROW,2)
			GOTO, HANDLE_ERROR
		END ELSE IF NOT VIRTUAL THEN IF MAXVAL[ICOL,ILUN] GT 0 THEN $
				BEGIN
			MESSAGE = 'Row ranges not allowed for ' +	$
				'variable-length columns'
			GOTO, HANDLE_ERROR
		ENDIF
;
;  Otherwise, if ROW is a single number, then just make sure it's valid.
;
	END ELSE BEGIN
		IF (ROW1 LT 1) OR (ROW1 GT NAXIS2[ILUN]) THEN BEGIN
			MESSAGE = 'ROW must be between 1 and ' +	$
				STRTRIM(NAXIS2[ILUN],2)
			GOTO, HANDLE_ERROR
		ENDIF
	ENDELSE
;
;  If a virtual column, then simply return the value.  If necessary, then
;  replicate the value the correct number of times.
;
	IF VIRTUAL THEN BEGIN
		IF ROW1 EQ ROW2 THEN DATA = VALUE ELSE	$
			DATA = REPLICATE(VALUE,ROW2-ROW1+1)
		RETURN
	ENDIF
;
;  Find the position of the first byte of the data array in the file.
;
	OFFSET = NHEADER[ILUN] + NAXIS1[ILUN]*(ROW1-1) + BYTOFF[ICOL,ILUN]
	POINT_LUN,UNIT,OFFSET
;
;  If a variable length array, then read in the number of elements, and the
;  pointer to the variable length array.  Change the pointing.
;
	IF MAXVAL[ICOL,ILUN] GT 0 THEN BEGIN
		POINTER = ULONARR(2)
		READU,UNIT,POINTER
		BYTEORDER, POINTER, /NTOHL
		DIMS = POINTER[0]
		POINT_LUN,UNIT,NHEADER[ILUN] + HEAP[ILUN] + POINTER[1]
;
;  If there are no elements in the array, then set !ERR to -1.
;
		IF DIMS EQ 0 THEN BEGIN
			MESSAGE,'Number of elements to read in is zero', $
				/INFORMATIONAL
			!ERR = -1
			RETURN
		ENDIF
;
;  If the datatype is a bit array, then the array is treated as a byte array
;  with 1/8 the number of elements.
;
		IF FORMAT[ICOL,ILUN] EQ 'X' THEN DIMS = LONG((DIMS+7)/8)
;
;  If fixed length, then get the dimensions of the output array.
;
	END ELSE BEGIN
		DIMS = N_DIMS[*,ICOL,ILUN]
		DIMS = DIMS[1:DIMS[0]]
	ENDELSE
;
;  If the DIMENSIONS keyword has been passed, then use that instead of the
;  dimensions already determined.
;
	IF (N_ELEMENTS(DIMS0) GT 0) AND (FORMAT[ICOL,ILUN] NE 'X')	$
			THEN BEGIN
		IF PRODUCT(DIMS0) GT PRODUCT(DIMS) THEN BEGIN
			MESSAGE = 'Requested dimensions exceeds the ' +	$
				'number of elements'
			GOTO, HANDLE_ERROR
		ENDIF
		DIMS = DIMS0
	ENDIF
;
;  Read in the data.  If a character string array, then read in a byte array.
;
	DATATYPE = IDLTYPE[ICOL,ILUN]
	IF DATATYPE EQ 7 THEN DATATYPE = 1
;
;  If only reading in a single row, then the pointer should already be set.
;  Otherwise, the pointer needs to be set for each row.
;
	IF ROW1 EQ ROW2 THEN BEGIN
		DATA = MAKE_ARRAY(TYPE=DATATYPE,DIMENSION=DIMS)
		DATA = REFORM(DATA,DIMS,/OVERWRITE)
		READU,UNIT,DATA
	END ELSE BEGIN
		DIMS2 = [DIMS, ROW2-ROW1+1]
		DATA = MAKE_ARRAY(TYPE=DATATYPE, DIMENSION=DIMS2)
		DATA = REFORM(DATA, DIMS2, /OVERWRITE)
		TEMPDATA = MAKE_ARRAY(TYPE=DATATYPE, DIMENSION=DIMS)
		TEMPDATA = REFORM(TEMPDATA, DIMS, /OVERWRITE)
		NTEMP = N_ELEMENTS(TEMPDATA)
		FOR IROW = ROW1,ROW2 DO BEGIN
			OFFSET = NHEADER[ILUN] + BYTOFF[ICOL,ILUN]
			POINT_LUN,UNIT,OFFSET + NAXIS1[ILUN]*(IROW-1)
			READU,UNIT,TEMPDATA
			DATA[(IROW-ROW1)*NTEMP] = TEMPDATA[*]
		ENDFOR
	ENDELSE
;
;  If a character string array, then convert to type string.
;
	IF IDLTYPE[ICOL,ILUN] EQ 7 THEN BEGIN
		DATA = STRING(DATA)
		COUNT = 0
;
;  Otherwise, if necessary, then convert the data to the native format of the
;  host machine.  Also, if NANVALUE is passed, then keep track of any IEEE NaN
;  values.
;
	END ELSE IF IDLTYPE[ICOL,ILUN] NE 1 THEN BEGIN
		IF (N_ELEMENTS(NANVALUE) EQ 1) AND (IDLTYPE[ICOL,ILUN] GE 4) $
			AND (IDLTYPE[ICOL,ILUN] LE 6) THEN	$
			W = WHERENAN(DATA,COUNT) ELSE COUNT = 0
                IF NOT KEYWORD_SET(NOIEEE) THEN $
		       SWAP_ENDIAN_INPLACE,DATA,/SWAP_IF_LITTLE 
	END ELSE COUNT = 0
;
;  If DIMS is simply the number 1, then convert DATA either to a scalar or to a
;  simple vector, depending on how many rows were read in.
;
	IF (N_ELEMENTS(DIMS) EQ 1) AND (DIMS[0] EQ 1) THEN BEGIN
		IF N_ELEMENTS(DATA) EQ 1 THEN DATA = DATA[0] ELSE	$
			DATA = REFORM(DATA,ROW2-ROW1+1,/OVERWRITE)
	ENDIF
;
;  If the parameters TZERO and TSCAL are non-trivial, then adjust the array by
;  these values.
;
	IF NOT KEYWORD_SET(NOSCALE) AND NOT KEYWORD_SET(NOIEEE) THEN BEGIN
		BZERO  = TZERO[ICOL,ILUN]
		BSCALE = TSCAL[ICOL,ILUN]
		IF (BSCALE NE 0) AND (BSCALE NE 1) THEN DATA *= BSCALE
                IF BZERO NE 0 THEN DATA += BZERO
                IF N_ELEMENTS(DIMS) NE 1 THEN BEGIN
                    DDIMS = DIMS
                    IF (SIZE(DATA,/TNAME) EQ 'STRING') AND $
                      (PRODUCT(DIMS) GT N_ELEMENTS(DATA)) THEN $
                        DDIMS = DIMS[1:*]
                    IF N_ELEMENTS(DDIMS) NE 1 THEN $
                      DATA = REFORM(DATA, DDIMS, /OVERWRITE)
                ENDIF
	ENDIF
;
;  Store NANVALUE everywhere where the data corresponded to IEE NaN.
;
	IF COUNT GT 0 THEN DATA[W] = NANVALUE
;
        IF N_ELEMENTS(ERRMSG) NE 0 THEN ERRMSG = ''
	RETURN
;
;  I/O error handling point.
;
HANDLE_IO_ERROR:
	MESSAGE = 'I/O error reading file'
;
;  Error handling point.
;
HANDLE_ERROR:
	IF N_ELEMENTS(ERRMSG) NE 0 THEN ERRMSG = MESSAGE ELSE MESSAGE, MESSAGE
	RETURN
	END
