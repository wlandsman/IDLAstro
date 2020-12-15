	PRO FXBWRITE, UNIT, DATA, COL, ROW, BIT=BIT, NANVALUE=NANVALUE,	$
		ERRMSG=ERRMSG
;+
; NAME: 
;	FXBWRITE
; Purpose     : 
;	Write a binary data array to a disk FITS binary table file.
; Explanation : 
;	Each call to FXBWRITE will write to the data file, which should already
;	have been created and opened by FXBCREATE.  One needs to call this
;	routine for every column and every row in the binary table.  FXBFINISH
;	will then close the file.
; Use         : 
;	FXBWRITE, UNIT, DATA, COL, ROW
; Inputs      : 
;	UNIT	= Logical unit number corresponding to the file containing the
;		  binary table.
;	DATA	= IDL data array to be written to the file.
;	COL	= Column in the binary table to place data in, starting from
;		  column one.
;	ROW	= Row in the binary table to place data in, starting from row
;		  one.
; Opt. Inputs : 
;	None.
; Outputs     : 
;	None.
; Opt. Outputs: 
;	None.
; Keywords    : 
;	BIT	= Number of bits in bit mask arrays (type "X").  Only used if
;		  the column is of variable size.
;	NANVALUE= Value signalling data dropout.  All points corresponding to
;		  this value are set to be IEEE NaN (not-a-number).  Ignored
;		  unless DATA is of type float, double-precision or complex.
;	ERRMSG	= If defined and passed, then any error messages will be
;		  returned to the user in this parameter rather than
;		  depending on the MESSAGE routine in IDL.  If no errors are
;		  encountered, then a null string is returned.  In order to
;		  use this feature, ERRMSG must be defined first, e.g.
;
;			ERRMSG = ''
;			FXBWRITE, ERRMSG=ERRMSG, ...
;			IF ERRMSG NE '' THEN ...
;
; Calls       : 
;	None.
; Common      : 
;	Uses common block FXBINTABLE--see "fxbintable.pro" for more
;	information.
; Restrictions: 
;	The binary table file must have been opened with FXBCREATE.
;
;	The data must be consistent with the column definition in the binary
;	table header.
;
;	The row number must be consistent with the number of rows stored in the
;	binary table header.
;
; Side effects: 
;	None.
; Category    : 
;	Data Handling, I/O, FITS, Generic.
; Prev. Hist. : 
;	W. Thompson, Jan 1992, based on WRITEFITS by J. Woffard and W. Landsman.
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
;	Version 5, Wayne Landsman, GSFC, 12 Aug 1997
;		Recognize IDL double complex data type
;	Version 6, Converted to IDL V5.0   W. Landsman   September 1997
;       Version 7, William Thompson, 18-May-2016, change POINTER to ULONG
; Version     :
;       Version 7, 18-May-2016
;-
;
@fxbintable
	ON_ERROR, 2
;
;  Check the number of parameters.
;
	IF N_PARAMS() LT 4 THEN BEGIN
		MESSAGE = 'Syntax:  FXBWRITE, UNIT, DATA, COL, ROW'
		IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
			ERRMSG = MESSAGE
			RETURN
		END ELSE MESSAGE, MESSAGE
	ENDIF
;
;  Find the logical unit number in the FXBINTABLE common block.
;
	ILUN = WHERE(LUN EQ UNIT,NLUN)
	ILUN = ILUN[0]
	IF NLUN EQ 0 THEN BEGIN
		MESSAGE,'Unit ' + STRTRIM(UNIT,2) +	$
			' not opened properly'
		IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
			ERRMSG = MESSAGE
			RETURN
		END ELSE MESSAGE, MESSAGE
	ENDIF
;
;  Check the row and column parameters against the header.
;
	IF (COL LT 1) OR (COL GT TFIELDS[ILUN]) THEN BEGIN
		MESSAGE = 'COL must be between 1 and ' +	$
			STRTRIM(TFIELDS[ILUN],2)
		IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
			ERRMSG = MESSAGE
			RETURN
		END ELSE MESSAGE, MESSAGE
	END ELSE IF (ROW LT 1) OR (ROW GT NAXIS2[ILUN]) THEN BEGIN
		MESSAGE = 'ROW must be between 1 and ' +	$
			STRTRIM(NAXIS2[ILUN],2)
		IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
			ERRMSG = MESSAGE
			RETURN
		END ELSE MESSAGE, MESSAGE
	ENDIF
;
;  Calculate the row and column parameters using IDL zero-based indexing.
;
	IROW = LONG(ROW) - 1
	ICOL = LONG(COL) - 1
;
;  Check the type of the data against that defined for this column.
;
	SZ = SIZE(DATA)
	TYPE = SZ[SZ[0]+1]
	IF TYPE NE IDLTYPE[ICOL,ILUN] THEN BEGIN
		CASE IDLTYPE[ICOL,ILUN] OF
			1: STYPE = 'byte'
			2: STYPE = 'short integer'
			3: STYPE = 'long integer'
			4: STYPE = 'floating point'
			5: STYPE = 'double precision'
			6: STYPE = 'complex'
			7: STYPE = 'string'
			9: STYPE = 'double complex'
		ENDCASE
		MESSAGE = 'Data type should be ' + STYPE
		IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
			ERRMSG = MESSAGE
			RETURN
		END ELSE MESSAGE, MESSAGE
	ENDIF
;
;  Check the number of elements, depending on whether or not the column
;  contains variable length arrays.
;
	IF MAXVAL[ICOL,ILUN] GT 0 THEN BEGIN
		IF N_ELEMENTS(DATA) GT MAXVAL[ICOL,ILUN] THEN BEGIN
			MESSAGE = 'Data array should have no more than ' + $
				STRTRIM(N_ELEM[ICOL,ILUN],2) + ' elements'
			IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
				ERRMSG = MESSAGE
				RETURN
			END ELSE MESSAGE, MESSAGE
		ENDIF
	END ELSE BEGIN
		IF N_ELEMENTS(DATA) NE N_ELEM[ICOL,ILUN] THEN BEGIN
			MESSAGE = 'Data array should have ' +	$
				STRTRIM(N_ELEM[ICOL,ILUN],2) + ' elements'
			IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
				ERRMSG = MESSAGE
				RETURN
			END ELSE MESSAGE, MESSAGE
		ENDIF
	ENDELSE
;
;  Find the position of the first byte of the data array in the file.
;
	OFFSET = NHEADER[ILUN] + NAXIS1[ILUN]*IROW + BYTOFF[ICOL,ILUN]
	POINT_LUN,UNIT,OFFSET
;
;  If a variable length array, then test to see if the array is of type
;  double-precision complex (M) or bit (X).
;
	IF MAXVAL[ICOL,ILUN] GT 0 THEN BEGIN
		N_ELEM0 = N_ELEMENTS(DATA)
		IF FORMAT[ICOL,ILUN] EQ "X" THEN BEGIN
			IF N_ELEMENTS(BIT) EQ 0 THEN BEGIN
				MESSAGE = 'Number of bits not defined'
				IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
					ERRMSG = MESSAGE
					RETURN
				END ELSE MESSAGE, MESSAGE
			END ELSE IF N_ELEMENTS(BIT) NE 1 THEN BEGIN
				MESSAGE = 'Number of bits must be a scalar'
				IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
					ERRMSG = MESSAGE
					RETURN
				END ELSE MESSAGE, MESSAGE
			END ELSE IF LONG((BIT+7)/8) NE N_ELEM0 THEN BEGIN
				MESSAGE = 'Number of bits does not match ' + $
					'array size'
				IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
					ERRMSG = MESSAGE
					RETURN
				END ELSE MESSAGE, MESSAGE
			ENDIF
			N_ELEM0 = BIT
		ENDIF
;
;  Write out the number of elements, and the pointer to the variable length
;  array.
;
		POINTER = ULONARR(2)
		POINTER[0] = N_ELEM0
		POINTER[1] = DHEAP[ILUN]
		SWAP_ENDIAN_INPLACE,POINTER,/SWAP_IF_LITTLE
		WRITEU,UNIT,POINTER
		POINT_LUN,UNIT,NHEADER[ILUN] + HEAP[ILUN] + DHEAP[ILUN]
;
;  Update the HEAP pointer.
;
		CASE TYPE OF
			1:  DDHEAP = N_ELEMENTS(DATA)		;Byte
			2:  DDHEAP = N_ELEMENTS(DATA) * 2	;Short integer
			3:  DDHEAP = N_ELEMENTS(DATA) * 4	;Long integer
			4:  DDHEAP = N_ELEMENTS(DATA) * 4	;Float
			5:  DDHEAP = N_ELEMENTS(DATA) * 8	;Double
			6:  DDHEAP = N_ELEMENTS(DATA) * 8	;Complex
			7:  DDHEAP = N_ELEMENTS(DATA)		;String
			9:  DDHEAP = N_ELEMENTS(DATA) * 16      ;Dble Complex
		ENDCASE
		DHEAP[ILUN] = DHEAP[ILUN] + DDHEAP
	ENDIF
;
;  If a byte array, then simply write out the data.
;
        IF TYPE EQ 1 THEN BEGIN
		WRITEU,UNIT,DATA
;
;  Otherwise, if a character string array, then write out the character strings
;  with the correct width, truncating or padding with blanks as necessary.
;  However, if a variable length string array, then simply write it out.
;
	END ELSE IF TYPE EQ 7 THEN BEGIN
		IF MAXVAL[ICOL,ILUN] GT 0 THEN BEGIN
			WRITEU,UNIT,DATA
		END ELSE BEGIN
			N_CHAR = N_DIMS[1,ICOL,ILUN]
			NEWDATA = REPLICATE(32B,N_CHAR,N_ELEMENTS(DATA))
			FOR I=0,N_ELEMENTS(DATA)-1 DO	$
				NEWDATA[0,I] = BYTE(STRMID(DATA[I],0,N_CHAR))
			WRITEU,UNIT,NEWDATA
		ENDELSE
;
;  Otherwise, if necessary, then byte-swap the data before writing it out.
;  Also, replace any values corresponding data dropout with IEEE NaN.
;
	END ELSE BEGIN
		IF (N_ELEMENTS(NANVALUE) EQ 1) AND (TYPE GE 4) AND	$
				((TYPE LE 6) OR (TYPE EQ 9)) THEN BEGIN
			W = WHERE(DATA EQ NANVALUE, COUNT)
			CASE TYPE OF
				4:  NAN = FLOAT(  REPLICATE('FF'XB,4),0,1)
				5:  NAN = DOUBLE( REPLICATE('FF'XB,8),0,1)
				6:  NAN = COMPLEX(REPLICATE('FF'XB,8),0,1)
				9:  NAN = DCOMPLEX(REPLICATE('FF'XB,16),0,1)
			ENDCASE
		END ELSE COUNT = 0
;
		NEWDATA = DATA
		SWAP_ENDIAN_INPLACE, NEWDATA, /SWAP_IF_LITTLE
		IF COUNT GT 0 THEN NEWDATA[W] = NAN
		WRITEU,UNIT,NEWDATA 
	ENDELSE
;
	IF N_ELEMENTS(ERRMSG) NE 0 THEN ERRMSG = ''
	RETURN
	END
