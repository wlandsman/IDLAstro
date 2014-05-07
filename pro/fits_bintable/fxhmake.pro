	PRO FXHMAKE, HEADER, DATA, EXTEND=EXTEND, DATE=DATE,	$
		INITIALIZE=INITIALIZE, ERRMSG=ERRMSG, XTENSION=XTENSION
;+
; NAME: 
;	FXHMAKE
; Purpose     : 
;	Create a basic FITS header array.
; Explanation : 
;	Creates a basic header array with all the required keywords.  This
;	defines a basic structure which can then be added to or modified by
;	other routines.
; Use         : 
;	FXHMAKE, HEADER  [, DATA ]
; Inputs      : 
;	None required.
; Opt. Inputs : 
;	DATA	= IDL data array to be written to file.    It must be in the 
;                  primary data unit unless the XTENSION keyword is supplied.
;		  This array is used to determine the values of the BITPIX and 
;                 NAXIS, etc. keywords.
;
;		  If not passed, then BITPIX is set to eight, NAXIS is set to
;		  zero, and no NAXISnnn keywords are included in this
;		  preliminary header.
; Outputs     : 
;	HEADER = String array containing FITS header.
; Opt. Outputs: 
;	None.
; Keywords    : 
;	INITIALIZE = If set, then the header is completely initialized, and any
;		     previous entries are lost.
;	EXTEND	= If set, then the keyword EXTEND is inserted into the file,
;		  with the value of "T" (true).
;	DATE	= If set, then the DATE keyword is added to the header.
;	ERRMSG	= If defined and passed, then any error messages will be
;		  returned to the user in this parameter rather than
;		  depending on the MESSAGE routine in IDL.  If no errors are
;		  encountered, then a null string is returned.  In order to
;		  use this feature, ERRMSG must be defined first, e.g.
;
;			ERRMSG = ''
;			FXHMAKE, ERRMSG=ERRMSG, ...
;			IF ERRMSG NE '' THEN ...
;       XTENSION - If set, then the header is appropriate for an image 
;                  extension, rather than the primary data unit.
; Calls       : 
;	GET_DATE, FXADDPAR, FXHCLEAN
; Common      : 
;	None.
; Restrictions: 
;	Groups are not currently supported.
; Side effects: 
;	BITPIX, NAXIS, etc. are defined such that complex arrays are stored as
;	floating point, with an extra first dimension of two elements (real and
;	imaginary parts).
; Category    : 
;	Data Handling, I/O, FITS, Generic.
; Prev. Hist. : 
;	William Thompson, Jan 1992, from SXHMAKE by D. Lindler and M. Greason.
;	Differences include:
;
;		* Use of FITS standard (negative BITPIX) to signal floating
;		  point numbers instead of (SDAS/Geis) DATATYPE keyword.
;		* Storage of complex numbers as pairs of real numbers.
;		* Support for EXTEND keyword, and for cases where there is no
;		  primary data array.
;		* Insertion of DATE record made optional.  Only required FITS
;		  keywords are inserted automatically.
; Written     : 
;	William Thompson, GSFC, January 1992.
; Modified    : 
;	Version 1, William Thompson, GSFC, 12 April 1993.
;		Incorporated into CDS library.
;	Version 2, William Thompson, GSFC, 21 June 1994
;		Added ERRMSG keyword.
;       Version 3, William Thompson, GSFC, 23 June 1994
;               Modified so that ERRMSG is not touched if not defined.
;	Version 4, Wayne Landsman, GSFC, 12 August 1997
;		Recognize double complex data type
;	Converted to IDL V5.0   W. Landsman   September 1997
;       Version 6, William Thompson, GSFC, 22 September 2004
;               Recognize unsigned integer types.
;       Version 6.1, C. Markwardt, GSFC, 19 Jun 2005
;               Add the XTENSION keyword, which writes an XTENSION
;               keyword instead of SIMPLE.
; Version     :
;       Version 6.1, 19 June 2005
;-
;
	ON_ERROR,2
;
;  Check the number of parameters first.
;
	IF N_PARAMS() LT 1 THEN BEGIN
		MESSAGE = 'Calling sequence:  FXHMAKE, HEADER  [, DATA ]'
		IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
			ERRMSG = MESSAGE
			RETURN
		END ELSE MESSAGE, MESSAGE
	ENDIF
;
;  If no data array was passed, then set BITPIX=8 and NAXIS=0.  Otherwise,
;  calculate these parameters.
;
	IF N_PARAMS() EQ 1 THEN BEGIN
		BITPIX = 8
		COMMENT = ''
		S = 0
	END ELSE BEGIN
		S = SIZE(DATA)			;obtain size of array.
		DTYPE = S[S[0]+1]		;type of data.
		CASE DTYPE OF
			0:  BEGIN
				MESSAGE = 'Data parameter is not defined'
				IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
					ERRMSG = MESSAGE
					RETURN
				END ELSE MESSAGE, MESSAGE
				END
			1:  BEGIN
				BITPIX = 8
				COMMENT = 'Integer*1 (byte)'
				END
			2:  BEGIN
				BITPIX = 16
				COMMENT = 'Integer*2 (short integer)'
				END
			3:  BEGIN
				BITPIX = 32
				COMMENT = 'Integer*4 (long integer)'
				END
			4:  BEGIN
				BITPIX = -32
				COMMENT = 'Real*4 (floating point)'
				END
			5:  BEGIN
				BITPIX = -64
				COMMENT = 'Real*8 (double precision)'
				END
			6:  BEGIN		;Complex*8 (complex)
				BITPIX = -32			;Store as float
				S = [S[0]+1, 2, S[1:*]]		;with extra dim
				COMMENT = 'Real*4 (complex, stored as float)'
				END
			7:  BEGIN
				MESSAGE = "Can't write strings to FITS files"
				IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
					ERRMSG = MESSAGE
					RETURN
				END ELSE MESSAGE, MESSAGE
				END
			8:  BEGIN
				MESSAGE = "Can't write structures to FITS files"
				IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
					ERRMSG = MESSAGE
					RETURN
				END ELSE MESSAGE, MESSAGE
				END
			9:  BEGIN
				BITPIX = -64			;Store as double
				S = [S[0]+1, 2, S[1:*]]		;with extra dim
				COMMENT = 'Real*8 (dcomplex, stored as double)'
				END
;
;  Unsigned data types may require use of BZERO/BSCALE--handled in writer.
;
			12: BEGIN       ;Unsigned integer
				BITPIX = 16
				COMMENT = 'Integer*2 (short integer)'
				END
			13:  BEGIN      ;Unsigned long integer
				BITPIX = 32
				COMMENT = 'Integer*4 (long integer)'
				END
                                
		ENDCASE
	ENDELSE
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
			IF ERRMSG NE '' THEN RETURN
		END ELSE FXHCLEAN,HEADER
	ENDELSE
;
;  The first keyword must be "SIMPLE".  Normally, this has the value "T"
;  (true).
;
        IF KEYWORD_SET(XTENSION) THEN BEGIN
            FXADDPAR,HEADER,'XTENSION','IMAGE','Written by IDL:  '+ SYSTIME()
        ENDIF ELSE BEGIN
            FXADDPAR,HEADER,'SIMPLE','T','Written by IDL:  '+ SYSTIME()
        ENDELSE
;
;  The second keyword must be "BITPIX", and the third "NAXIS".
;
	FXADDPAR,HEADER,'BITPIX',BITPIX,COMMENT
	FXADDPAR,HEADER,'NAXIS',S[0]	;# of dimensions
;
;  If NAXIS is not zero, then add the keywords for the axes.  If the data array
;  is complex, then add a comment to the first axis to note that this is
;  actually the real and imaginary parts of the complex number.
;
	IF S[0] NE 0 THEN FOR I=1,S[0] DO BEGIN
		IF (I EQ 1) AND (DTYPE EQ 6) THEN BEGIN
			FXADDPAR,HEADER,'NAXIS1',S[I],	$
				'Real and imaginary parts'
		END ELSE BEGIN
			FXADDPAR,HEADER,'NAXIS'+STRTRIM(I,2),S[I]
		ENDELSE
	ENDFOR
;
;  If requested, add the EXTEND keyword to the header, and set it to true.
;
	IF KEYWORD_SET(EXTEND) THEN	$
		FXADDPAR,HEADER,'EXTEND','T','File contains extensions'
;
;  If requested, add the DATE keyword to the header, containing the current
;  date.
;
	IF KEYWORD_SET(DATE) THEN BEGIN
	        GET_DATE,DTE                    ;Get current date as CCYY-MM-DD
        	FXADDPAR,HEADER,'DATE',DTE
	ENDIF
;
	IF N_ELEMENTS(ERRMSG) NE 0 THEN ERRMSG = ''
	RETURN
	END
