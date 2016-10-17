	PRO FXWRITE, FILENAME, HEADER, DATA, NANVALUE=NANVALUE,		$
		NOUPDATE=NOUPDATE, ERRMSG=ERRMSG, APPEND=APPEND
;+
; NAME: 
;	FXWRITE
; Purpose     : 
;	Write a disk FITS file.
; Explanation : 
;       Creates or appends to a disk FITS file and writes a FITS
;       header, and optionally an image data array.
; Use         : 
;	FXWRITE, FILENAME, HEADER [, DATA ]
; Inputs      : 
;	FILENAME = String containing the name of the file to be written.
;	HEADER	 = String array containing the header for the FITS file.
; Opt. Inputs : 
;	DATA	 = IDL data array to be written to the file.  If not passed,
;		   then it is assumed that extensions will be added to the
;		   file.
; Outputs     : 
;	None.
; Opt. Outputs: 
;	None.
; Keywords    : 
;	NANVALUE = Value signalling data dropout.  All points corresponding to
;		   this value are set to be IEEE NaN (not-a-number).  Ignored
;		   unless DATA is of type float, double-precision or complex.
;	NOUPDATE = If set, then the optional BSCALE and BZERO keywords in the
;		   HEADER array will not be changed.  The default is to reset
;		   these keywords to BSCALE=1, BZERO=0.
;       APPEND = If set, then an existing file will be appended to.
;                Appending to a non-existent file will create it.  If
;                a primary HDU already exists then it will be modified
;                to have EXTEND = T.
;	ERRMSG	 = If defined and passed, then any error messages will be
;		   returned to the user in this parameter rather than
;		   depending on the MESSAGE routine in IDL.  If no errors are
;		   encountered, then a null string is returned.  In order to
;		   use this feature, ERRMSG must be defined first, e.g.
;
;			ERRMSG = ''
;			FXWRITE, ERRMSG=ERRMSG, ...
;			IF ERRMSG NE '' THEN ...
;
; Calls       : 
;	CHECK_FITS, GET_DATE, FXADDPAR, FXPAR
; Common      : 
;	None.
; Restrictions: 
;	If DATA is passed, then HEADER must be consistent with it.  If no data
;	array is being written to the file, then HEADER must also be consistent
;	with that.  The routine FXHMAKE can be used to create a FITS header.
;
;	If found, then the optional keywords BSCALE and BZERO in the HEADER
;	array is changed so that BSCALE=1 and BZERO=0.  This is so that these
;	scaling parameters are not applied to the data a second time by another
;	routine.  Also, history records are added storing the original values
;	of these constants.  (Other values of BZERO are used for unsigned
;	integers.)
;
;	If the /NOUPDATE keyword is set, however, then the BSCALE and BZERO
;	keywords are not changed.  The user should then be aware that FITS
;	readers will apply these numbers to the data, even if the data is
;	already converted to floating point form.
;
;	Groups are not supported.
;
; Side effects: 
;	HEADER may be modified.  One way it may be modified is describe
;       above under NOUPDATE.  The first header card may also be
;       modified to conform to the FITS standard if it does not
;       already agree (i.e. use of either the SIMPLE or XTENSION
;       keyword depending on whether the image is the primary HDU or
;       not).
; Category    : 
;	Data Handling, I/O, FITS, Generic.
; Prev. Hist. : 
;	W. Thompson, Jan 1992, from WRITEFITS by J. Woffard and W. Landsman.
;	Differences include:
;
;		* Made DATA array optional, and HEADER array mandatory.
;		* Changed order of HEADER and DATA parameters.
;		* No attempt made to fix HEADER array.
;
;	W. Thompson, May 1992, changed open statement to force 2880 byte fixed
;			       length records (VMS).  The software here does not
;			       depend on this file configuration, but other
;			       FITS readers might.
;	W. Thompson, Aug 1992, added code to reset BSCALE and BZERO records,
;			       and added the NOUPDATE keyword.
; Written     : 
;	William Thompson, GSFC, January 1992.
; Modified    : 
;	Version 1, William Thompson, GSFC, 12 April 1993.
;		Incorporated into CDS library.
;	Version 2, William Thompson, GSFC, 31 May 1994
;		Added ERRMSG keyword.
;	Version 3, William Thompson, GSFC, 23 June 1994
;		Modified so that ERRMSG is not touched if not defined.
;	Version 4, William Thompson, GSFC, 12 August 1999
;		Catch error if unable to open file.
;       Version 4.1 Wayne Landsman, GSFC, 02 May 2000
;               Remove !ERR in call to CHECK_FITS, Use ARG_PRESENT()
;       Version 5, William Thompson, GSFC, 22 September 2004
;               Recognize unsigned integer types
;       Version 5.1 W. Landsman 14 November 2004 
;               Allow for need for 64bit number of bytes
;       Version 6, Craig Markwardt, GSFC, 30 May 2005
;               Ability to append to existing files
;       Version 7, W. Landsman GSFC, Mar 2014
;               Remove HOST_TO_IEEE, Use V6.0 notation
; Version     : 
;	Version 6, 30 May 2005
;-
;
	ON_ERROR, 2
;
;  Check the number of parameters.
;   
	IF N_PARAMS() LT 2 THEN BEGIN
	    MESSAGE = 'Syntax:  FXWRITE, FILENAME, HEADER  [, DATA ]'
	    GOTO, HANDLE_ERROR
	ENDIF
;
;  Check the header against the data being written to the file.  If the data
;  array is not passed, then NAXIS should be set to zero, and EXTEND should be
;  true.
;
	IF N_PARAMS() EQ 2 THEN BEGIN
	    IF (FXPAR(HEADER,'NAXIS') NE 0) THEN BEGIN
		MESSAGE = 'NAXIS should be zero for no primary data array'
		GOTO, HANDLE_ERROR
	    END ELSE IF (~FXPAR(HEADER,'EXTEND')) THEN BEGIN
		MESSAGE = 'EXTEND should be true for no primary data array'
		GOTO, HANDLE_ERROR
	    ENDIF
	END ELSE BEGIN
	    CHECK_FITS, DATA, HEADER, ERRMSG = MESSAGE
	    IF MESSAGE NE '' THEN GOTO, HANDLE_ERROR
	ENDELSE
;
;  Set the BSCALE and BZERO keywords to their default values.
;
        SZ = SIZE(DATA)
        TYPE = SZ[SZ[0]+1]
        IF N_PARAMS() EQ 3 THEN NEWDATA = DATA
	IF ~KEYWORD_SET(NOUPDATE) THEN BEGIN
	    BZERO  = FXPAR(HEADER,'BZERO')
	    BSCALE = FXPAR(HEADER,'BSCALE')
	    GET_DATE,DTE
	    IF (BSCALE NE 0) AND (BSCALE NE 1) THEN BEGIN
		FXADDPAR,HEADER,'BSCALE',1.
		FXADDPAR,HEADER,'HISTORY',DTE+' reset BSCALE, was '+ $
			STRTRIM(BSCALE,2)
            ENDIF
;
;  If an unsigned data type then redefine BZERO to allow all the data to be
;  stored in the file.
;
            BZERO0 = 0
            IF (TYPE EQ 12) && (~KEYWORD_SET(NOUPDATE)) THEN BEGIN
                BZERO0 = '8000'X
                NEWDATA = FIX(TEMPORARY(NEWDATA) - BZERO)
            ENDIF
            IF (TYPE EQ 13) && (~KEYWORD_SET(NOUPDATE)) THEN BEGIN
                BZERO0 = '80000000'X
                NEWDATA = LONG(TEMPORARY(NEWDATA) - BZERO)
            ENDIF
	    IF BZERO NE BZERO0 THEN BEGIN
		FXADDPAR,HEADER,'BZERO',BZERO0
		FXADDPAR,HEADER,'HISTORY',DTE+' reset BZERO, was '+ $
			STRTRIM(BZERO,2)
	    ENDIF
	ENDIF
;
;  Get the UNIT number, and open the file.
;
       	GET_LUN, UNIT      
       	OPENW, UNIT, FILENAME, 2880, /BLOCK, ERROR=ERR, APPEND=APPEND
        VERB = 'creating'
        IF KEYWORD_SET(APPEND) THEN VERB = 'appending to'
	IF ERR NE 0 THEN BEGIN
	    MESSAGE = 'Error '+VERB+' file '+FILENAME
	    GOTO, HANDLE_ERROR
        ENDIF

;
;  Special processing is required when we are appending to 
;  the file, to ensure that the FITS standards are met.
;  (i.e. primary HDU must have EXTEND = T, and the header
;  to be written must have XTENSION = 'IMAGE').
;  

        POINT_LUN, -UNIT, POS
        IF POS GT 0 THEN BEGIN
            ;; Release the file and call FXHMODIFY to edit the
            ;; header of the primary HDU.  It is required to have
            ;; EXTEND=T.  FXHMODIFY calls FXADDPAR, which
            ;; automatically places the EXTEND keyword in the
            ;; required position.
            FREE_LUN, UNIT
            FXHMODIFY, FILENAME, ERRMSG=MESSAGE, $ ; (EXTENSION=0 implied)
              'EXTEND', 'T', ' FITS dataset may contain extensions'
            IF MESSAGE NE '' THEN GOTO, HANDLE_ERROR
            
            ;; Re-open the file
            GET_LUN, UNIT      
            OPENW, UNIT, FILENAME, 2880, /BLOCK, ERROR=ERR, APPEND=APPEND
            IF ERR NE 0 THEN BEGIN
                MESSAGE = 'Error re-opening file '+FILENAME
                GOTO, HANDLE_ERROR
            ENDIF
            
            ;; Revise the header so that it begins with an
            ;; XTENSION keyword... if it doesn't already
            IF STRMID(HEADER[0], 0, 9) EQ 'SIMPLE  =' THEN BEGIN
                ;; Extra work to preserve the comment
                DUMMY = FXPAR(HEADER, 'SIMPLE', COMMENT=COMMENT) 
                FXADDPAR, DUMMYHEADER, 'XTENSION', 'IMAGE', COMMENT
                HEADER[0] = DUMMYHEADER[0]
            ENDIF

            ;; Find last NAXIS* keyword, since PCOUNT/GCOUNT follow them
            NAXIS = FXPAR(HEADER, 'NAXIS', COUNT=COUNT_NAXIS)
            IF NAXIS[0] GT 0 THEN PCOUNT_AFTER='NAXIS'+strtrim(NAXIS[0],2)
            ;; Required PCOUNT/GCOUNT keywords for following extensions
            FXADDPAR, HEADER, 'PCOUNT', 0, ' number of random group parameters', $
              AFTER=PCOUNT_AFTER
            FXADDPAR, HEADER, 'GCOUNT', 1, ' number of random groups', $
              AFTER='PCOUNT'
            
        ENDIF ELSE BEGIN
            ;; In the off chance that this header was used before to
            ;; write a header with XTENSION, make sure this *new* file
            ;; has SIMPLE = T
            
            IF STRMID(HEADER[0], 0, 9) EQ 'XTENSION=' THEN BEGIN
                ;; Extra work to preserve the comment
                DUMMY = FXPAR(HEADER, 'XTENSION', COMMENT=COMMENT) 
                FXADDPAR, DUMMYHEADER, 'SIMPLE', 'T', COMMENT
                HEADER[0] = DUMMYHEADER[0]
            ENDIF

        ENDELSE


;
;  Determine if an END line occurs, and add one if necessary
;
	ENDLINE = WHERE( STRMID(HEADER,0,8) EQ 'END     ', NEND)
	ENDLINE = ENDLINE[0]
	IF NEND EQ 0 THEN BEGIN
	    MESSAGE, 'WARNING - An END statement has been appended ' + $
		'to the FITS header', /INFORMATIONAL
	    HEADER = [HEADER, 'END' + STRING(REPLICATE(32B,77))]
	    ENDLINE = N_ELEMENTS(HEADER) - 1 
	ENDIF
	NMAX = ENDLINE + 1		;Number of 80 byte records
	NHEAD = FIX((NMAX+35)/36)	;Number of 2880 byte records
;
;  Convert to byte and force into 80 character lines
;
	BHDR = REPLICATE(32B, 80, 36*NHEAD)
	FOR N = 0,ENDLINE DO BHDR[0,N] = BYTE( STRMID(HEADER[N],0,80) )
	WRITEU, UNIT, BHDR
;
;  If passed, then write the data array.
;
	IF N_PARAMS() EQ 3 THEN BEGIN
;
;  If necessary, then byte-swap the data before writing it out.  Also, replace
;  any values corresponding data dropout with IEEE NaN.
;
	    IF (N_ELEMENTS(NANVALUE) EQ 1) && (TYPE GE 4) &&	$
		    (TYPE LE 6) THEN BEGIN
		W = WHERE(DATA EQ NANVALUE, COUNT)
		CASE TYPE OF
		    4:  NAN = FLOAT(  REPLICATE('FF'XB,4),0,1)
		    5:  NAN = DOUBLE( REPLICATE('FF'XB,8),0,1)
		    6:  NAN = COMPLEX(REPLICATE('FF'XB,8),0,1)
		    9:  NAN = DCOMPLEX(REPLICATE('FF'XB,16),0,1)
		ENDCASE
	    END ELSE COUNT = 0
;
	    SWAP_ENDIAN_INPLACE, NEWDATA, /SWAP_IF_LITTLE
	    IF COUNT GT 0 THEN NEWDATA[W] = NAN
;
	    WRITEU,UNIT,NEWDATA
;
;  If necessary, then pad out to an integral multiple of 2880 bytes.
;
	    BITPIX = FXPAR( HEADER, 'BITPIX' )
	    NBYTES = LONG64(N_ELEMENTS(DATA)) * (ABS(BITPIX) / 8 )
	    NPAD = NBYTES MOD 2880
	    IF NPAD NE 0 THEN BEGIN
		NPAD = 2880 - NPAD
		WRITEU,UNIT,BYTARR(NPAD)
	    ENDIF
	ENDIF
;
;  Close the file and return.
;
	FREE_LUN, UNIT
	IF ARG_PRESENT(ERRMSG)  THEN ERRMSG = ''
	RETURN
;
HANDLE_ERROR:
	IF N_ELEMENTS(UNIT) EQ 1 THEN FREE_LUN, UNIT
	IF ARG_PRESENT(ERRMSG) THEN ERRMSG = 'FXWRITE: ' + MESSAGE	$
		ELSE MESSAGE, MESSAGE
;
	END
