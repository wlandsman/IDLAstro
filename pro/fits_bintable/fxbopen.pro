	PRO FXBOPEN, UNIT, FILENAME0, EXTENSION, HEADER, NO_TDIM=NO_TDIM, $
                ERRMSG=ERRMSG, ACCESS=ACCESS, REOPEN=REOPEN
;+
; NAME: 
;	FXBOPEN
; Purpose     : 
;	Open binary table extension in a disk FITS file for reading or updating
; Explanation : 
;	Opens a binary table extension in a disk FITS file for reading.  The
;	columns are then read using FXBREAD, and the file is closed when done
;	with FXBCLOSE.
; Use         : 
;	FXBOPEN, UNIT, FILENAME, EXTENSION  [, HEADER ]
; Inputs      : 
;       FILENAME  = Name of FITS file to be opened.  Optional
;                   extension *number* may be specified, in either of
;                   the following formats (using the FTOOLS
;                   convention): FILENAME[EXT] or FILENAME+EXT, where
;                   EXT is 1 or higher.  Such an extension
;                   specification takes priority over EXTENSION.
;                
;	EXTENSION = Either the number of the FITS extension, starting with the
;		    first extension after the primary data unit being one; or a
;		    character string containing the value of EXTNAME to search
;		    for.
; Opt. Inputs : 
;	None.
; Outputs     : 
;	UNIT	  = Logical unit number of the opened file.
; Opt. Outputs: 
;	HEADER	  = String array containing the FITS binary table extension
;		    header.
; Keywords    : 
;	NO_TDIM	  = If set, then any TDIMn keywords found in the header are
;		    ignored.
;
;       ACCESS    = A scalar string describing access privileges as
;                   one of READ ('R') or UPDATE ('RW').
;                   DEFAULT: 'R'
;
;       REOPEN    = If set, UNIT must be an already-opened file unit.
;                   FXBOPEN will treat the file as a FITS file.
;
;	ERRMSG	  = If defined and passed, then any error messages will be
;		    returned to the user in this parameter rather than
;		    depending on the MESSAGE routine in IDL.  If no errors are
;		    encountered, then a null string is returned.  In order to
;		    use this feature, ERRMSG must be defined first, e.g.
;
;			ERRMSG = ''
;			FXBOPEN, ERRMSG=ERRMSG, ...
;			IF ERRMSG NE '' THEN ...
;
; Calls       : 
;	FXBFINDLUN, FXBPARSE, FXHREAD, FXPAR
; Common      : 
;	Uses common block FXBINTABLE--see "fxbintable.pro" for more
;	information.
; Restrictions: 
;	The file must be a valid FITS file.
; Side effects: 
;	None.
; Category    : 
;	Data Handling, I/O, FITS, Generic.
; Prev. Hist. : 
;	W. Thompson, Feb 1992, based on READFITS by J. Woffard and W. Landsman.
;	W. Thompson, Feb 1992, changed from function to procedure.
;	W. Thompson, June 1992, fixed up error handling.
; Written     : 
;	William Thompson, GSFC, February 1992.
; Modified    : 
;	Version 1, William Thompson, GSFC, 12 April 1993.
;		Incorporated into CDS library.
;	Version 2, William Thompson, GSFC, 27 May 1994
;		Added ERRMSG keyword.
;	Version 3, William Thompson, GSFC, 21 June 1994
;		Extended ERRMSG to call to FXBPARSE
;       Version 4, William Thompson, GSFC, 23 June 1994
;               Modified so that ERRMSG is not touched if not defined.
;       Version 4, 23 June 1994
;
; Added ACCESS, REOPEN keywords, and FXFILTER package, CM 1999 Feb 03
; Added FILENAME[EXT] and FILENAME+EXT extension parsing, CM 1999 Jun 28
; Some general tidying, CM 1999 Nov 18
;       Allow for possible 64bit integer number of bytes W. Landsman Nov 2007
;       Make Ndata a 64bit integer to deal with larger files, E. Hivon, Mar 2008
;       
;
;-
;
@fxbintable
	ON_ERROR, 2
;
;  Check the number of parameters.
;
	IF N_PARAMS() LT 3 THEN BEGIN
		MESSAGE = 'Syntax:  FXBOPEN, UNIT, FILENAME, EXTENSION  ' + $
			'[, HEADER ]'
		IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
			ERRMSG = MESSAGE
			RETURN
		END ELSE MESSAGE, MESSAGE
	ENDIF
;
;  Check the type of the EXTENSION parameter.
;
	IF N_ELEMENTS(EXTENSION) NE 1 THEN BEGIN
		MESSAGE = 'EXTENSION must be a scalar'
		IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
			ERRMSG = MESSAGE
			RETURN
		END ELSE MESSAGE, MESSAGE
	ENDIF
	SZ = SIZE(EXTENSION)
	ETYPE = SZ[SZ[0]+1]
	IF ETYPE EQ 8 THEN BEGIN
		MESSAGE = 'EXTENSION must not be a structure'
		IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
			ERRMSG = MESSAGE
			RETURN
		END ELSE MESSAGE, MESSAGE
	ENDIF
;
;  If EXTENSION is of type string, then search for the proper extension by
;  name.  Otherwise, search by number.
;
	IF ETYPE EQ 7 THEN BEGIN
		S_EXTENSION = STRTRIM(STRUPCASE(EXTENSION),2)
	END ELSE BEGIN
		I_EXTENSION = FIX(EXTENSION)
		IF I_EXTENSION LT 1 THEN BEGIN
			MESSAGE = 'EXTENSION must be greater than zero'
			IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
				ERRMSG = MESSAGE
				RETURN
			END ELSE MESSAGE, MESSAGE
		ENDIF
	ENDELSE
;
;  Check access parameter
        IF N_ELEMENTS(ACCESS) EQ 0 THEN ACCESS='R'
        SZ = SIZE(ACCESS)
        IF SZ[SZ[0]+1] NE 7 THEN GOTO, ACCERR
        IF STRUPCASE(ACCESS) NE 'R' AND STRUPCASE(ACCESS) NE 'RW' THEN BEGIN
            ACCERR:
            MESSAGE = "ACCESS must be either 'R' or 'RW'"
            IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
                ERRMSG = MESSAGE
                RETURN
            END ELSE MESSAGE, MESSAGE
        ENDIF
            
;
;  Establish the read/write state
;
	ST = 1                                    ; Read only
        IF STRUPCASE(ACCESS) EQ 'RW' THEN ST = 2  ; Read/write

;
;  Get a logical unit number, and open the file.
;
        FILENAME = FILENAME0
        IF NOT KEYWORD_SET(REOPEN) THEN BEGIN

            ;; Check for extension name at the end of a filename
            LEN = STRLEN(FILENAME0)
            NEWEXT = 0L
            BFILENAME = BYTE(FILENAME)
            B0 = (BYTE('0'))(0) & B9 = (BYTE('9'))(0) 
            I = LEN-1
            BB = BFILENAME[I]

            ;; First case:  FILENAME[5]
            IF LEN GE 4 AND STRING(BB) EQ ']' THEN BEGIN ;; Count backwards
                I = I - 1
                IF BFILENAME[I] GE B0 AND BFILENAME[I] LE B9 THEN BEGIN
                    WHILE I GT 0 AND $
                      BFILENAME[I] GE B0 AND BFILENAME[I] LT B9 DO I = I - 1
                    IF I GT 0 AND STRING(BFILENAME[I]) EQ '[' THEN BEGIN
                        NEWEXT = LONG(STRMID(FILENAME,I+1,10))
                        FLEN = I
                    ENDIF
                ENDIF
            ENDIF

            ;; Second case: FILENAME+5
            IF LEN GE 3 AND BB GE B0 AND BB LE B9 THEN BEGIN ;; Count backwards
                WHILE I GT 0 AND $
                  BFILENAME[I] GE B0 AND BFILENAME[I] LT B9 DO I = I - 1
                IF I GT 0 AND STRING(BFILENAME[I]) EQ '+' THEN BEGIN
                    NEWEXT = LONG(STRMID(FILENAME,I+1,10))
                    FLEN = I
                ENDIF
            ENDIF
            IF NEWEXT GT 0 THEN BEGIN
                FILENAME = STRMID(FILENAME, 0, FLEN)
                I_EXTENSION = NEWEXT
                ETYPE = 1
            ENDIF

            ;; Open the file
            IF ST EQ 1 THEN $
              OPENR, UNIT, FILENAME, /BLOCK, /GET_LUN, ERROR=ERROR $
            ELSE $
              OPENU, UNIT, FILENAME, /BLOCK, /GET_LUN, ERROR=ERROR
            IF ERROR NE 0 THEN GOTO, NO_SUCH_FILE
        ENDIF

;
;  Reopen the file if requested.  Essentially this means seeking to
;  the start, after some error checking.
;        
        IF KEYWORD_SET(REOPEN) THEN BEGIN
            SZ = SIZE(UNIT)
            IF N_ELEMENTS(UNIT) NE 1 OR SZ[SZ[0]+1] EQ 8 THEN BEGIN
                MESSAGE = 'UNIT must be a scalar numeric type'
                IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
                    ERRMSG = MESSAGE
                    RETURN
                END ELSE MESSAGE, MESSAGE
            ENDIF

;
;  Error checking on file unit
;
            UNIT = UNIT[0]
            FS = FSTAT(UNIT)
            IF (FS.OPEN NE 1) OR (FS.READ NE 1) $
              OR (ST EQ 2 AND FS.WRITE NE 1) THEN BEGIN
                MESSAGE = 'UNIT '+strtrim(unit,2)+' must be open for reading'
                IF ST EQ 2 THEN MESSAGE = MESSAGE + '/writing'
                IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
                    ERRMSG = MESSAGE
                    RETURN
                END ELSE MESSAGE, MESSAGE
            ENDIF

            ;; Seek to the start of the file
            POINT_LUN, UNIT, 0L
        ENDIF


;
;  Store the UNIT number in the common block, and leave space for the other
;  parameters.  Initialize the common block if need be.  ILUN is an index into
;  the arrays.
;
	ILUN = FXBFINDLUN(UNIT)
;
;  Mark the file as open for read or write.
;
	STATE[ILUN] = ST
;
;  Read the primary header.
;
	FXHREAD,UNIT,HEADER,STATUS
	IF STATUS NE 0 THEN BEGIN
		FREE_LUN,UNIT
		MESSAGE = 'Unable to read primary FITS header'
		IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
			ERRMSG = MESSAGE
			RETURN
		END ELSE MESSAGE, MESSAGE
	ENDIF
	I_EXT = 0
;
;  Make sure that the file does contain extensions.
;
	START = 0L
	IF NOT FXPAR(HEADER,'EXTEND', START=START) THEN BEGIN
		FREE_LUN, UNIT
		MESSAGE = 'File ' + FILENAME + ' does not contain extensions'
		IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
			ERRMSG = MESSAGE
			RETURN
		END ELSE MESSAGE, MESSAGE
	ENDIF
;
;  Get the number of bytes taken up by the data.
;
NEXT_EXT:
	BITPIX = FXPAR(HEADER,'BITPIX', START=START)
	NAXIS  = FXPAR(HEADER,'NAXIS',  START=START)
	GCOUNT = FXPAR(HEADER,'GCOUNT', START=START)
	IF GCOUNT EQ 0 THEN GCOUNT = 1
	PCOUNT = FXPAR(HEADER,'PCOUNT', START=START)
	IF NAXIS GT 0 THEN BEGIN 
		DIMS = FXPAR(HEADER,'NAXIS*')		;Read dimensions
		NDATA = long64(DIMS[0])
		IF NAXIS GT 1 THEN FOR I=2,NAXIS DO NDATA = NDATA*DIMS[I-1]
	ENDIF ELSE NDATA = 0
	NBYTES = LONG64(ABS(BITPIX) / 8) * GCOUNT * (PCOUNT + NDATA)
;
;  Read the next extension header in the file.
;
	NREC = (NBYTES + 2879) / 2880
	POINT_LUN, -UNIT, POINTLUN			;Current position
	MHEAD0 = POINTLUN + NREC*2880L
	POINT_LUN, UNIT, MHEAD0				;Next FITS extension
	FXHREAD,UNIT,HEADER,STATUS
	IF STATUS NE 0 THEN BEGIN
		FREE_LUN,UNIT
		MESSAGE = 'Requested extension not found'
		IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
			ERRMSG = MESSAGE
			RETURN
		END ELSE MESSAGE, MESSAGE
	ENDIF
	I_EXT = I_EXT + 1
;
;  Check to see if the current extension is the one desired.
;
	START = 0L
	IF ETYPE EQ 7 THEN BEGIN
		EXTNAME = STRTRIM(STRUPCASE(FXPAR(HEADER,'EXTNAME', $
                                                  START=START)),2)
		IF EXTNAME EQ S_EXTENSION THEN GOTO, DONE
	END ELSE IF I_EXT EQ I_EXTENSION THEN GOTO, DONE
	GOTO, NEXT_EXT
;
;  Check to see if the extension type is BINTABLE or A3DTABLE.
;
DONE:
	XTENSION = STRTRIM(STRUPCASE(FXPAR(HEADER,'XTENSION', START=START)),2)
	IF (XTENSION NE 'BINTABLE') AND (XTENSION NE 'A3DTABLE') THEN BEGIN
		IF ETYPE EQ 7 THEN EXT = S_EXTENSION ELSE EXT = I_EXTENSION
		FREE_LUN,UNIT
		MESSAGE = 'Extension ' + STRTRIM(EXT,2) +		$
			' is not a binary table'
		IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
			ERRMSG = MESSAGE
			RETURN
		END ELSE MESSAGE, MESSAGE
	ENDIF
;
;  Get the rest of the information, and store it in the common block.
;
	MHEADER[ILUN] = MHEAD0
	FXBPARSE,ILUN,HEADER,NO_TDIM=NO_TDIM,ERRMSG=ERRMSG
	RETURN
;
;  Error point for not being able to open the file
;
NO_SUCH_FILE:
	MESSAGE = 'Unable to open file ' + STRTRIM(FILENAME,2)
	IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
		ERRMSG = MESSAGE
		RETURN
	END ELSE MESSAGE, MESSAGE
	END
