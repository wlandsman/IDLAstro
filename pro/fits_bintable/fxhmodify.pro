PRO FXHMODIFY, FILENAME, NAME, VALUE, COMMENT, BEFORE=BEFORE,   $
               AFTER=AFTER, FORMAT=FORMAT, EXTENSION=EXTENSION, ERRMSG=ERRMSG,$
               NOGROW=NOGROW
;+
; NAME: 
;       FXHMODIFY
; PURPOSE     : 
;       Modify a FITS header in a file on disk.
; Explanation : 
;       Opens a FITS file, and adds or modifies a parameter in the FITS header.
;       Can be used for either the main header, or for an extension header. 
;       The modification is performed directly on the disk file.
; Use         : 
;       FXHMODIFY, FILENAME, NAME, VALUE, COMMENT
; Inputs      : 
;       FILENAME = String containing the name of the file to be read.
;
;       NAME    = Name of parameter, scalar string  If NAME is already in the 
;                 header the value and possibly comment fields are modified. 
;                 Otherwise a new record is added to the header.  If NAME is 
;                 equal to either "COMMENT" or "HISTORY" then the value will be 
;                 added to the record without replacement.  In this case the 
;                 comment parameter is ignored.
;
;       VALUE   = Value for parameter.  The value expression must be of the
;                 correct type, e.g. integer, floating or string.  String
;                 values of 'T' or 'F' are considered logical values.
;
; Opt. Inputs : 
;       COMMENT = String field.  The '/' is added by this routine.  Added
;                 starting in position 31.  If not supplied, or set equal to ''
;                 (the null string), then any previous comment field in the
;                 header for that keyword is retained (when found).
; Outputs     : 
;       None.
; Opt. Outputs: 
;       None.
; Keywords    : 
;       EXTENSION = Either the number of the FITS extension, starting with the
;                   first extension after the primary data unit being one; or a
;                   character string containing the value of EXTNAME to search
;                   for.  If not passed, then the primary FITS header is
;                   modified.           
;
;       BEFORE  = Keyword string name.  The parameter will be placed before the
;                 location of this keyword.  For example, if BEFORE='HISTORY'
;                 then the parameter will be placed before the first history
;                 location.  This applies only when adding a new keyword;
;                 keywords already in the header are kept in the same position.
;
;       AFTER   = Same as BEFORE, but the parameter will be placed after the
;                 location of this keyword.  This keyword takes precedence over
;                 BEFORE.
;
;       FORMAT  = Specifies FORTRAN-like format for parameter, e.g. "F7.3".  A
;                 scalar string should be used.  For complex numbers the format
;                 should be defined so that it can be applied separately to the
;                 real and imaginary parts.
;       ERRMSG  = If defined and passed, then any error messages will be
;                 returned to the user in this parameter rather than
;                 depending on the MESSAGE routine in IDL.  If no errors are
;                 encountered, then a null string is returned.  In order to
;                 use this feature, ERRMSG must be defined first, e.g.
;
;                       ERRMSG = ''
;                       FXHMODIFY, ERRMSG=ERRMSG, ...
;                       IF ERRMSG NE '' THEN ...
;
; Calls       : 
;       FXHREAD, FXPAR, FXADDPAR, BLKSHIFT
; Restrictions: 
;       This routine can not be used to modify any of the keywords that control
;       the structure of the FITS file, e.g. BITPIX, NAXIS, PCOUNT, etc.  Doing
;       so could corrupt the readability of the FITS file.
; Example:
;       Modify the name 'OBJECT' keyword in the primary FITS header of a FITS 
;       file 'spec98.ccd' to contain the value 'test domeflat'
;
;       IDL> fxhmodify, 'spec98.ccd', 'OBJECT', 'test domeflat'
;
; Side effects: 
;       If adding a record to the FITS header would increase the
;       number of 2880 byte records stored on disk, then the file is
;       enlarged before modification, unless the NOGROW keyword is passed.
;  
; Category    : 
;       Data Handling, I/O, FITS, Generic.
; Prev. Hist. : 
;       None.
; Written     : 
;       William Thompson, GSFC, 3 March 1994.
; Modified    : 
;       Version 1, William Thompson, GSFC, 3 March 1994.
;       Version 2, William Thompson, GSFC, 31 May 1994
;               Added ERRMSG keyword.
;       Version 3, William Thompson, GSFC, 23 June 1994
;               Modified so that ERRMSG is not touched if not defined.
;      Version 3.1 Wayne Landsman GSFC   17 March 2006
;               Fix problem in BLKSHIFT call if primary header  extended
;       Version 3.2 W. Landsman 14 November 204 
;               Allow for need for 64bit number of bytes
;; Version     :
;       Version 3.2, 14 Nov 2007
;-
;
        COMPILE_OPT IDL2
        ON_ERROR, 2
;
;  Check the number of parameters.
;
        IF N_PARAMS() LT 3 THEN BEGIN
                MESSAGE = $     ;Need at least 3 parameters
                    'Syntax:  FXHMODIFY, FILENAME, NAME, VALUE [, COMMENT ]'
                IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
                        ERRMSG = MESSAGE
                        RETURN
                END ELSE MESSAGE, MESSAGE
        ENDIF
;
;  If passed, check the type of the EXTENSION parameter.
;
        IF N_ELEMENTS(EXTENSION) GT 1 THEN BEGIN
                MESSAGE = 'EXTENSION must be a scalar'
                IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
                        ERRMSG = MESSAGE
                        RETURN
                END ELSE MESSAGE, MESSAGE
        END ELSE IF N_ELEMENTS(EXTENSION) EQ 1 THEN BEGIN
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
        ENDIF
;
;  Get the UNIT number, and open the file.
;
        OPENU, UNIT, FILENAME, /BLOCK, /GET_LUN
;
;  Read in the primary FITS header.
;
        FXHREAD,UNIT,HEADER,STATUS
        IF STATUS NE 0 THEN BEGIN
                FREE_LUN,UNIT
                MESSAGE = 'Unable to read FITS header'
                IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
                        ERRMSG = MESSAGE
                        RETURN
                END ELSE MESSAGE, MESSAGE
        ENDIF
        MHEAD0 = 0
        I_EXT = 0
;
;  If the EXTENSION parameter was passed, then look for the requested
;  extension.
;
        IF N_ELEMENTS(EXTENSION) EQ 1 THEN BEGIN
;
;  Make sure that the file does contain extensions.
;
                IF NOT FXPAR(HEADER,'EXTEND') THEN BEGIN
                        FREE_LUN, UNIT
                        MESSAGE = 'File ' + FILENAME +  $
                                ' does not contain extensions'
                        IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
                                ERRMSG = MESSAGE
                                RETURN
                        END ELSE MESSAGE, MESSAGE
                ENDIF
;
;  Get the number of bytes taken up by the data.
;
NEXT_EXT:
                BITPIX = FXPAR(HEADER,'BITPIX')
                NAXIS  = FXPAR(HEADER,'NAXIS')
                GCOUNT = FXPAR(HEADER,'GCOUNT')
                IF GCOUNT EQ 0 THEN GCOUNT = 1
                PCOUNT = FXPAR(HEADER,'PCOUNT')
                IF NAXIS GT 0 THEN BEGIN 
                        DIMS = FXPAR(HEADER,'NAXIS*')   ;Read dimensions
                        NDATA = DIMS[0]
                        IF NAXIS GT 1 THEN FOR I=2,NAXIS DO     $
                                NDATA = NDATA*DIMS[I-1]
                ENDIF ELSE NDATA = 0
                NBYTES = LONG64(ABS(BITPIX) / 8) * GCOUNT * (PCOUNT + NDATA)
;
;  Read the next extension header in the file.
;
                NREC = (NBYTES + 2879) / 2880
                POINT_LUN, -UNIT, POINTLUN              ;Current position
                MHEAD0 = POINTLUN + NREC*2880L
                POINT_LUN, UNIT, MHEAD0                 ;Next FITS extension
                FXHREAD,UNIT,HEADER,STATUS
                POINT_LUN, -UNIT, END_HEADER
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
                IF ETYPE EQ 7 THEN BEGIN
                        EXTNAME = STRTRIM(STRUPCASE(FXPAR(HEADER,'EXTNAME')),2)
                        IF EXTNAME EQ S_EXTENSION THEN GOTO, DONE
                END ELSE IF I_EXT EQ I_EXTENSION THEN GOTO, DONE
                GOTO, NEXT_EXT
DONE:
        ENDIF ELSE POINT_LUN, -UNIT, END_HEADER

;
;  Add or modify the keyword parameter in the header, keeping track of the
;  initial size of the header array.
;
        IEND = WHERE(STRMID(HEADER,0,8) EQ 'END     ')
        N_INITIAL = 1 + IEND[0]/36
        IF N_PARAMS() EQ 4 THEN BEGIN
                FXADDPAR, HEADER, NAME, VALUE , COMMENT, BEFORE=BEFORE, $
                        AFTER=AFTER, FORMAT=FORMAT
        END ELSE BEGIN
                FXADDPAR, HEADER, NAME, VALUE, BEFORE=BEFORE, AFTER=AFTER, $
                        FORMAT=FORMAT
        ENDELSE
;
;  If the length of the header has changed, then print an error message.
;
        IEND = WHERE(STRMID(HEADER,0,8) EQ 'END     ')
        N_FINAL = 1 + IEND[0]/36
        IF N_FINAL NE N_INITIAL THEN BEGIN
            IF KEYWORD_SET(NOGROW) THEN BEGIN
                MESSAGE, /CONTINUE, 'Adding parameter would increase ' + $
                        'header length, no action taken.'
            ENDIF ELSE BEGIN
                ;; Increase size of the file by inserting multiples of
                ;; 2880 bytes at the end of the current header.  Then
                ;; resume normal operations.
                BLKSHIFT, UNIT, END_HEADER, (N_FINAL-N_INITIAL)*36L*80L
                GOTO, WRITE_HEADER
            ENDELSE
;
;  Otherwise, rewind to the beginning of the header, and write the new header
;  over the old header.  Convert to byte and force into 80 character lines.
;
        ENDIF ELSE BEGIN
            WRITE_HEADER:
                BHDR = REPLICATE(32B, 80, 36*N_FINAL)
                FOR N = 0,IEND[0] DO BHDR[0,N] = BYTE(STRMID(HEADER[N],0,80))
                POINT_LUN, UNIT, MHEAD0
                WRITEU, UNIT, BHDR
        ENDELSE
;
;  Close the file and return.
;
        FREE_LUN, UNIT
        IF N_ELEMENTS(ERRMSG) NE 0 THEN ERRMSG = ''
        RETURN
        END
