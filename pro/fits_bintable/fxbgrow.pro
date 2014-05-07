        PRO FXBGROW, UNIT, HEADER, NROWS, ERRMSG=ERRMSG, NOZERO=NOZERO, $
                     BUFFERSIZE=BUFFERSIZE0
;+
; NAME: 
;        FXBGROW
; PURPOSE     : 
;       Increase the number of rows in a binary table.
; EXPLANATION : 
;       Call FXBGROW to increase the size of an already-existing FITS
;       binary table.  The number of rows increases to NROWS; however
;       the table cannot shrink by this operation.  This procedure is
;       useful when a table with an unknown number of rows must be
;       created.  The caller would then call FXBCREATE to construct a
;       table of some base size, and follow with calls to FXBGROW to
;       lengthen the table as needed.  The extension being enlarged
;       need not be the last extension in the file.  If subsequent
;       extensions exist in the file, they will be shifted properly.
;
; CALLING SEQUENCE :
;       FXBGROW, UNIT, HEADER, NROWS[, ERRMSG= , NOZERO= , BUFFERSIZE= ]
;
; INPUT PARAMETERS :
;       UNIT     = Logical unit number of an already-opened file.
;       HEADER   = String array containing the FITS binary table extension
;                  header.  The header is modified in place.
;       NROWS    = New number of rows, always more than the previous
;                  number.
;
; OPTIONAL INPUT KEYWORDS:
;       NOZERO   = when set, FXBGROW will not zero-pad the new data if
;                  it doesn't have to.
;       ERRMSG    = If defined and passed, then any error messages will be
;                   returned to the user in this parameter rather than
;                   depending on the MESSAGE routine in IDL.  If no errors are
;                   encountered, then a null string is returned.  In order to
;                   use this feature, ERRMSG must be defined first, e.g.
;
;                       ERRMSG = ''
;                       FXBGROW, ERRMSG=ERRMSG, ...
;                       IF ERRMSG NE '' THEN ...
;       BUFFERSIZE = Size in bytes for intermediate data transfers
;                    (default 32768)
;
; Calls       : 
;       FXADDPAR, FXHREAD, BLKSHIFT
; Common      : 
;       Uses common block FXBINTABLE--see "fxbintable.pro" for more
;       information.
; Restrictions: 
;       The file must be open with write permission.
;
;       The binary table extension in question must already by written
;       to the file (using FXBCREATE).
;
;       A table can never shrink via this operation.
;
; SIDE EFFECTS: 
;       The FITS file will grow in size, and heap areas are
;       preserved by moving them to the end of the file.
;
;       The header is modified to reflect the new number of rows.
; CATEGORY    : 
;       Data Handling, I/O, FITS, Generic.
;       Initially written, C. Markwardt, GSFC, Nov 1998
;       Added ability to enlarge arbitrary extensions and tables with
;         variable sized rows, not just the last extension in a file,
;         CM, April 2000
;       Fix bug in the zeroing of the output file, C. Markwardt, April 2005
;
;-
;
@fxbintable
        ON_ERROR, 0
;
;  Check the number of parameters.
;
        IF N_PARAMS() NE 3 THEN BEGIN
                MESSAGE = 'Syntax:  FXBGROW, UNIT, HEADER, NROWS'
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
                MESSAGE = 'Unit ' + STRTRIM(UNIT,2) +   $
                        ' not opened properly'
                IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
                        ERRMSG = MESSAGE
                        RETURN
                END ELSE MESSAGE, MESSAGE
        ENDIF
;
;  Don't shrink the file.
;
        IF NAXIS2[ILUN] GE NROWS THEN GOTO, FINISH
;
;  Make sure the file was opened for write access.
;
        IF STATE[ILUN] NE 2 THEN BEGIN
                MESSAGE = 'Unit ' + STRTRIM(UNIT,2) +   $
                        ' not opened for write access'
                IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
                        ERRMSG = MESSAGE
                        RETURN
                END ELSE MESSAGE, MESSAGE
        ENDIF
;
;  Compute number of bytes and buffer size
;

        NBYTES = (NROWS-NAXIS2[ILUN])*NAXIS1[ILUN]
        IF N_ELEMENTS(BUFFERSIZE0) EQ 0 THEN BUFFERSIZE0 = 32768L
        BUFFERSIZE = LONG(BUFFERSIZE0[0])
        BUFFERSIZE = FLOOR(BUFFERSIZE/NAXIS1[ILUN])*NAXIS1[ILUN]
        IF BUFFERSIZE LE 0 THEN BUFFERSIZE = NAXIS1[ILUN]

;
;  First, shift the following extensions by block multiples
;
        ;; Current beginning of next extension
        N_EXT  = NHEADER[ILUN] + HEAP[ILUN] + DHEAP[ILUN] 
        ;; New beginning of next extension, after shifting
        N_EXT1 = N_EXT + NBYTES
        ;; Round to nearest block size
        IF N_EXT  MOD 2880 NE 0 THEN N_EXT  = N_EXT  + 2880 - (N_EXT  MOD 2880)
        IF N_EXT1 MOD 2880 NE 0 THEN N_EXT1 = N_EXT1 + 2880 - (N_EXT1 MOD 2880)
        NBYTES1 = N_EXT1 - N_EXT

        ERRMSG1 = ''
        IF NBYTES1 GT 0 THEN BEGIN
            BLKSHIFT, UNIT, N_EXT, NBYTES1, ERRMSG=ERRMSG1, $
              NOZERO=KEYWORD_SET(NOZERO), BUFFERSIZE=BUFFERSIZE
            IF ERRMSG1 NE '' THEN GOTO, RETMESSAGE
        ENDIF
;
;  Next, shift the data between the end of the table and the next
;  extension, if any.
;
        ;; End of table data (but before variable-sized heap data)
        ETAB = NHEADER[ILUN] + NAXIS1[ILUN]*NAXIS2[ILUN]
        IF N_EXT GT ETAB THEN BEGIN
            BLKSHIFT, UNIT, [ETAB, N_EXT1-NBYTES-1L], NBYTES, ERRMSG=ERRMSG1, $
              NOZERO=KEYWORD_SET(NOZERO), BUFFERSIZE=BUFFERSIZE
        ENDIF

        RETMESSAGE:
        IF ERRMSG1 NE '' THEN BEGIN
            MESSAGE = ERRMSG1
            IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
                ERRMSG = MESSAGE
                RETURN
            END ELSE MESSAGE, MESSAGE
        ENDIF


;
;  Zero-fill if necessary (if the original table had no trailing
;  extensions)
;

        FS = FSTAT(UNIT)
        
        IF FS.SIZE LT N_EXT1 AND NOT KEYWORD_SET(NOZERO) THEN BEGIN
            POINT_LUN, UNIT, ETAB
            NLEFT = N_EXT1 - ETAB
            NBUFF = BUFFERSIZE < NLEFT
            BB = BYTARR(NBUFF)

            WHILE NLEFT GT 0 DO BEGIN
                WRITEU, UNIT, BB
                NLEFT = NLEFT - N_ELEMENTS(BB)
                IF (NLEFT LT NBUFF) AND (NLEFT GT 0) THEN BB = BB[0:NLEFT-1]
            ENDWHILE
        ENDIF

;
;  Update the internal state.
;
        HEAP[ILUN] = HEAP[ILUN] + NBYTES
        NAXIS2[ILUN] = NROWS

;
;  Modify passed copy of header
;
        IF N_ELEMENTS(HEADER) GT 0 THEN BEGIN
            FXADDPAR, HEADER, 'NAXIS2', LONG(NROWS), 'Number of rows (grown)'
            THEAP = FXPAR(HEADER, 'THEAP', COUNT=COUNT)
            IF COUNT GT 0 THEN BEGIN
                THEAP = THEAP + NBYTES
                FXADDPAR, HEADER, 'THEAP', THEAP, 'Offset of heap'
            ENDIF
        ENDIF


;
;  Modify internal copy of HEADER
;
        XHEADER = HEAD[*,ILUN]
        FXADDPAR, XHEADER, 'NAXIS2', LONG(NROWS), 'Number of rows (grown)'
        THEAP = FXPAR(XHEADER, 'THEAP', COUNT=COUNT)
        IF COUNT GT 0 THEN BEGIN
            THEAP = THEAP + NBYTES
            FXADDPAR, XHEADER, 'THEAP', THEAP, 'Offset of heap'
        ENDIF
        HEAD[*,ILUN] = XHEADER

;
;  Modify disk copy of HEADER
;
        POINT_LUN, UNIT, MHEADER[ILUN]
        FXHREAD, UNIT, DHEADER, STATUS
        IF STATUS NE 0 THEN BEGIN
            MESSAGE = 'Could not load header from file'
            IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
                ERRMSG = MESSAGE
                RETURN
            END ELSE MESSAGE, MESSAGE
        ENDIF
        FXADDPAR, DHEADER, 'NAXIS2', LONG(NROWS), 'Number of rows (grown)'
        THEAP = FXPAR(DHEADER, 'THEAP', COUNT=COUNT)
        IF COUNT GT 0 THEN BEGIN
            THEAP = THEAP + NBYTES
            FXADDPAR, DHEADER, 'THEAP', THEAP, 'Offset of heap'
        ENDIF
        ;; Don't worry about the header increasing in size, since
        ;; every binary table has to have NAXIS2 already.
        SLEN = STRLEN(DHEADER[0])
        FULL = STRING(REPLICATE(32B, 80))
        ;; Pad with spaces
        IF SLEN LT 80 THEN DHEADER[0] = DHEADER[0] + STRMID(FULL,0,80-SLEN)
        BHDR = BYTE(DHEADER)
        BHDR = BHDR[0:79,*]
        POINT_LUN, UNIT, MHEADER[ILUN]
        WRITEU, UNIT, BHDR

FINISH:
        IF N_ELEMENTS(ERRMSG) NE 0 THEN ERRMSG = ''
        RETURN
        END
