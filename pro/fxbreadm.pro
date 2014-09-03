;+
; NAME:
;       FXBREADM
; PURPOSE: 
;       Read multiple columns/rows from a disk FITS binary table file.
; EXPLANATION : 
;       A call to FXBREADM will read data from multiple rows and
;       multiple columns in a single procedure call.  Up to forty-nine
;       columns may be read in a single pass; the number of rows is
;       limited essentially by available memory.  The file should have
;       already been opened with FXBOPEN.  FXBREADM optimizes reading
;       multiple columns by first reading a large chunk of data from
;       the FITS file directly, and then slicing the data into columns
;       within memory.  FXBREADM can read variable-length arrays (see
;       below).
;
;       The number of columns is limited to 49 if data are passed by
;       positional argument.  However, this limitation can be overcome
;       by having FXBREADM return the data in an array of pointers.
;       The user should set the PASS_METHOD keyword to 'POINTER', and an 
;       array of pointers to the data will be returned in the POINTERS keyword.
;       The  user is responsible for freeing the pointers; however,
;       FXBREADM will reuse any pointers  passed into the procedure, and 
;       hence any pointed-to data will be destroyed.
;
;       FXBREADM can also read variable-length columns from FITS
;       binary tables.  Since such data is not of a fixed size, it is
;       returned as a structure.  The structure has the following
;       elements:
;
;              VARICOL:    ;; Flag: variable length column (= 1)
;              N_ELEMENTS: ;; Total number of elements returned
;              TYPE:       ;; IDL data type code (integer)
;              N_ROWS:     ;; Number of rows read from table (integer)
;              INDICES:    ;; Indices of each row's data (integer array)
;              DATA:       ;; Raw data elements (variable type array)
;
;       In order to gain access to the Ith row's data, one should
;       examine DATA(INDICES(I):INDICES(I+1)-1), which is similar in
;       construct to the REVERSE_INDICES keyword of the HISTOGRAM
;       function.
;
; CALLING SEQUENCE: 
;       FXBREADM, UNIT, COL, DATA1, [ DATA2, ... DATA48, ROW=, BUFFERSIZE = ]
;           /NOIEEE, /NOSCALE, /VIRTUAL, NANVALUE=, PASS_METHOD = POINTERS=, 
;           ERRMSG = , WARNMSG = , STATUS = , /DEFAULT_FLOAT]
;
; INPUT PARAMETERS : 
;       UNIT    = Logical unit number corresponding to the file containing the
;                 binary table.
;       COL     = An array of columns in the binary table to read data
;                 from, either as character strings containing column
;                 labels (TTYPE), or as numerical column indices
;                 starting from column one.
; Outputs     : 
;       DATA1, DATA2...DATA48 = A named variable to accept the data values, one
;                 for each column.  The columns are stored in order of the
;                 list in COL.  If the read operation fails for a
;                 particular column, then the corresponding output Dn
;                 variable is not altered.  See the STATUS keyword.
;                 Ignored if PASS_METHOD is 'POINTER'.
;
; OPTIONAL INPUT KEYWORDS: 
;       ROW     = Either row number in the binary table to read data from,
;                 starting from row one, or a two element array containing a
;                 range of row numbers to read.  If not passed, then the entire
;                 column is read in.
;       /DEFAULT_FLOAT = If set, then scaling with TSCAL/TZERO is done with
;                 floating point rather than double precision.
;       /NOIEEE = If set, then then IEEE floating point data will not
;                be converted to the host floating point format (and
;                this by definition implies NOSCALE).  The user is
;                responsible for their own floating point conversion.
;       /NOSCALE = If set, then the output data will not be scaled using the
;                 optional TSCAL and TZERO keywords in the FITS header.
;                 Default is to scale.
;       VIRTUAL = If set, and COL is passed as a name rather than a number,
;                 then if the program can't find a column with that name, it
;                 will then look for a keyword with that name in the header.
;                 Such a keyword would then act as a "virtual column", with the
;                 same value for every row.
;       DIMENSIONS = FXBREADM ignores this keyword.  It is here for
;	          compatibility only.
;       NANVALUE= Value signalling data dropout.  All points corresponding to
;                 IEEE NaN (not-a-number) are converted to this number.
;                 Ignored unless DATA is of type float, double-precision or
;                 complex.
;       PASS_METHOD = A scalar string indicating method of passing
;                 data from FXBREADM.  Either 'ARGUMENT' (indicating
;                 pass by positional argument), or 'POINTER' (indicating
;                 passing an array of pointers by the POINTERS
;                 keyword).
;                 Default: 'ARGUMENT'
;       POINTERS = If PASS_METHOD is 'POINTER' then an array of IDL
;                 pointers is returned in this keyword, one for each
;                 requested column.    Any pointers passed into FXBREADM will 
;                 have their pointed-to data destroyed.  Ultimately the
;                 user is responsible for deallocating pointers. 
;       BUFFERSIZE = Raw data are transferred from the file in chunks
;                 to conserve memory.  This is the size in bytes of
;                 each chunk.  If a value of zero is given, then all
;                 of the data are transferred in one pass.  Default is
;                 32768 (32 kB).
; OPTIONAL OUTPUT KEYWORDS:
;       ERRMSG  = If defined and passed, then any error messages will be
;                 returned to the user in this parameter rather than
;                 depending on the MESSAGE routine in IDL.  If no errors are
;                 encountered, then a null string is returned.  In order to
;                 use this feature, ERRMSG must be defined first, e.g.
;
;                       ERRMSG = ''
;                       FXBREAD, ERRMSG=ERRMSG, ...
;                       IF ERRMSG NE '' THEN ...
;       WARNMSG = Messages which are considered to be non-fatal
;                 "warnings" are returned in this output string.
;                 Note that if some but not all columns are
;                 unreadable, this is considered to be non-fatal.
;       STATUS  = An output array containing the status for each
;                 column read, 1 meaning success and 0 meaning failure.
;
; Calls       : 
;       FXPAR(), WHERENAN()
; Common      : 
;       Uses common block FXBINTABLE--see "fxbintable.pro" for more
;       information.
; Restrictions: 
;       The binary table file must have been opened with FXBOPEN.
;
;       The data must be consistent with the column definition in the binary
;       table header.
;
;       The row number must be consistent with the number of rows stored in the
;       binary table header.
;
;       Generally speaking, FXBREADM will be faster than iterative
;       calls to FXBREAD when (a) a large number of columns is to be
;       read or (b) the size in bytes of each cell is small, so that
;       the overhead of the FOR loop in FXBREAD becomes significant.
;
; SIDE EFFECTS: 
;       If there are no elements to read in (the number of elements is zero),
;       then the program sets !ERR to -1, and DATA is unmodified.
;
; Category    : 
;       Data Handling, I/O, FITS, Generic.
; Prev. Hist. : 
;       C. Markwardt, based in concept on FXBREAD version 12 from
;                              IDLASTRO, but with significant and
;                              major changes to accommodate the
;                              multiple row/column technique.  Mostly
;                              the parameter checking and general data
;                              flow remain.
;       C. Markwardt, updated to read variable length arrays, and to
;                              pass columns by handle or pointer.
;                              20 Jun 2001
;       C. Markwardt, try to conserve memory when creating the arrays
;                              13 Oct 2001
;   Handle case of GE 50 columns, C. Markwardt, 18 Apr 2002
;   Handle case where TSCAL/TZERO changes type of column,
;       C. Markwardt, 23 Feb 2003
;   Fix bug in handling of FOUND and numeric columns, 
;       C. Markwardt 12 May 2003
;   Removed pre-V5.0 HANDLE options  W. Landsman July 2004
;   Fix bug when HANDLE options were removed, July 2004
;   Handle special cases of TSCAL/TZERO which emulate unsigned
;      integers, Oct 2003
;   Add DEFAULT_FLOAT keyword to select float values instead of double
;      for TSCAL'ed, June 2004
;   Read 64bit integer columns, E. Hivon, Mar 2008
;   Add support for columns with TNULLn keywords, C. Markwardt, Apr 2010
;   Add support for files larger than 2 GB, C. Markwardt, 2012-04-17
;   Use V6 notation, remove IEEE_TO_HOST  W. Landsman Mar 2014
;
;-
;


;; This is a utility routine which converts the data from raw bytes to
;; IDL variables.
PRO FXBREADM_CONV, BB, DD, CTYPE, PERROW, NROWS, $
                   NOIEEE=NOIEEE, NOSCALE=NOSCALE, VARICOL=VARICOL, $
                   NANVALUE=NANVALUE, TZERO=TZERO, TSCAL=TSCAL, $
                   TNULL_VALUE=TNULL, TNULL_FLAG=TNULLQ, $
                   DEFAULT_FLOAT=DF

  COMMON FXBREADM_CONV_COMMON, DTYPENAMES
  IF N_ELEMENTS(DTYPENAMES) EQ 0 THEN $
    DTYPENAMES = [ '__BAD', 'BYTE', 'FIX', 'LONG', $
                   'FLOAT', 'DOUBLE', 'COMPLEX', 'STRING', $
                   '__BAD', 'DCOMPLEX', '__BAD', '__BAD', '__BAD', '__BAD', 'LONG64' ]
  
  TYPENAME = DTYPENAMES[CTYPE]

  IF CTYPE EQ 7 THEN BEGIN
      DD = STRING(TEMPORARY(BB))
  ENDIF ELSE BEGIN
      DD = CALL_FUNCTION(TYPENAME, TEMPORARY(BB), 0, PERROW*NROWS)
  ENDELSE
  IF N_ELEMENTS(DD) EQ 1 THEN DD = [DD]
  DD = REFORM(DD, PERROW, NROWS, /OVERWRITE)

  ;; Now perform any type-specific conversions, etc.
  COUNT = 0L
  CASE 1 OF
      ;; Integer types
      (CTYPE EQ 2 || CTYPE EQ 3 || ctype eq 14): BEGIN
          IF ~KEYWORD_SET(NOIEEE) || KEYWORD_SET(VARICOL) THEN $
            SWAP_ENDIAN_INPLACE, DD, /SWAP_IF_LITTLE 
          ;; Check for TNULL values
          ;; We will convert to NAN values later (or if the user
          ;; requested a different value we will use that)
          IF KEYWORD_SET(TNULLQ) THEN BEGIN
              W = WHERE(DD EQ TNULL,COUNT)
              IF N_ELEMENTS(NANVALUE) EQ 0 THEN NANVALUE = !VALUES.D_NAN
          ENDIF
      END

      ;; Floating and complex types
      (CTYPE GE 4 || CTYPE LE 6 || CTYPE EQ 9): BEGIN
          IF ~KEYWORD_SET(NOIEEE) THEN BEGIN
              IF N_ELEMENTS(NANVALUE) GT 0 THEN W=WHERENAN(DD,COUNT)
              SWAP_ENDIAN_INPLACE, DD, /SWAP_IF_LITTLE
          ENDIF
      END

      ;; String types (CTYPE EQ 7) have already been converted
      ;; in the above CALL_FUNCTION.  No further conversion
      ;; is necessary here.
  ENDCASE

;
;  If the parameters TZERO and TSCAL are non-trivial, then adjust the array by
;  these values.
;
  IF ((~KEYWORD_SET(NOIEEE) && ~KEYWORD_SET(NOSCALE)) && $
      (~KEYWORD_SET(VARICOL)) && $
      (N_ELEMENTS(TZERO) EQ 1 && N_ELEMENTS(TSCAL) EQ 1)) THEN BEGIN

      IF KEYWORD_SET(DF) THEN BEGIN
          ;; Default to float
          TSCAL = FLOAT(TSCAL)
          TZERO = FLOAT(TZERO)
      ENDIF

      IF CTYPE EQ 2 AND TSCAL[0] EQ 1 AND TZERO[0] EQ 32768 THEN BEGIN
          ;; SPECIAL CASE: Unsigned 16-bit integer
          DD = UINT(DD) - UINT(32768)
      ENDIF ELSE IF CTYPE EQ 3 AND TSCAL[0] EQ 1 AND $
        TZERO[0] EQ 2147483648D THEN BEGIN
          ;; SPECIAL CASE: Unsigned 32-bit integer
          DD = ULONG(DD) - ULONG(2147483648)
      ENDIF ELSE BEGIN
          IF (TSCAL[0] NE 0) && (TSCAL[0] NE 1) THEN DD = TSCAL[0]*DD
          IF TZERO[0] NE 0 THEN DD = DD + TZERO[0]
      ENDELSE
  ENDIF

;
;  Store NANVALUE everywhere where the data corresponded to IEEE NaN.
;
  IF COUNT GT 0 && N_ELEMENTS(NANVALUE) GT 0 THEN DD[W] = NANVALUE
  
END

PRO FXBREADM, UNIT, COL, $
              D0,  D1,  D2,  D3,  D4,  D5,  D6,  D7,  D8,  D9, $
              D10, D11, D12, D13, D14, D15, D16, D17, D18, D19, $
              D20, D21, D22, D23, D24, D25, D26, D27, D28, D29, $
              D30, D31, D32, D33, D34, D35, D36, D37, D38, D39, $
              D40, D41, D42, D43, D44, D45, D46, D47, $
              ROW=ROW, VIRTUAL=VIR, DIMENSIONS=DIM, $
              NOSCALE=NOSCALE, NOIEEE=NOIEEE, DEFAULT_FLOAT=DEFAULT_FLOAT, $
              PASS_METHOD=PASS_METHOD, POINTERS=POINTERS, $
              NANVALUE=NANVALUE, BUFFERSIZE=BUFFERSIZE, $
              ERRMSG=ERRMSG, WARNMSG=WARNMSG, STATUS=OUTSTATUS

@fxbintable
        ON_ERROR, 2
;
;  Check the number of parameters.
;
        IF N_PARAMS() LT 2 THEN BEGIN
                MESSAGE = 'Syntax:  FXBREADM, UNIT, COL, D0, D1, ... [, ROW= ]'
                IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
                        ERRMSG = MESSAGE
                        RETURN
                END ELSE MESSAGE, MESSAGE
        ENDIF
        IF N_ELEMENTS(BUFFERSIZE) EQ 0 THEN BUFFERSIZE = 32768L

;
;  COL may be one of several descriptors:
;     * a list of column numbers, beginning with 1
;     * a list of column names
;
        MYCOL = [ COL ]    ; Make sure it is an array

        SC = SIZE(MYCOL)
        NUMCOLS = N_ELEMENTS(MYCOL)
        OUTSTATUS = LONARR(NUMCOLS)
        COLNAMES = 'D'+STRTRIM(LINDGEN(NUMCOLS),2)

;
;  Determine whether the data is to be extracted as pointers or arguments
;
        IF N_ELEMENTS(PASS_METHOD) EQ 0 THEN PASS_METHOD = 'ARGUMENT'
        PASS = STRUPCASE(STRTRIM(PASS_METHOD[0],2))
        IF PASS NE 'ARGUMENT' AND PASS NE 'POINTER' THEN BEGIN
            MESSAGE = 'ERROR: PASS_METHOD must be ARGUMENT or POINTER'
            IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
                ERRMSG = MESSAGE
                RETURN
            END ELSE MESSAGE, MESSAGE
        ENDIF

        NP = N_ELEMENTS(POINTERS)
        IF PASS EQ 'POINTER' THEN BEGIN
            IF NP EQ 0 THEN POINTERS = PTRARR(NUMCOLS, /ALLOCATE_HEAP)
            NP = N_ELEMENTS(POINTERS)
            SZ = SIZE(POINTERS)
            IF SZ[SZ[0]+1] NE 10 THEN BEGIN
                MESSAGE = 'ERROR: POINTERS must be an array of pointers'
                IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
                        ERRMSG = MESSAGE
                        RETURN
                END ELSE MESSAGE, MESSAGE
            ENDIF

;
;  Expand the pointer array if necessary
;
            IF NP LT NUMCOLS THEN $
              POINTERS = [POINTERS[*], PTRARR(NUMCOLS-NP, /ALLOCATE_HEAP)]
            NP = N_ELEMENTS(POINTERS)

;
;  Make sure there are no null pointers, which cannot be assigned to.
;
            WH = WHERE(PTR_VALID(POINTERS) EQ 0, CT)
            IF CT GT 0 THEN POINTERS[WH] = PTRARR(CT, /ALLOCATE_HEAP)
                
        ENDIF


;
;  Find the logical unit number in the FXBINTABLE common block.
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
;  Check the number of columns.  It should be fewer than 49
;
        IF PASS EQ 'ARGUMENT' THEN BEGIN
            IF NUMCOLS GT 49 THEN BEGIN
                MESSAGE = 'Maximum of 49 columns exceeded'
                IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
                    ERRMSG = MESSAGE
                    RETURN
                END ELSE MESSAGE, MESSAGE
            ENDIF
            IF N_PARAMS()-2 LT NUMCOLS AND N_ELEMENTS(ERRMSG) EQ 0 THEN BEGIN
                MESSAGE, 'WARNING: number of data parameters less than columns', $
                  /INFO
            ENDIF
        ENDIF
            
        ICOL    = LONARR(NUMCOLS)
        VIRTUAL = BYTARR(NUMCOLS)
        VIRTYPE = LONARR(NUMCOLS)
        FOUND   = BYTARR(NUMCOLS)
        VARICOL = BYTARR(NUMCOLS)
        NOTFOUND = ''
        NNOTFOUND = 0L
        IF N_ELEMENTS(WARNMSG) NE 0 THEN WARNMSG = ''

;
;  If COL is of type string, then search for a column with that label.
;
        IF SC[SC[0]+1] EQ 7 THEN BEGIN
            MYCOL = STRUPCASE(STRTRIM(MYCOL,2))
            FOR I = 0, NUMCOLS-1 DO BEGIN
                XCOL = WHERE(TTYPE[*,ILUN] EQ MYCOL[I], NCOL)
                ICOL[I] = XCOL[0]
;
;  If the column was not found, and VIRTUAL was set, then search for a keyword
;  by that name.
;
                IF NCOL GT 0 THEN FOUND[I] = 1
                IF NOT FOUND[I] AND KEYWORD_SET(VIR) THEN BEGIN
                    HEADER = HEAD[*,ILUN]
                    VALUE = FXPAR(HEADER,MYCOL[I], Count = N_VALUE)
                    IF N_VALUE GE 0 THEN BEGIN
                        RESULT = EXECUTE(COLNAMES[I]+' = VALUE')
                        SV = SIZE(VALUE)
                        VIRTYPE[I] = SV[SV[0]+1]
                        VIRTUAL[I] = 1
                        FOUND[I] = 1
                    ENDIF
                ENDIF ELSE IF ~FOUND[I] THEN BEGIN
                    IF NOTFOUND EQ '' THEN NOTFOUND = MYCOL[I] $
                    ELSE NOTFOUND = NOTFOUND +', ' + MYCOL[I]
                    NNOTFOUND++
                ENDIF

            ENDFOR

            IF NNOTFOUND EQ NUMCOLS THEN BEGIN
                MESSAGE = 'ERROR: None of the requested columns were found'
                IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
                    ERRMSG = MESSAGE
                    RETURN
                END ELSE MESSAGE, MESSAGE
            ENDIF ELSE IF NNOTFOUND GT 0 THEN BEGIN
                MESSAGE = 'WARNING: Columns ' + NOTFOUND + ' were not found'
                IF N_ELEMENTS(WARNMSG) NE 0 THEN WARNMSG = MESSAGE $
                ELSE MESSAGE, MESSAGE, /INFO
            ENDIF
                
;
;  Otherwise, a numerical column was passed.  Check its value.
;
        ENDIF ELSE BEGIN
            ICOL[*] = LONG(MYCOL) - 1
            FOUND[*] = 1
        ENDELSE

;  Step through each column index
        MESSAGE = ''
        FOR I = 0, NUMCOLS-1 DO BEGIN
            IF ~FOUND[I] THEN GOTO, LOOP_END_COLCHECK
            IF VIRTUAL[I] THEN GOTO, LOOP_END_COLCHECK

            IF (ICOL[I] LT 0) OR (ICOL[I] GE TFIELDS[ILUN]) THEN BEGIN
                MESSAGE = MESSAGE + '; COL "'+STRTRIM(MYCOL[I],2)+$
                  '" must be between 1 and ' +  $
                  STRTRIM(TFIELDS[ILUN],2)
                FOUND[I] = 0
            ENDIF
;
;  If there are no elements in the array, then set !ERR to -1.
;
            IF FOUND[I] AND N_ELEM[ICOL[I],ILUN] EQ 0 THEN BEGIN
                FOUND[I] = 0
                MESSAGE = MESSAGE + '; Number of elements to read in "'+$
                  STRTRIM(MYCOL[I],2)+'" is zero'
;                !ERR = -1
;                RETURN
            ENDIF

;
;  Flag variable-length columns
;
            IF MAXVAL[ICOL[I],ILUN] GT 0 THEN BEGIN
                FOUND[I] = 1
                VARICOL[I] = 1
            ENDIF

            LOOP_END_COLCHECK:

        ENDFOR

;
;  Check to be sure that there are columns to be read
;
        W  = WHERE(FOUND EQ 1, COUNT)
        WV = WHERE(FOUND EQ 1 OR VARICOL EQ 1, WVCOUNT)
        IF WVCOUNT EQ 0 THEN BEGIN
            STRPUT, MESSAGE, ':', 0
            MESSAGE = 'ERROR: No requested columns could be read'+MESSAGE
            IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
                ERRMSG = MESSAGE
                RETURN
            END ELSE MESSAGE, MESSAGE
        ENDIF ELSE IF MESSAGE NE '' THEN BEGIN
            STRPUT, MESSAGE, ':', 0
            MESSAGE = 'WARNING: Some columns could not be read'+MESSAGE
            IF N_ELEMENTS(WARNMSG) NE 0 THEN WARNMSG = MESSAGE $
            ELSE MESSAGE, MESSAGE, /INFO
        ENDIF
            
;
;  If ROW was not passed, then set it equal to the entire range.  Otherwise,
;  extract the range.
;
        IF N_ELEMENTS(ROW) EQ 0 THEN ROW = [1LL, NAXIS2[ILUN]]
        CASE N_ELEMENTS(ROW) OF
                1:  ROW2 = LONG64(ROW[0])
                2:  ROW2 = LONG64(ROW[1])
                ELSE:  BEGIN
                        MESSAGE = 'ROW must have one or two elements'
                        IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
                                ERRMSG = MESSAGE
                                RETURN
                        END ELSE MESSAGE, MESSAGE
                        END
        ENDCASE
        ROW1 = LONG64(ROW[0])
;
;  If ROW represents a range, then make sure that the row range is legal, and
;  that reading row ranges is allowed (i.e., the column is not variable length.
;
        IF ROW1 NE ROW2 THEN BEGIN
                MAXROW = NAXIS2[ILUN]
                IF (ROW1 LT 1) OR (ROW1 GT MAXROW) THEN BEGIN
                        MESSAGE = 'ROW[0] must be between 1 and ' +     $
                                STRTRIM(MAXROW,2)
                        IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
                                ERRMSG = MESSAGE
                                RETURN
                        END ELSE MESSAGE, MESSAGE
                END ELSE IF (ROW2 LT ROW1) OR (ROW2 GT MAXROW) THEN BEGIN
                        MESSAGE = 'ROW[1] must be between ' +   $
                                STRTRIM(ROW1,2) + ' and ' + STRTRIM(MAXROW,2)
                        IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
                                ERRMSG = MESSAGE
                                RETURN
                        END ELSE MESSAGE, MESSAGE
                ENDIF
;
;  Otherwise, if ROW is a single number, then just make sure it's valid.
;
        END ELSE BEGIN
                IF (ROW1 LT 1) OR (ROW1 GT NAXIS2[ILUN]) THEN BEGIN
                        MESSAGE = 'ROW must be between 1 and ' +        $
                                STRTRIM(NAXIS2[ILUN],2)
                        IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
                                ERRMSG = MESSAGE
                                RETURN
                        END ELSE MESSAGE, MESSAGE
                ENDIF
        ENDELSE

;
;  Compose information about the output
;
        HEADER = HEAD[*,ILUN]
        COLNDIM = LONARR(NUMCOLS)
        COLDIM  = LONARR(NUMCOLS, 20) ;; Maximum of 20 dimensions in output
        COLTYPE = LONARR(NUMCOLS)
        BOFF1   = LONARR(NUMCOLS)
        BOFF2   = LONARR(NUMCOLS)
        TNULL_FLG = INTARR(NUMCOLS) ;; 1 if TNULLn column is present
        TNULL_VAL = DBLARR(NUMCOLS) ;; value of TNULLn column if present
        NROWS = ROW2-ROW1+1
        FOR I = 0L, NUMCOLS-1 DO BEGIN

            IF ~FOUND[I] THEN GOTO, LOOP_END_DIMS
            ;;  Data type of the input.
            IF VIRTUAL[I] THEN BEGIN
                ; Virtual column: read from keyword itself
                COLTYPE[I] = VIRTYPE[I] 
                GOTO, LOOP_END_DIMS
            ENDIF ELSE IF VARICOL[I] THEN BEGIN
                ; Variable length column: 2-element long
                COLTYPE[I] = 3
                DIMS = [1L, 2L]
            ENDIF ELSE BEGIN
                COLTYPE[I] = IDLTYPE[ICOL[I],ILUN]
                DIMS = N_DIMS[*,ICOL[I],ILUN]
            ENDELSE
            
            NDIMS = DIMS[0]
            DIMS  = DIMS[1:NDIMS]

            IF NDIMS EQ 1 AND DIMS[0] EQ 1 THEN BEGIN

                ;; Case of only one output element, try to return a
                ;; scalar.  Otherwise, it is a vector equal to the
                ;; number of rows to be read

                COLNDIM[I] = 1L
                COLDIM[I,0] = NROWS
            ENDIF ELSE BEGIN

                COLNDIM[I] = NDIMS
                COLDIM[I,0:(NDIMS-1)] = DIMS
                IF NROWS GT 1 THEN BEGIN
                    COLDIM[I,NDIMS] = NROWS
                    COLNDIM[I]++
                ENDIF

            ENDELSE
            
            ;; For strings, the number of characters is the first
            ;; dimension.  This information is useless to us now,
            ;; since the STRING() type cast which will appear below
            ;; handles the array conversion automatically.
            IF COLTYPE[I] EQ 7 THEN BEGIN
                IF COLNDIM[I] GT 1 THEN BEGIN
                    COLDIM[I,0:COLNDIM[I]-2] = COLDIM[I,1:COLNDIM[I]-1]
                    COLDIM[I,COLNDIM[I]-1]   = 0
                    COLNDIM[I] = COLNDIM[I] - 1
                ENDIF ELSE BEGIN  ;; Case of a single row
                    COLNDIM[I] = 1L
                    COLDIM[I,0] = NROWS
                ENDELSE
            ENDIF

            ;; Byte offsets
            BOFF1[I] = BYTOFF[ICOL[I],ILUN]
            IF ICOL[I] EQ TFIELDS[ILUN]-1 THEN $
              BOFF2[I] = NAXIS1[ILUN]-1 $
            ELSE $
              BOFF2[I] = BYTOFF[ICOL[I]+1,ILUN]-1

            ;; TNULLn keywords for integer type columns
            IF (COLTYPE[I] GE 1 AND COLTYPE[I] LE 3) OR $
              (COLTYPE[I] GE 12 AND COLTYPE[I] LE 15) THEN BEGIN
                TNULLn = 'TNULL'+STRTRIM(ICOL[I]+1,2)
                VALUE = FXPAR(HEADER,TNULLn, Count = N_VALUE)
                IF N_VALUE GT 0 THEN BEGIN
                    TNULL_FLG[I] = 1
                    TNULL_VAL[I] = VALUE
                ENDIF
            ENDIF
            
            LOOP_END_DIMS:

        ENDFOR

;
;  Construct any virtual columns first
;
        WC = WHERE(FOUND EQ 1 AND VIRTUAL EQ 1, WCCOUNT)
        FOR I = 0L, WCCOUNT-1 DO BEGIN
            ;; If it's virtual, then the value only needs to be
            ;; replicated
            EXTCMD = COLNAMES[WC[I]]+'= REPLICATE(D'+COLNAMES[WC[I]]+',NROWS)'
            ;; Run the command that selects the data
            RESULT = EXECUTE(EXTCMD)
            IF RESULT EQ 0 THEN BEGIN
                MESSAGE = 'ERROR: Could not extract data (column '+$
                  STRTRIM(MYCOL[WC[I]],2)+')'
                IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
                    ERRMSG = MESSAGE
                    RETURN
                ENDIF ELSE MESSAGE, MESSAGE
            ENDIF
            OUTSTATUS[I] = 1
        ENDFOR


;  Skip to processing variable-length columns if all other columns are virtual
        WC = WHERE(FOUND EQ 1 AND VIRTUAL EQ 0, WCCOUNT)
        IF WCCOUNT EQ 0 THEN GOTO, PROC_CLEANUP

;  Create NANVALUES, the template to use when a NAN is found
        IF N_ELEMENTS(NANVALUE) GE NUMCOLS THEN BEGIN
            NANVALUES = NANVALUE[0:NUMCOLS-1]
        ENDIF ELSE IF N_ELEMENTS(NANVALUE) GT 0 THEN BEGIN
            NANVALUES = REPLICATE(NANVALUE[0], NUMCOLS)
            NANVALUES[0] = NANVALUE
            I = N_ELEMENTS(NANVALUE)
            IF I LT NUMCOLS THEN $
              NANVALUES[I:*] = NANVALUE[0]
        ENDIF

;
;  Find the position of the first byte of the data array in the file.
;
        OFFSET0 = NHEADER[ILUN] + NAXIS1[ILUN]*(ROW1-1LL)
        POS = 0LL
        NROWS0 = NROWS
        J = 0LL
        FIRST = 1
        ;; Here, we constrain the buffer to be at least 16 rows long.
        ;; If we fill up 32 kB with fewer than 16 rows, then there
        ;; must be a lot of (big) columns in this table.  It's
        ;; probably a candidate for using FXBREAD instead.
        BUFFROWS = LONG((BUFFERSIZE/NAXIS1[ILUN]) > 16L)
        IF BUFFERSIZE LE 0 THEN BUFFROWS = NROWS0

;
;  Loop through the data in chunks
;
        WHILE NROWS GT 0 DO BEGIN
        J++
        NR  = NROWS < BUFFROWS
        OFFSET1 = NAXIS1[ILUN]*POS

;
;  Proceed by reading a byte array from the input data file
;  FXBREADM reads all columns from the specified rows, and
;  sorts out the details of which bytes belong to which columns
;  in the next FOR loop.
;
        BB = BYTARR(NAXIS1[ILUN], NR)
        POINT_LUN, UNIT, OFFSET0+OFFSET1
        READU, UNIT, BB
;        FXGSEEK, UNIT, OFFSET0+OFFSET1
;        FXGREAD, UNIT, BB

;
;  Now select out the desired columns
;
        FOR I = 0, NUMCOLS-1 DO BEGIN
           
            ;; Extract the proper rows and columns
            IF ~FOUND[I] THEN GOTO, LOOP_END_STORE
            IF VIRTUAL[I]   THEN GOTO, LOOP_END_STORE

            ;; Extract the data from the byte array and convert it
            ;; The inner CALL_FUNCTION is to one of the coercion
            ;; functions, such as FIX(), DOUBLE(), STRING(), etc.,
            ;; which is called with an offset to force a conversion
            ;; from bytes to the data type.
            ;; The outer CALL_FUNCTION is to REFORM(), which makes
            ;; sure that the data structure is correct.
            ;;
            DIMS = COLDIM[I,0:COLNDIM[I]-1]
            PERROW = ROUND(PRODUCT(DIMS)/NROWS0)
            
            IF N_ELEMENTS(NANVALUES) GT 0 THEN $
              EXTRA={NANVALUE: NANVALUES[I]}

            FXBREADM_CONV, BB[BOFF1[I]:BOFF2[I], *], DD, COLTYPE[I], PERROW, NR,$
              NOIEEE=KEYWORD_SET(NOIEEE), NOSCALE=KEYWORD_SET(NOSCALE), $
              TZERO=TZERO[ICOL[I], ILUN], TSCAL=TSCAL[ICOL[I], ILUN], $
              VARICOL=VARICOL[I], DEFAULT_FLOAT=DEFAULT_FLOAT, $
              TNULL_VALUE=TNULL_VAL[I], TNULL_FLAG=TNULL_FLG[I], $
              _EXTRA=EXTRA

            ;; Initialize the output variable on the first chunk
            IF FIRST THEN BEGIN
                SZ = SIZE(DD)
                ;; NOTE: type could have changed if TSCAL/TZERO were used
                COLTYPEI = SZ(SZ[0]+1)  
                RESULT = EXECUTE(COLNAMES[I]+' = 0')
                RESULT = EXECUTE(COLNAMES[I]+' = '+$
                                 'MAKE_ARRAY(PERROW, NROWS0, TYPE=COLTYPEI)')
                RESULT = EXECUTE(COLNAMES[I]+' = '+$
                         'REFORM('+COLNAMES[I]+', PERROW, NROWS0,/OVERWRITE)')
            ENDIF

            ;; Finally, store this in the output variable
            RESULT = EXECUTE(COLNAMES[I]+'[0,POS] = DD')
            DD = 0
            IF RESULT EQ 0 THEN BEGIN
                MESSAGE = 'ERROR: Could not compose output data '+COLNAMES[I]
                IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
                    ERRMSG = MESSAGE
                    RETURN
                ENDIF ELSE MESSAGE, MESSAGE
            ENDIF

            OUTSTATUS[I] = 1

            LOOP_END_STORE:
        ENDFOR

        FIRST = 0
        NROWS = NROWS - NR
        POS   = POS + NR
        ENDWHILE

;
;  Read the variable-length columns from the heap.  Adjacent data are
;  coalesced into one read operation.  Note: this technique is thus
;  optimal for extensions with only one variable-length column.  If
;  there are more than one then coalescence will not occur.
;

        ;; Width of the various data types in bytes
        WIDARR = [0L, 1L, 2L, 4L, 4L, 8L, 8L, 1L, 0L,16L, 0L]
        WV = WHERE(OUTSTATUS EQ 1 AND VARICOL EQ 1, WVCOUNT)
        FOR J = 0, WVCOUNT-1 DO BEGIN
            I = WV[J]
            RESULT = EXECUTE('PDATA = '+COLNAMES[I])
            NVALS = PDATA[0,*]          ;; Number of values in each row
            NTOT  = ROUND(TOTAL(NVALS)) ;; Total number of values
            IF NTOT EQ 0 THEN BEGIN
                DD = {N_ELEMENTS: 0L, N_ROWS: NROWS0, $
                      INDICES: LON64ARR(NROWS0+1), DATA: 0L}
                GOTO, FILL_VARICOL
            ENDIF

            ;; Compute the width in bytes of the data value
            TYPE = IDLTYPE[ICOL[I], ILUN]
            WID = LONG64(WIDARR[TYPE < 10])
            IF WID EQ 0 THEN BEGIN
                OUTSTATUS[I] = 0
                MESSAGE = 'ERROR: Column '+COLNAMES[I]+' has unknown data type'
                IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
                        ERRMSG = MESSAGE
                        RETURN
                END ELSE MESSAGE, MESSAGE
            ENDIF

            ;; Coalesce the data pointers
            BOFF1 = LONG64(PDATA[1,*])
            BOFF2 = BOFF1 + NVALS*WID
            WH = WHERE(BOFF1[1:*] NE BOFF2, CT)
            IF CT GT 0 THEN BI = [-1LL, WH, N_ELEMENTS(BOFF1)-1] $
            ELSE            BI = [-1LL,     N_ELEMENTS(BOFF1)-1]
            CT = CT + 1

            ;; Create the output array
            BC = BOFF2[BI[1:*]] - BOFF1[BI[0:CT-1]+1] ;; Byte count
            NB = ROUND(TOTAL(BC))                     ;; Total # bytes
            BB = BYTARR(NB)                           ;; Byte array

            ;; Initialize the counter variables used in the read-loop
            CC = 0LL & CC1 = 0LL & K = 0LL
            BUFFROWS = ROUND(BUFFERSIZE/WID) > 128L
            BASE = LONG64(NHEADER[ILUN]+HEAP[ILUN])

            ;; Read data from file
            WHILE CC LT NB DO BEGIN
                NB1 = (BC[K]-CC1) < BUFFROWS
                BB1 = BYTARR(NB1)

                POINT_LUN, UNIT, BASE+BOFF1[BI[K]+1]+CC1
                READU, UNIT, BB1
;                FXGSEEK, UNIT, BASE+BOFF1[BI[K]+1]+CC1
;                FXGREAD, UNIT, BB1
                BB[CC] = TEMPORARY(BB1)

                CC  = CC  + NB1
                CC1 = CC1 + NB1
                IF CC1 EQ BC[K] THEN BEGIN
                    K = K + 1
                    CC1 = 0L
                ENDIF
            ENDWHILE

            ;; Convert the data
            IF N_ELEMENTS(NANVALUES) GT 0 THEN $
              EXTRA={NANVALUE: NANVALUES[I]}

            FXBREADM_CONV, BB, DD, TYPE, NTOT, 1L, $
              NOIEEE=KEYWORD_SET(NOIEEE), NOSCALE=KEYWORD_SET(NOSCALE), $
              TZERO=TZERO[ICOL[I], ILUN], TSCAL=TSCAL[ICOL[I], ILUN], $
              DEFAULT_FLOAT=DEFAULT_FLOAT, _EXTRA=EXTRA
            
            ;; Ensure the correct dimensions, now that we know them
            COLNDIM[I] = 1
            COLDIM[I,0] = NTOT
            
            ;; Construct the indices; unfortunately we need to make an
            ;; accumulant with a FOR loop
            INDICES = LON64ARR(NROWS0+1)
            FOR K = 1LL, NROWS0 DO $
              INDICES[K] = INDICES[K-1] + NVALS[K-1]

            ;; Construct a structure with additional data
            DD = {N_ELEMENTS: NTOT, N_ROWS: NROWS0, TYPE: TYPE, $
                  INDICES: INDICES, DATA: TEMPORARY(DD)}

            FILL_VARICOL:
            RESULT = EXECUTE(COLNAMES[I] +' = TEMPORARY(DD)')
        ENDFOR

;
;  Compose the output columns, which might need reforming
;
        FOR I = 0, NUMCOLS-1 DO BEGIN
            IF OUTSTATUS[I] NE 1 THEN GOTO, LOOP_END_FINAL

            ;; Extract the dimensions and name of the column data
            DIMS = COLDIM[I,0:COLNDIM[I]-1]
            NEL  = PRODUCT(DIMS)
            CNAME = COLNAMES[I]
            IF VARICOL[I] THEN CNAME = CNAME + '.DATA'

            ;; Compose the reforming part
            IF NEL EQ 1 THEN $
              CMD = CNAME+'[0]' $
            ELSE $
              CMD = 'REFORM(TEMPORARY('+CNAME+'),DIMS,/OVERWRITE)'

            ;; Variable-length columns return extra information
            IF VARICOL[I] THEN BEGIN
                CMD = ('{VARICOL:    1,'+$
                       ' N_ELEMENTS: '+COLNAMES[I]+'.N_ELEMENTS, '+$
                       ' TYPE:       '+COLNAMES[I]+'.TYPE, '+$
                       ' N_ROWS:     '+COLNAMES[I]+'.N_ROWS, '+$
                       ' INDICES:    '+COLNAMES[I]+'.INDICES, '+$
                       ' DATA:       '+CMD+'}')
            ENDIF

            ;; Assign to pointer, or re-assign to column
            IF PASS EQ 'ARGUMENT' THEN $
              CMD = COLNAMES[I]+' = ' + CMD $
            ELSE IF PASS EQ 'POINTER' THEN $
              CMD = '*(POINTERS[I]) = ' + CMD 

            RESULT = EXECUTE(CMD)
            LOOP_END_FINAL:
        ENDFOR

        PROC_CLEANUP:
;
        IF N_ELEMENTS(ERRMSG) NE 0 THEN ERRMSG = ''
        RETURN
        
        END
