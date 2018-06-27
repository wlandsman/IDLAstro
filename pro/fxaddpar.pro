;+
; NAME: 
;       FXADDPAR
; Purpose     : 
;       Add or modify a parameter in a FITS header array.
; Explanation : 
;       This version of FXADDPAR will write string values longer than 68 
;       characters using the FITS continuation convention described at 
;       http://fits.gsfc.nasa.gov/registry/continue_keyword.html
; Use         : 
;       FXADDPAR, HEADER, NAME, VALUE, COMMENT
; Inputs      : 
;       HEADER  = String array containing FITS header.  The maximum string
;                 length must be equal to 80.  If not defined, then FXADDPAR
;                 will create an empty FITS header array.
;
;       NAME    = Name of parameter.  If NAME is already in the header the
;                 value and possibly comment fields are modified. Otherwise a
;                 new record is added to the header.  If NAME is equal to
;                 either "COMMENT" or "HISTORY" then the value will be added to
;                 the record without replacement.  In this case the comment
;                 parameter is ignored.
;
;       VALUE   = Value for parameter.  The value expression must be of the
;                 correct type, e.g. integer, floating or string.
;                 String values of 'T' or 'F' are considered logical
;                 values unless the /NOLOGICAL keyword is set.  If the value is
;                 a string and is "long" (more than 69 characters), then it 
;                 may be continued over more than one line using the OGIP 
;                 CONTINUE standard.
;
;                 The special BOOLEAN datatype introduced in IDL 8.4 is also
;                 recognized, and recorded as either 'T' or 'F' in the header.
;
; Opt. Inputs : 
;       COMMENT = String field.  The '/' is added by this routine.  Added
;                 starting in position 31.  If not supplied, or set equal to ''
;                 (the null string), then any previous comment field in the
;                 header for that keyword is retained (when found).
; Outputs     : 
;       HEADER  = Updated header array.
; Opt. Outputs: 
;       None.
; Keywords    : 
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
;                 real and imaginary parts.  If not supplied, then the IDL
;                 default formatting is used, except that double precision is
;                 given a format of G19.12.
;
;       /NOCONTINUE = By default, FXADDPAR will break strings longer than 68 
;                characters into multiple lines using the continuation
;                convention.    If this keyword is set, then the line will
;                instead be truncated to 68 characters.    This was the default
;                behaviour of FXADDPAR prior to December 1999.  
;
;      /NOLOGICAL = If set, then the values 'T' and 'F' are not interpreted as
;                logical values, and are simply added without interpretation.
;
;       /NULL   = If set, then keywords with values which are undefined, or
;                 which have non-finite values (such as NaN, Not-a-Number) are
;                 stored in the header without a value, such as
;
;                       MYKEYWD =                      /My comment
;
;       MISSING = A value which signals that data with this value should be
;                 considered missing.  For example, the statement
;
;                       FXADDPAR, HEADER, 'MYKEYWD', -999, MISSING=-999
;
;                 would result in the valueless line described above for the
;                 /NULL keyword.  Setting MISSING to a value implies /NULL.
;                 Cannot be used with string or complex values.
;
;	ERRMSG	 = If defined and passed, then any error messages will be
;		   returned to the user in this parameter rather than
;		   depending on the MESSAGE routine in IDL, e.g.
;
;			ERRMSG = ''
;			FXADDPAR, ERRMSG=ERRMSG, ...
;			IF ERRMSG NE '' THEN ...
;
; Calls       : 
;       DETABIFY(), FXPAR(), FXPARPOS()
; Common      : 
;       None.
; Restrictions: 
;       Warning -- Parameters and names are not checked against valid FITS
;       parameter names, values and types.
;
;       The required FITS keywords SIMPLE (or XTENSION), BITPIX, NAXIS, NAXIS1,
;       NAXIS2, etc., must be entered in order.  The actual values of these
;       keywords are not checked for legality and consistency, however.
;
; Side effects: 
;       All HISTORY records are inserted in order at the end of the header.
;
;       All COMMENT records are also inserted in order at the end of the
;       header, but before the HISTORY records.  The BEFORE and AFTER keywords
;       can override this.
;
;       All records with no keyword (blank) are inserted in order at the end of
;       the header, but before the COMMENT and HISTORY records.  The BEFORE and
;       AFTER keywords can override this.
;
;       All other records are inserted before any of the HISTORY, COMMENT, or
;       "blank" records.  The BEFORE and AFTER keywords can override this.
;
;       String values longer than 68 characters will be split into multiple
;       lines using the OGIP CONTINUE convention, unless the /NOCONTINUE keyword
;       is set.    For a description of the CONTINUE convention see    
;       http://fits.gsfc.nasa.gov/registry/continue_keyword.html
; Category    : 
;       Data Handling, I/O, FITS, Generic.
; Prev. Hist. : 
;       William Thompson, Jan 1992, from SXADDPAR by D. Lindler and J. Isensee.
;       Differences include:
;
;               * LOCATION parameter replaced with keywords BEFORE and AFTER.
;               * Support for COMMENT and "blank" FITS keywords.
;               * Better support for standard FITS formatting of string and
;                 complex values.
;               * Built-in knowledge of the proper position of required
;                 keywords in FITS (although not necessarily SDAS/Geis) primary
;                 headers, and in TABLE and BINTABLE extension headers.
;
;       William Thompson, May 1992, fixed bug when extending length of header,
;       and new record is COMMENT, HISTORY, or blank.
; Written     : 
;       William Thompson, GSFC, January 1992.
; Modified    : 
;       Version 1, William Thompson, GSFC, 12 April 1993.
;               Incorporated into CDS library.
;       Version 2, William Thompson, GSFC, 5 September 1997
;               Fixed bug replacing strings that contain "/" character--it
;               interpreted the following characters as a comment.
;       Version 3, Craig Markwardt, GSFC,  December 1997
;               Allow long values to extend over multiple lines
;	Version 4, D. Lindler, March 2000, modified to use capital E instead
;		of a lower case e for exponential format.
;       Version 4.1 W. Landsman April 2000, make user-supplied format uppercase
;       Version 4.2 W. Landsman July 2002, positioning of EXTEND keyword
;       Version 5, 23-April-2007, William Thompson, GSFC
;       Version 6, 02-Aug-2007, WTT, bug fix for OGIP long lines
;       Version 6.1, 10-Feb-2009, W. Landsman, increase default format precision
;       Version 6.2  30-Sep-2009, W. Landsman, added /NOLOGICAL keyword
;       Version 7, 13-Aug-2015, William Thompson, allow null values
;               Add keywords /NULL, MISSING.  Catch non-finite values (e.g. NaN)
;       Version 7.1, 22-Sep-2015, W. Thompson, No slash if null & no comment
;       Version 8, 15-Sep-2016, W. Thompson, treat byte and boolean values
;       Version 8.1, 28-Sep-2016, W. Thompson, use EXECUTE() for pre 8.4
;       Version 8.2, 28-Sep-2016, W. Thompson, instead use COMPILE_OPT IDL2
;       Version 9, 16-Mar-2017, W. Thompson, include comments in long strings
;               Use FXPARPOS, /LAST option.  Put space between slash and
;               comment
;       Version 10, 21-Jun-2018, W. Thompson, for backward compatibility, save
;               non-finite values (e.g. NaN) as strings if /NULL not set
; Version     : 
;       Version 10, 21-Jun-2018
;-
;

; This is a utility routine, which splits a parameter into several
; continuation bits.
PRO FXADDPAR_CONTPAR, VALUE, COMMENT, CONTINUED
  
  APOST = "'"
  BLANK = STRING(REPLICATE(32B,80)) ;BLANK line

  ;; The value may not need to be CONTINUEd.  If it does, then split
  ;; out the first value now.  The first value does not have a
  ;; CONTINUE keyword, because it will be grafted onto the proper
  ;; keyword in the calling routine.

  IF (STRLEN(VALUE) GT 68) THEN BEGIN
      CONTINUED = [ STRMID(VALUE, 0, 67)+'&' ]
      VALUE = STRMID(VALUE, 67, STRLEN(VALUE)-67)
  ENDIF ELSE BEGIN
      CONTINUED = [ VALUE ]
      RETURN
  ENDELSE

  ;; Split out the remaining values.
  WHILE (STRLEN(VALUE) GT 0) OR (STRLEN(COMMENT) GT 0) DO BEGIN
      H = BLANK

      ;; Add CONTINUE keyword
      STRPUT, H, 'CONTINUE  '+APOST
      ;; Add the next split
      IF(STRLEN(VALUE) GT 68) THEN BEGIN
          STRPUT, H, STRMID(VALUE, 0, 67)+'&'+APOST, 11
          VALUE = STRMID(VALUE, 67, STRLEN(VALUE)-67)
      ENDIF ELSE IF (STRLEN(VALUE)+STRLEN(COMMENT)) GT 66 THEN BEGIN
          IF STRLEN(VALUE) GT 67 THEN BEGIN
              STRPUT, H, STRMID(VALUE, 0, 67)+'&'+APOST, 11
              VALUE = STRMID(VALUE, 67, STRLEN(VALUE)-67)
          ENDIF ELSE BEGIN
              TEMP = VALUE+'&'+APOST+' /'
              VALUE = ''
              NTEMP = STRLEN(TEMP)
              IF NTEMP LT 68 THEN BEGIN
                  NCOM = 68 - NTEMP
                  WORDS = STRSPLIT(COMMENT, ' ', /EXTRACT)
                  IF STRLEN(WORDS[0]) GT NCOM THEN BEGIN
                      TEMP = TEMP + ' ' + STRMID(COMMENT,0,NCOM)
                      COMMENT = STRMID(COMMENT, NCOM, STRLEN(COMMENT)-NCOM)
                  ENDIF ELSE BEGIN
                      WHILE STRLEN(WORDS[0]) LT NCOM DO BEGIN
                          TEMP = TEMP + ' ' + WORDS[0]
                          NTEMP = STRLEN(TEMP)
                          NCOM = 69 - NTEMP
                          WORDS = WORDS[1:*]
                      ENDWHILE
                      COMMENT = WORDS[0]
                      FOR IWORD = 1,N_ELEMENTS(WORDS)-1 DO $
                        COMMENT = COMMENT + ' ' + WORDS[IWORD]
                  ENDELSE
              ENDIF
              STRPUT, H, TEMP, 11
          ENDELSE
      ENDIF ELSE BEGIN
          TEMP = VALUE+APOST
          IF N_ELEMENTS(COMMENT) GT 0 THEN TEMP = TEMP + ' / ' + COMMENT
          STRPUT, H, TEMP, 11
          VALUE = ''
          COMMENT = ''
      ENDELSE

      CONTINUED = [ CONTINUED, H ]
  ENDWHILE

  RETURN
END

; Utility routine to add a warning to the file.  The calling routine
; must ensure that the header is in a consistent state before calling
; FXADDPAR_CONTWARN because the header will be subsequently modified
; by calls to FXADDPAR.
PRO FXADDPAR_CONTWARN, HEADER, NAME

;  By OGIP convention, the keyword LONGSTRN is added to the header as
;  well.  It should appear before the first occurrence of a long
;  string encoded with the CONTINUE convention.

  CONTKEY = FXPAR(HEADER, 'LONGSTRN', COUNT = N_LONGSTRN)

;  Calling FXADDPAR here is okay since the state of the header is
;  clean now.
  IF N_LONGSTRN GT 0 THEN $
    RETURN

  FXADDPAR, HEADER, 'LONGSTRN', 'OGIP 1.0', $
    'The OGIP long string convention may be used.', $
    BEFORE=NAME

  FXADDPAR, HEADER, 'COMMENT', $
    'This FITS file may contain long string keyword values that are', $
    BEFORE=NAME

  FXADDPAR, HEADER, 'COMMENT', $
    "continued over multiple keywords.  This convention uses the  '&'", $
    BEFORE=NAME

  FXADDPAR, HEADER, 'COMMENT', $
    'character at the end of a string which is then continued', $
    BEFORE=NAME

  FXADDPAR, HEADER, 'COMMENT', $
    "on subsequent keywords whose name = 'CONTINUE'.", $
    BEFORE=NAME

  RETURN
END


PRO FXADDPAR, HEADER, NAME, VALUE, COMMENT, BEFORE=BEFORE,      $
              AFTER=AFTER, FORMAT=FORMAT, NOCONTINUE = NOCONTINUE, $
              ERRMSG=ERRMSG, NOLOGICAL=NOLOGICAL, MISSING=MISSING, NULL=NULL
        COMPILE_OPT IDL2
        ON_ERROR,2                              ;Return to caller
;
;  Check the number of parameters.
;
        IF N_PARAMS() LT 3 THEN BEGIN
            MESSAGE = 'Syntax:  FXADDPAR, HEADER, NAME, VALUE [, COMMENT ]'
            GOTO, HANDLE_ERROR
        ENDIF
;
; Define a blank line and the END line
;
        ENDLINE = 'END' + STRING(REPLICATE(32B,77))     ;END line
        BLANK = STRING(REPLICATE(32B,80))               ;BLANK line
;
;  If no comment was passed, then use a null string.
;
        IF N_PARAMS() LT 4 THEN COMMENT = ''
;
;  Check the HEADER array.
;
        N = N_ELEMENTS(HEADER)          ;# of lines in FITS header
        IF N EQ 0 THEN BEGIN            ;header defined?
                HEADER=STRARR(36)       ;no, make it.
                HEADER[0]=ENDLINE
                N=36
        ENDIF ELSE BEGIN
                S = SIZE(HEADER)        ;check for string type
                IF (S[0] NE 1) OR (S[2] NE 7) THEN BEGIN
                    MESSAGE = 'FITS Header (first parameter) must be a ' + $
                      'string array'
                    GOTO, HANDLE_ERROR
                ENDIF
        ENDELSE
;
;  Make sure NAME is 8 characters long
;
        NN = STRING(REPLICATE(32B,8))   ;8 char name
        STRPUT,NN,STRUPCASE(NAME)       ;Insert name
;
;  Check VALUE.
;
        S = SIZE(VALUE)         ;get type of value parameter
        STYPE = S[S[0]+1]
        SAVE_AS_NULL = 0
        IF S[0] NE 0 THEN BEGIN
            MESSAGE = 'Keyword Value (third parameter) must be scalar'
            GOTO, HANDLE_ERROR
        END ELSE IF STYPE EQ 0 THEN BEGIN
            IF (N_ELEMENTS(MISSING) EQ 1) OR KEYWORD_SET(NULL) THEN $
              SAVE_AS_NULL = 1 ELSE BEGIN
                MESSAGE = 'Keyword Value (third parameter) is not defined'
                GOTO, HANDLE_ERROR
            ENDELSE
        END ELSE IF STYPE EQ 8 THEN BEGIN
            MESSAGE = 'Keyword Value (third parameter) cannot be structure'
            GOTO, HANDLE_ERROR
        ENDIF
;
;  Check to see if the parameter should be saved as a null value.
;
        IF (STYPE NE 6) AND (STYPE NE 7) AND (STYPE NE 9) THEN BEGIN
            IF N_ELEMENTS(MISSING) EQ 1 THEN $
              IF VALUE EQ MISSING THEN SAVE_AS_NULL = 1
            IF NOT SAVE_AS_NULL THEN IF NOT FINITE(VALUE) THEN BEGIN
                IF ((N_ELEMENTS(MISSING) EQ 1) OR KEYWORD_SET(NULL)) THEN $
                  SAVE_AS_NULL = 1 ELSE BEGIN
                    MESSAGE, /CONTINUE, 'Keyword Value (third parameter) ' + $
                             'is not finite, saving as string.'
                    STYPE = 7
                    SAVE_AS_STRING = 1
                ENDELSE
            ENDIF
        ENDIF
;
;  Extract first 8 characters of each line of header, and locate END line
;
        KEYWRD = STRMID(HEADER,0,8)                     ;Header keywords
        IEND = WHERE(KEYWRD EQ 'END     ',NFOUND)
;
;  If no END, then add it.  Either put it after the last non-null string, or
;  append it to the end.
;
        IF NFOUND EQ 0 THEN BEGIN
                II = WHERE(STRTRIM(HEADER) NE '',NFOUND)
                II = MAX(II) + 1
                IF (NFOUND EQ 0) OR (II EQ N_ELEMENTS(HEADER)) THEN     $
                        HEADER = [HEADER,ENDLINE] ELSE HEADER[II] = ENDLINE
                KEYWRD = STRMID(HEADER,0,8)
                IEND = WHERE(KEYWRD EQ 'END     ',NFOUND)
        ENDIF
;
        IEND = IEND[0] > 0                      ;Make scalar
;
;  History, comment and "blank" records are treated differently from the
;  others.  They are simply added to the header array whether there are any
;  already there or not.
;
        IF (NN EQ 'COMMENT ') OR (NN EQ 'HISTORY ') OR          $
                        (NN EQ '        ') THEN BEGIN
;
;  If the header array needs to grow, then expand it in increments of 36 lines.
;
                IF IEND GE (N-1) THEN BEGIN
                        HEADER = [HEADER,REPLICATE(BLANK,36)]
                        N = N_ELEMENTS(HEADER)
                ENDIF
;
;  Format the record.
;
                NEWLINE = BLANK
                IF STYPE EQ 1 THEN SVALUE = STRING(FIX(VALUE)) ELSE $
                  SVALUE = STRING(VALUE)
                STRPUT,NEWLINE,NN+SVALUE,0
;
;  If a history record, then append to the record just before the end.
;
                IF NN EQ 'HISTORY ' THEN BEGIN
                        HEADER[IEND] = NEWLINE          ;add history rec.
                        HEADER[IEND+1]=ENDLINE          ;move end up
;
;  The comment record is placed immediately after the last previous comment
;  record, or immediately before the first history record, unless overridden by
;  either the BEFORE or AFTER keywords.
;
                END ELSE IF NN EQ 'COMMENT ' THEN BEGIN
                        I = FXPARPOS(KEYWRD,IEND,AFTER=AFTER,BEFORE=BEFORE)
                        IF I EQ IEND THEN I =   $
                            FXPARPOS(KEYWRD,IEND,AFTER='COMMENT',$
                                     BEFORE='HISTORY')
                        HEADER[I+1] = HEADER[I:N-2]     ;move rest up
                        HEADER[I] = NEWLINE             ;insert comment
;
;  The "blank" record is placed immediately after the last previous "blank"
;  record, or immediately before the first comment or history record, unless
;  overridden by either the BEFORE or AFTER keywords.
;
                END ELSE BEGIN
                        I = FXPARPOS(KEYWRD,IEND,AFTER=AFTER,BEFORE=BEFORE)
                        IF I EQ IEND THEN I =   $
                            FXPARPOS(KEYWRD,IEND,AFTER='',BEFORE='COMMENT')<$
                            FXPARPOS(KEYWRD,IEND,AFTER='',BEFORE='HISTORY')
                        HEADER[I+1] = HEADER[I:N-2]     ;move rest up
                        HEADER[I] = NEWLINE             ;insert "blank"
                ENDELSE
                RETURN
        ENDIF                           ;history/comment/blank
;
;  Find location to insert keyword.  If the keyword is already in the header,
;  then simply replace it.  If no new comment is passed, then retain the old
;  one.
;
        IPOS  = WHERE(KEYWRD EQ NN,NFOUND)
        IF NFOUND GT 0 THEN BEGIN
                I = IPOS[0]
                IF COMMENT EQ '' THEN BEGIN
                        SLASH = STRPOS(HEADER[I],'/')
                        QUOTE = STRPOS(HEADER[I],"'")
                        IF (QUOTE GT 0) AND (QUOTE LT SLASH) THEN BEGIN
                                QUOTE = STRPOS(HEADER[I],"'",QUOTE+1)
                                IF QUOTE LT 0 THEN SLASH = -1 ELSE      $
                                        SLASH = STRPOS(HEADER[I],'/',QUOTE+1)
                        ENDIF
                        IF SLASH NE -1 THEN BEGIN
                            SPOS = SLASH + 1
                            IF STRMID(HEADER[I],SPOS,1) EQ ' ' THEN $
                              SPOS = SLASH + 2
                            COMMENT = STRMID(HEADER[I],SPOS,80)
                        END ELSE COMMENT = STRING(REPLICATE(32B,80))
                ENDIF
                GOTO, REPLACE
        ENDIF
;
;  Start of section dealing with the positioning of required FITS keywords.  If
;  the keyword is SIMPLE, then it must be at the beginning.
;
        IF NN EQ 'SIMPLE  ' THEN BEGIN
                I = 0
                GOTO, INSERT
        ENDIF
;
;  In conforming extensions, if the keyword is XTENSION, then it must be at the
;  beginning. 
;
        IF NN EQ 'XTENSION' THEN BEGIN
                I = 0
                GOTO, INSERT
        ENDIF
;
;  If the keyword is BITPIX, then it must follow the either SIMPLE or XTENSION
;  keyword.
;
        IF NN EQ 'BITPIX  ' THEN BEGIN
                IF (KEYWRD[0] NE 'SIMPLE  ') AND                $
                        (KEYWRD[0] NE 'XTENSION') THEN BEGIN
                    MESSAGE = 'Header must start with either SIMPLE or XTENSION'
                    GOTO, HANDLE_ERROR
                ENDIF
                I = 1
                GOTO, INSERT
        ENDIF
;
;  If the keyword is NAXIS, then it must follow the BITPIX keyword.
;
        IF NN EQ 'NAXIS   ' THEN BEGIN
                IF KEYWRD[1] NE 'BITPIX  ' THEN BEGIN
                    MESSAGE = 'Required BITPIX keyword not found'
                    GOTO, HANDLE_ERROR
                ENDIF
                I = 2
                GOTO, INSERT
        ENDIF
;
;  If the keyword is NAXIS1, then it must follow the NAXIS keyword.
;
        IF NN EQ 'NAXIS1  ' THEN BEGIN
                IF KEYWRD[2] NE 'NAXIS   ' THEN BEGIN
                    MESSAGE = 'Required NAXIS keyword not found'
                    GOTO, HANDLE_ERROR
                ENDIF
                I = 3
                GOTO, INSERT
        ENDIF
;
;  If the keyword is NAXIS<n>, then it must follow the NAXIS<n-1> keyword.
;
        IF STRMID(NN,0,5) EQ 'NAXIS' THEN BEGIN
                NUM_AXIS = FIX(STRMID(NN,5,3))
                PREV = STRING(REPLICATE(32B,8))         ;Format NAXIS<n-1>
                STRPUT,PREV,'NAXIS',0                   ;Insert NAXIS
                STRPUT,PREV,STRTRIM(NUM_AXIS-1,2),5     ;Insert <n-1>
                IF KEYWRD[NUM_AXIS+1] NE PREV THEN BEGIN
                    MESSAGE = 'Required '+PREV+' keyword not found'
                    GOTO, HANDLE_ERROR
                ENDIF
                I = NUM_AXIS + 2
                GOTO, INSERT
        ENDIF

;
;  If the keyword is EXTEND, then it must follow the last NAXIS* keyword.
;

        IF NN EQ 'EXTEND  ' THEN BEGIN
                IF KEYWRD[2] NE 'NAXIS   ' THEN BEGIN
                    MESSAGE = 'Required NAXIS keyword not found'
                    GOTO, HANDLE_ERROR
                ENDIF
                FOR I = 3, N-2 DO $   
                    IF STRMID(KEYWRD[I],0,5) NE 'NAXIS' THEN GOTO, INSERT 
                   
         ENDIF
    
;
;  If the first keyword is XTENSION, and has the value of either 'TABLE' or
;  'BINTABLE', then there are some additional required keywords.
;
        IF KEYWRD[0] EQ 'XTENSION' THEN BEGIN
                XTEN = FXPAR(HEADER,'XTENSION')
                IF (XTEN EQ 'TABLE   ') OR (XTEN EQ 'BINTABLE') THEN BEGIN
;
;  If the keyword is PCOUNT, then it must follow the NAXIS2 keyword.
;
                        IF NN EQ 'PCOUNT  ' THEN BEGIN
                                IF KEYWRD[4] NE 'NAXIS2  ' THEN BEGIN
                                    MESSAGE = 'Required NAXIS2 keyword not found'
                                    GOTO, HANDLE_ERROR
                                ENDIF
                                I = 5
                                GOTO, INSERT
                        ENDIF
;
;  If the keyword is GCOUNT, then it must follow the PCOUNT keyword.
;
                        IF NN EQ 'GCOUNT  ' THEN BEGIN
                                IF KEYWRD[5] NE 'PCOUNT  ' THEN BEGIN
                                    MESSAGE = 'Required PCOUNT keyword not found'
                                    GOTO, HANDLE_ERROR
                                ENDIF
                                I = 6
                                GOTO, INSERT
                        ENDIF
;
;  If the keyword is TFIELDS, then it must follow the GCOUNT keyword.
;
                        IF NN EQ 'TFIELDS ' THEN BEGIN
                                IF KEYWRD[6] NE 'GCOUNT  ' THEN BEGIN
                                    MESSAGE = 'Required GCOUNT keyword not found'
                                    GOTO, HANDLE_ERROR
                                ENDIF
                                I = 7
                                GOTO, INSERT
                        ENDIF
                ENDIF
        ENDIF
;
;  At this point the location has not been determined, so a new line is added
;  at the end of the FITS header, but before any blank, COMMENT, or HISTORY
;  keywords at the end of the header, unless overridden by the BEFORE or AFTER
;  keywords.
;
        I = FXPARPOS(KEYWRD,IEND,AFTER=AFTER,BEFORE=BEFORE)
        IF I EQ IEND THEN I = FXPARPOS(KEYWRD,IEND,AFTER=AFTER,/LAST)
;
;  A new line needs to be added.  First check to see if the length of the
;  header array needs to be extended.  Then insert a blank record at the proper
;  place.
;
INSERT:
        IF IEND EQ (N-1) THEN BEGIN
                HEADER = [HEADER,REPLICATE(BLANK,36)]
                N = N_ELEMENTS(HEADER)
        ENDIF
        HEADER[I+1] = HEADER[I:N-2]
        HEADER[I] = BLANK
        IEND = IEND + 1        ; CM 24 Sep 1997
;
;  Now put value into keyword at line I.
;
REPLACE: 
        H=BLANK                 ;80 blanks
        STRPUT,H,NN+'= '        ;insert name and =.
        APOST = "'"             ;quote (apostrophe) character
;
;  Store the value depending on the data type.  If a character string, first
;  check to see if it is one of the logical values "T" (true) or "F" (false).
;

        IF STYPE EQ 7 THEN BEGIN              ;which type?
                UPVAL = STRUPCASE(VALUE)        ;force upper case.
                IF ~KEYWORD_SET(NOLOGICAL)  $ 
		   &&  ((UPVAL EQ 'T') OR (UPVAL EQ 'F')) THEN BEGIN
                        STRPUT,H,UPVAL,29       ;insert logical value.
;
;  Otherwise, remove any tabs, and check for any apostrophes in the string.
;
                END ELSE BEGIN
                        IF KEYWORD_SET(SAVE_AS_STRING) THEN $
                          VAL = STRTRIM(VALUE,2) ELSE VAL = DETABIFY(VALUE)
                        NEXT_CHAR = 0
                        REPEAT BEGIN
                                AP = STRPOS(VAL,"'",NEXT_CHAR)
                                IF AP GE 66 THEN BEGIN
                                        VAL = STRMID(VAL,0,66)
                                END ELSE IF AP GE 0 THEN BEGIN
                                        VAL = STRMID(VAL,0,AP+1) + APOST + $
                                          STRMID(VAL,AP+1,80)
                                        NEXT_CHAR = AP + 2
                                ENDIF
                        ENDREP UNTIL AP LT 0

;
;  If a long string, then add the comment as soon as possible.
;
; CM 24 Sep 1997
;  Separate parameter if it needs to be CONTINUEd.
;
                        IF NOT KEYWORD_SET(NOCONTINUE) THEN BEGIN
                            CCOM = STRTRIM(COMMENT)
                            FXADDPAR_CONTPAR, VAL, CCOM, CVAL
                        ENDIF ELSE CVAL = STRMID(VAL,0,68)
                        K = I + 1
                        ;; See how many CONTINUE lines there already are
                        WHILE K LT IEND DO BEGIN
                            IF STRMID(HEADER[K],0,8) NE 'CONTINUE' THEN $
                              GOTO, DONE_CHECK_CONT
                            K = K + 1
                        ENDWHILE
                        
                        DONE_CHECK_CONT:
                        NOLDCONT = K - I - 1
                        NNEWCONT = N_ELEMENTS(CVAL) - 1

                        ;; Insert new lines if needed
                        IF NNEWCONT GT NOLDCONT THEN BEGIN
                            INS = NNEWCONT - NOLDCONT
                            WHILE IEND+INS GE N DO BEGIN
                                HEADER = [HEADER, REPLICATE(BLANK,36)]
                                N = N_ELEMENTS(HEADER)
                            ENDWHILE
                        ENDIF 

                        ;; Shift the old lines properly
                        IF NNEWCONT NE NOLDCONT THEN $
                          HEADER[I+NNEWCONT+1] = HEADER[I+NOLDCONT+1:IEND]
                        IEND = IEND + NNEWCONT - NOLDCONT

                        ;; Blank out any lines at the end if needed
                        IF NNEWCONT LT NOLDCONT THEN BEGIN
                            DEL = NOLDCONT - NNEWCONT
                            HEADER[IEND+1:IEND+DEL] = REPLICATE('', DEL)
                        ENDIF

                        IF STRLEN(CVAL[0]) GT 18 THEN BEGIN
                            STRPUT,H,APOST+STRMID(CVAL[0],0,68)+APOST+ $
                              ' / '+COMMENT,10
                            HEADER[I]=H
                                
;  There might be a continuation of this string.  CVAL would contain
;  more than one element if that is so.
                            
                            ;; Add new continuation lines
                            IF N_ELEMENTS(CVAL) GT 1 THEN BEGIN
                              HEADER[I+1] = CVAL[1:*]
                            
                            ;; Header state is now clean, so add
                            ;; warning to header

                               FXADDPAR_CONTWARN, HEADER, NAME
                            ENDIF
                            DONE_CONT:
                            RETURN
;
;  If a short string, then pad out to at least eight characters.
;
                        END ELSE BEGIN
                                STRPUT,H,APOST+CVAL[0],10
                                STRPUT,H,APOST,11+(STRLEN(CVAL[0])>8)
                        ENDELSE

                    ENDELSE
;
;  If complex, then format the real and imaginary parts, and add the comment
;  beginning in column 51.
;
        END ELSE IF (STYPE EQ 6) OR (STYPE EQ 9) THEN BEGIN
                IF STYPE EQ 6 THEN VR = FLOAT(VALUE) ELSE VR = DOUBLE(VALUE)
                VI = IMAGINARY(VALUE)
                IF N_ELEMENTS(FORMAT) EQ 1 THEN BEGIN   ;use format keyword
                        VR = STRING(VR, '('+STRUPCASE(FORMAT)+')')
                        VI = STRING(VI, '('+STRUPCASE(FORMAT)+')')
                 END ELSE BEGIN
                        VR = STRTRIM(VR, 2)
                        VI = STRTRIM(VI, 2)
                ENDELSE
                SR = STRLEN(VR)  &  STRPUT,H,VR,(30-SR)>10
                SI = STRLEN(VI)  &  STRPUT,H,VI,(50-SI)>30
                STRPUT,H,' / '+COMMENT,50
                HEADER[I] = H
                RETURN
;
;  If not complex or a string, then format according to either the FORMAT
;  keyword, or the default for that datatype.
;
        END ELSE BEGIN
            IF NOT SAVE_AS_NULL THEN BEGIN
                IF (N_ELEMENTS(FORMAT) EQ 1) THEN $ ;use format keyword
                  V = STRING(VALUE,'('+STRUPCASE(FORMAT)+')' ) ELSE BEGIN
                    IF STYPE EQ 5 THEN V = STRING(VALUE,FORMAT='(G19.12)') $
                    ELSE BEGIN
                        IF STYPE GT 1 THEN SVALUE = STRING(VALUE) ELSE BEGIN
                            SVALUE = STRING(FIX(VALUE))
                            IF !VERSION.RELEASE GE '8.4' THEN BEGIN
                                ISBOOL = ISA(VALUE, /BOOLEAN)
                                IF ISBOOL THEN BEGIN
                                    FT = ['F','T']
                                    SVALUE = FT[VALUE]
                                ENDIF
                            ENDIF
                        ENDELSE
                        V = STRTRIM(SVALUE,2) ;default format
                    ENDELSE
                ENDELSE
                S = STRLEN(V)                 ;right justify
                STRPUT,H,V,(30-S)>10          ;insert
            ENDIF
        ENDELSE
;
;  Add the comment, and store the completed line in the header.  Don't
;  add the slash if the value is null and there is no comment.
;
        IF (NOT SAVE_AS_NULL) OR (STRLEN(STRTRIM(COMMENT)) GT 0) THEN BEGIN
            STRPUT,H,' / ',30   ;add ' / '
            STRPUT,H,COMMENT,33 ;add comment
        ENDIF
        HEADER[I]=H             ;save line
;
        ERRMSG = ''
        RETURN
;
;  Error handling point.
;
HANDLE_ERROR:
	IF ARG_PRESENT(ERRMSG) THEN ERRMSG = 'FXADDPAR: ' + MESSAGE	$
		ELSE MESSAGE, MESSAGE
        RETURN
        END

