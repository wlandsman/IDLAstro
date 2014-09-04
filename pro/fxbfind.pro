	PRO FXBFIND,P1,KEYWORD,COLUMNS,VALUES,N_FOUND,DEFAULT, $
                    COMMENTS=COMMENTS
;+
; NAME: 
;	FXBFIND
; Purpose     : 
;	Find column keywords in a FITS binary table header.
; Explanation : 
;	Finds the value of a column keyword for all the columns in the binary
;	table for which it is set.  For example,
;
;		FXBFIND, UNIT, 'TTYPE', COLUMNS, VALUES, N_FOUND
;
;	Would find all instances of the keywords TTYPE1, TTYPE2, etc.  The
;	array COLUMNS would contain the column numbers for which a TTYPEn
;	keyword was found, and VALUES would contain the values.  N_FOUND would
;	contain the total number of instances found.
;
; Use         : 
;	FXBFIND, [UNIT or HEADER], KEYWORD, COLUMNS, VALUES, N_FOUND
;		[, DEFAULT ]
; Inputs      : 
;	Either UNIT or HEADER must be passed.
;
;	UNIT	= Logical unit number of file opened by FXBOPEN.
;	HEADER	= FITS binary table header.
;	KEYWORD	= Prefix to a series of FITS binary table column keywords.  The
;		  keywords to be searched for are formed by combining this
;		  prefix with the numbers 1 through the value of TFIELDS in the
;		  header.
; Opt. Inputs : 
;	DEFAULT	= Default value to use for any column keywords that aren't
;		  found.  If passed, then COLUMNS and VALUES will contain
;		  entries for every column.  Otherwise, COLUMNS and VALUES only
;		  contain entries for columns where values were found.
; Outputs     : 
;	COLUMNS	= Array containing the column numbers for which values of the
;		  requested keyword series were found.
;	VALUES	= Array containing the found values.
;	N_FOUND	= Number of values found.  The value of this parameter is
;		  unaffected by whether or not DEFAULT is passed.
; Opt. Outputs: 
;	None.
; Output Keywords    : 
;      COMMENTS = Comments associated with each keyword, if any
; Calls       : 
;	FXBFINDLUN, FXPAR
; Common      : 
;	Uses common block FXBINTABLE--see "fxbintable.pro" for more
;	information.
; Restrictions: 
;	If UNIT is passed, then the file must have been opened with FXBOPEN.
;	If HEADER is passed, then it must be a legal FITS binary table header.
;
;	The type of DEFAULT must be consistent with the values of the requested
;	keywords, i.e. both most be either of string or numerical type.
;
;	The KEYWORD prefix must not have more than five characters to leave
;	room for the three digits allowed for the column numbers.
;
; Side effects: 
;	None.
; Category    : 
;	Data Handling, I/O, FITS, Generic.
; Prev. Hist. : 
;	William Thompson, Feb. 1992.
; Written     : 
;	William Thompson, GSFC, February 1992.
; Modified    : 
;	Version 1, William Thompson, GSFC, 12 April 1993.
;		Incorporated into CDS library.
;       Vectorized implementation improves performance, CM 18 Nov 1999
;       Added COMMENTS keyword CM Nov 2003
;       Remove use of obsolete !ERR system variable W. Landsman April 2010
;       Fix error introduced April 2010  W. Landsman
; Version     : 
;	Version 3, April 2010.
;-
;
@fxbintable
	ON_ERROR,2
;
;  Check the number of parameters.
;
	IF N_PARAMS() LT 5 THEN MESSAGE,	$
		'Syntax:  FXBFIND,[UNIT/HEADER],KEYWORD,COLUMNS,VALUES,' + $
		'N_FOUND [,DEFAULT]'
;
;  Get the header.
;
	IF N_ELEMENTS(P1) EQ 1 THEN BEGIN
		ILUN = FXBFINDLUN(P1)
		HEADER = HEAD[*,ILUN]
	END ELSE HEADER = P1
;
;  Get the value of TFIELDS from HEADER.
;
	TFIELDS0 = FXPAR(HEADER,'TFIELDS')
	IF TFIELDS0 EQ 0 THEN MESSAGE,'No columns found in HEADER'

;
;  Extract the keyword values all in one pass
;        
        KEYVALUES = FXPAR(HEADER, STRTRIM(KEYWORD,2)+'*', $
                          COMMENT=COMMENT_STRS, DATATYPE=DEFAULT, COUNT=NKEY)
        N_FOUND = 0L

;
;  INDEX is used as an array index to fill in the final output
;   
        IF NKEY GT 0 THEN BEGIN
            N_FOUND = N_ELEMENTS(KEYVALUES)
            INDEX   = LINDGEN(N_FOUND)
        ENDIF


;
;  INDEX is used as an array index to fill in the final output
;
        IF N_FOUND GT 0 THEN INDEX   = LINDGEN(N_FOUND)
 
;
;  If a default was given, then we are a little more careful to 
;  reproduce the correct number of values.
;
        IF N_ELEMENTS(DEFAULT) GT 0 THEN BEGIN
            ;; If no values were found we need to fill KEYVALUES with
            ;; *something*.
            IF N_FOUND LE 0 THEN KEYVALUES = DEFAULT
            COLUMNS  = LINDGEN(TFIELDS0) + 1

            ;; Make an array with the number of columns in the table
            SZ_VALUE = SIZE(KEYVALUES[0])
            VALUES   = MAKE_ARRAY(TFIELDS0, TYPE=SZ_VALUE[1], VALUE=DEFAULT)
            COMMENTS = STRARR(TFIELDS0)

            ;; Fill the columns which had this keyword
            IF N_FOUND GT 0 THEN BEGIN
                VALUES[INDEX] = KEYVALUES
                COMMENTS[INDEX] = COMMENT_STRS
            ENDIF

        ENDIF ELSE BEGIN

;
;  If no default was given, we can simply return the values returned
;  by FXPAR.
;
            IF N_FOUND GT 0 THEN BEGIN
                COLUMNS = INDEX + 1
                VALUES  = KEYVALUES
                COMMENTS = COMMENT_STRS
            ENDIF

        ENDELSE
        RETURN
            
	END
