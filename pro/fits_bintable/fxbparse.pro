	PRO FXBPARSE, ILUN, HEADER, NO_TDIM=NO_TDIM, ERRMSG=ERRMSG
;+
; NAME: 
;	FXBPARSE
; Purpose     : 
;	Parse the binary table extension header.
; Explanation : 
;	Parses the binary table extension header, and store the information
;	about the format of the binary table in the FXBINTABLE common
;	block--called from FXBCREATE and FXBOPEN.
; Use         : 
;	FXBPARSE, ILUN, UNIT, HEADER
; Inputs      : 
;	ILUN	= Index into the arrays in the FXBINTABLE common block.
;	HEADER	= FITS binary table extension header.
; Opt. Inputs : 
;	None.
; Outputs     : 
;	None.
; Opt. Outputs: 
;	None.
; Keywords    : 
;	NO_TDIM	  = If set, then any TDIMn keywords found in the header are
;		    ignored.
;	ERRMSG	  = If defined and passed, then any error messages will be
;		    returned to the user in this parameter rather than
;		    depending on the MESSAGE routine in IDL.  If no errors are
;		    encountered, then a null string is returned.  In order to
;		    use this feature, ERRMSG must be defined first, e.g.
;
;			ERRMSG = ''
;			FXBPARSE, ERRMSG=ERRMSG, ...
;			IF ERRMSG NE '' THEN ...
;
; Calls       : 
;	FXBFIND, FXBTDIM, FXBTFORM, FXPAR
; Common      : 
;	Uses common block FXBINTABLE--see "fxbintable.pro" for more
;	information.
; Restrictions: 
;	None.
; Side effects: 
;	Any TDIMn keywords found for bit arrays (format 'X') are ignored, since
;	the dimensions would refer to bits, not bytes.
; Category    : 
;	Data Handling, I/O, FITS, Generic.
; Prev. Hist. : 
;	William Thompson, Feb. 1992.
;	William Thompson, Jan. 1993, modified for renamed FXBTFORM and FXBTDIM.
; Written     : 
;	William Thompson, GSFC, February 1992.
; Modified    : 
;	Version 1, William Thompson, GSFC, 12 April 1993.
;		Incorporated into CDS library.
;	Version 2, William Thompson, GSFC, 21 June 1994
;		Added ERRMSG keyword.
;       Version 3, William Thompson, GSFC, 23 June 1994
;               Modified so that ERRMSG is not touched if not defined.
;	Version 4, Michael Schubnell, University of Michigan, 22 May 1996
;		Change N_DIMS from short to long integer.
;	Version 5, W. Landsman, GSFC, 12 Aug 1997
;		Use double complex datatype, if needed
;	Version 6, W. Landsman GSFC 30 Aug 1997
;       Optimized FXPAR; call FXBFIND for speed, CM 1999 Nov 18
;       Modify DHEAP(ILUN) when opening table now, CM 2000 Feb 22
;       Default the TZERO/TSCAL tables to double instead of single
;         precision floating point, CM 2003 Nov 23
;       Make NAXIS1 and NAXIS2 64-bit integers to deal with large files,
;         E. Hivon Mar 2008
;       Remove use of Obsolete !ERR system variable
;  Version 
;       Version 8   April 2010
;-
;
@fxbintable
	ON_ERROR,2
        COMPILE_OPT IDL2
;
;  Check the number of parameters.
;
	IF N_PARAMS() NE 2 THEN BEGIN
		MESSAGE = 'Syntax:  FXBPARSE, ILUN, HEADER'
		IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
			ERRMSG = MESSAGE
			RETURN
		END ELSE MESSAGE, MESSAGE
	ENDIF
;
;  Gather the necessary information, and store it in the common block.
;
	IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
		FXBTFORM,HEADER,BYTOFF0,IDLTYPE0,FORMAT0,N_ELEM0,MAXVAL0, $
			ERRMSG=ERRMSG
		IF ERRMSG NE '' THEN RETURN
	END ELSE FXBTFORM,HEADER,BYTOFF0,IDLTYPE0,FORMAT0,N_ELEM0,MAXVAL0
;
	FXBFIND,HEADER,'TTYPE',COLUMNS,TTYPE0,N_FOUND,''
	FXBFIND,HEADER,'TSCAL',COLUMNS,TSCAL0,N_FOUND,1D
	FXBFIND,HEADER,'TZERO',COLUMNS,TZERO0,N_FOUND,0D
	POINT_LUN,-LUN[ILUN],NHEAD0
;
;  Get the information from the required keywords.
;
	STORE_ARRAY,HEAD,HEADER,ILUN
	NHEADER[ILUN] = NHEAD0
	START = 0L
	NAXIS1[ILUN]  = long64(FXPAR(HEADER,'NAXIS1', START=START))
	NAXIS2[ILUN]  = long64(FXPAR(HEADER,'NAXIS2', START=START))
	TFIELDS[ILUN] = FXPAR(HEADER,'TFIELDS', START=START)
	PCOUNT        = FXPAR(HEADER,'PCOUNT', START=START)
;
;  If THEAP is not present, then set it equal to the size of the table.
;
	THEAP = FXPAR(HEADER,'THEAP', START=START, COUNT=N_THEAP)
	IF N_THEAP LE 0 THEN THEAP = NAXIS1[ILUN]*NAXIS2[ILUN]
	HEAP[ILUN] = THEAP
;
;  Modify DHEAP
;
        DDHEAP = PCOUNT - (THEAP - NAXIS1[ILUN]*NAXIS2[ILUN])
        IF DDHEAP GT 0 THEN DHEAP[ILUN] = DDHEAP ELSE DHEAP[ILUN] = 0
;
;  Store the information about the columns.
;
	STORE_ARRAY,BYTOFF,BYTOFF0,ILUN
	STORE_ARRAY,TTYPE,STRUPCASE(STRTRIM(TTYPE0,2)),ILUN
	STORE_ARRAY,IDLTYPE,IDLTYPE0,ILUN
	STORE_ARRAY,FORMAT,FORMAT0,ILUN
	STORE_ARRAY,N_ELEM,N_ELEM0,ILUN
	STORE_ARRAY,TSCAL,TSCAL0,ILUN
	STORE_ARRAY,TZERO,TZERO0,ILUN
	STORE_ARRAY,MAXVAL,MAXVAL0,ILUN
	STORE_ARRAY,N_DIMS,LONARR(9,N_ELEMENTS(N_ELEM0)),ILUN
;
;  If not a variable length array, then get the dimensions associated with each
;  column from the TDIMn keywords.  If not found, then assume to be the number
;  of elements.
;
	FXBFIND,HEADER,'TDIM',COLUMNS,TDIMS,N_FOUND,''
	FOR ICOL = 0,TFIELDS[ILUN]-1 DO IF MAXVAL[ICOL,ILUN] EQ 0 THEN BEGIN
		TDIM = TDIMS[ICOL]
		TDIM_USED = (TDIM NE '') AND (NOT KEYWORD_SET(NO_TDIM))
		IF TDIM_USED THEN DIMS = FIX(FXBTDIM(TDIM))	$
			     ELSE DIMS = N_ELEM[ICOL,ILUN]
		DIMS = [N_ELEMENTS(DIMS),DIMS]
;
;  If the datatype is a bit array, then no dimensions are applied to the data.
;
		IF FORMAT[ICOL,ILUN] EQ 'X' THEN DIMS = [1,N_ELEM[ICOL,ILUN]]
		N_DIMS[0,ICOL,ILUN] = DIMS
;
;  For those columns which are character strings, then the number of
;  characters, N_CHAR, is the first dimension, and the number of elements is
;  actually N_ELEM/N_CHAR.
;
		IF IDLTYPE[ICOL,ILUN] EQ 7 THEN		$
			N_ELEM[ICOL,ILUN] = N_ELEM[ICOL,ILUN] / DIMS[1]
	ENDIF
;
	IF N_ELEMENTS(ERRMSG) NE 0 THEN ERRMSG = ''
	RETURN
	END
