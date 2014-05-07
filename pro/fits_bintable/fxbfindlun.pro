	FUNCTION FXBFINDLUN, UNIT
;+
; NAME: 
;	FXBFINDLUN()
; Purpose     : 
;	Find logical unit number UNIT in FXBINTABLE common block.
; Explanation : 
;	Finds the proper index to use for getting information about the logical
;	unit number UNIT in the arrays stored in the FXBINTABLE common block.
;	Called from FXBCREATE and FXBOPEN.
; Use         : 
;	Result = FXBFINDLUN( UNIT )
; Inputs      : 
;	UNIT	= Logical unit number.
; Opt. Inputs : 
;	None.
; Outputs     : 
;	The result of the function is an index into the FXBINTABLE common
;	block.
; Opt. Outputs: 
;	None.
; Keywords    : 
;	None.
; Calls       : 
;	None.
; Common      : 
;	Uses common block FXBINTABLE--see "fxbintable.pro" for more
;	information.
; Restrictions: 
;	None.
; Side effects: 
;	If UNIT is not found in the common block, then it is added to the
;	common block.
; Category    : 
;	Data Handling, I/O, FITS, Generic.
; Prev. Hist. : 
;	William Thompson, Feb. 1992.
; Written     : 
;	William Thompson, GSFC, February 1992.
; Modified    : 
;	Version 1, William Thompson, GSFC, 12 April 1993.
;		Incorporated into CDS library.
;	Version 2, William Thompson, GSFC, 21 July 1993.
;		Added DHEAP variable to fix bug with variable length arrays.
;	Version 3, Michael Schubnell, University of Michigan, 22 May 1996
;		Change N_DIMS from short to long integer.
; Version     : 
;	Version 3, 22 May 1996
;	Converted to IDL V5.0   W. Landsman   September 1997
;       Make NAXIS1, NAXIS2, HEAP, DHEAP, BYTOFF 64-bit integers to deal with large files,
;         E. Hivon Mar 2008
;-
;
@fxbintable
	ON_ERROR, 2
;
;  Check the number of parameters.
;
	IF N_PARAMS() NE 1 THEN MESSAGE,	$
		'Syntax:  ILUN = FXBFINDLUN( UNIT )'
;
;  If the common block hasn't been initialized yet, then initialize it.
;
	IF N_ELEMENTS(LUN) EQ 0 THEN BEGIN
		LUN     = UNIT
		STATE	= 0
		HEAD	= ''
		MHEADER	= 0L
		NHEADER = 0L
		NAXIS1  = 0LL
		NAXIS2  = 0LL
		TFIELDS = 0L
		HEAP	= 0LL
		DHEAP	= 0LL
		BYTOFF  = 0LL
		TTYPE	= ''
		FORMAT	= ''
		IDLTYPE	= 0
		N_ELEM	= 0L
		TSCAL	= 1.
		TZERO	= 0.
		MAXVAL	= 0L
		N_DIMS	= LONARR(9,2)
		ILUN = 0
;
;  Otherwise, find the logical unit number in the common block.  If not found,
;  then add it.
;
	END ELSE BEGIN
		ILUN = WHERE(LUN EQ UNIT,NLUN)
		ILUN = ILUN[0]
		IF NLUN EQ 0 THEN BEGIN
			LUN     = [LUN,UNIT]
			STATE	= [STATE,  0]
			BOOST_ARRAY,HEAD,''
			MHEADER = [MHEADER,0]
			NHEADER = [NHEADER,0]
			NAXIS1  = [NAXIS1, 0]
			NAXIS2  = [NAXIS2, 0]
			TFIELDS = [TFIELDS,0]
			HEAP	= [HEAP,   0]
			DHEAP	= [DHEAP,  0]
			BOOST_ARRAY,BYTOFF,0
			BOOST_ARRAY,TTYPE,''
			BOOST_ARRAY,FORMAT,''
			BOOST_ARRAY,IDLTYPE,0
			BOOST_ARRAY,N_ELEM,0
			BOOST_ARRAY,TSCAL,1.
			BOOST_ARRAY,TZERO,0.
			BOOST_ARRAY,MAXVAL,0
			BOOST_ARRAY,N_DIMS,LONARR(9,2)
			ILUN = N_ELEMENTS(LUN)-1
		ENDIF
	ENDELSE
;
;  Return the index into the common block arrays.
;
	RETURN,ILUN
	END
