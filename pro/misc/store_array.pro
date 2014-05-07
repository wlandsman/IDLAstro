	PRO STORE_ARRAY, DESTINATION, INSERT, INDEX
;+
; NAME:
;	STORE_ARRAY
; PURPOSE:
;	Insert array INSERT into the array DESTINATION
; EXPLANATION:
;	The dimensions of the DESTINATION array are adjusted to accommodate
;	the inserted array.
; CATEGOBY:
;	Utility
; CALLING SEQUENCE:
;	STORE_ARRAY, DESTINATION, INSERT, INDEX
; INPUT:
;	DESTINATION	= Array to be expanded.
;	INSERT		= Array to insert into DESTINATION.
;	INDEX		= Index of the final dimension of DESTINATION to insert
;			  INSERT into.
; OUTPUTS:
;	DESTINATION	= Expanded output array.  If both input arrays have the
;			  same number of dimensions, then the DESTINATION will
;			  be replaced with INSERT.
; RESTRICTIONS:
;	DESTINATION and INSERT have to be either both of type string or both of
;	numerical types.
;
;	INSERT must not have more dimensions than DESTINATION.
;
; MODIFICATION HISTOBY:
;	William Thompson, Feb. 1992, from BOOST_ARRAY by D. Zarro and P. Hick.
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
;
	ON_ERROR, 2			;On error, return to caller
;
;  Check the number of parameters.
;
	IF N_PARAMS() NE 3 THEN MESSAGE,	$
		'Syntax:  STORE_ARRAY, DESTINATION, INSERT, INDEX'
;
;  Make sure everything is defined.
;
	IF N_ELEMENTS(INSERT) EQ 0 THEN MESSAGE,'INSERT not defined'
	IF N_ELEMENTS(INDEX) EQ 0 THEN MESSAGE,'INDEX not defined'
;
;  If DESTINATION is not defined, then set it equal to INSERT.
;
	IF N_ELEMENTS(DESTINATION) EQ 0 THEN BEGIN
		DESTINATION = INSERT
		RETURN
	ENDIF
;
;  Get the array types and dimensions of DESTINATION and INSERT.
;
	SD = SIZE(DESTINATION)
	SA = SIZE(INSERT)
	D_NDIM = SD[0]
	A_NDIM = SA[0]
	IF D_NDIM EQ 0 THEN D_DIM = 1 ELSE D_DIM = SD[1:D_NDIM]
	IF A_NDIM EQ 0 THEN A_DIM = 1 ELSE A_DIM = SA[1:A_NDIM]
	D_TYPE = SD[N_ELEMENTS(SD)-2]
	A_TYPE = SA[N_ELEMENTS(SA)-2]
;
;  Treat scalars as one-dimensional arrays.
;
	D_NDIM = D_NDIM > 1
	A_NDIM = A_NDIM > 1
; 
;  Check to see if both arrays are of type string or numeric.
;
	IF D_TYPE EQ 7 THEN D_STRING = 1  ELSE D_STRING = 0
	IF A_TYPE EQ 7 THEN A_STRING = 1  ELSE A_STRING = 0
	IF D_STRING NE A_STRING THEN MESSAGE,	$
		'Data arrays should be either both string or both non-string'
;
;  If both arrays have the same number of elements, then replace DESTINATION
;  with INSERT.
;
	IF D_NDIM EQ A_NDIM THEN BEGIN
		DESTINATION = INSERT
		RETURN
;
;  Otherwise, make sure that INSERT has fewer dimensions than DESTINATION.
;
	END ELSE IF D_NDIM LT A_NDIM THEN MESSAGE,	$
		'INSERT has more dimensions than DESTINATION'
;
;  Check INDEX
;
	LAST = D_DIM[D_NDIM-1] - 1
	IF (INDEX LT 0) OR (INDEX GT LAST) THEN MESSAGE,	$
		'INDEX must be between 0 and ' + STRTRIM(LAST,2)
;
;  Merge the dimensions of DESTINATION and INSERT.
;
	R_DIM = D_DIM
	FOR I = 0,A_NDIM-1 DO R_DIM[I] = D_DIM[I] > A_DIM[I]
;
;  Create the output array with the correct number of elements, and the greater
;  of the types of DESTINATION and INSERT.
;
	OUTPUT = MAKE_ARRAY(DIMENSION=R_DIM, TYPE=(D_TYPE > A_TYPE))
	R_NDIM = N_ELEMENTS(R_DIM)
;
;  If INDEX is not zero, then store the first part of DESTINATION in the output
;  array.
;
	IF INDEX NE 0 THEN BEGIN
	    K = INDEX - 1
	    CASE R_NDIM OF
	    	2:  OUTPUT[0,0] = DESTINATION[*,0:K]
	    	3:  OUTPUT[0,0,0] = DESTINATION[*,*,0:K]
	    	4:  OUTPUT[0,0,0,0] = DESTINATION[*,*,*,0:K]
	    	5:  OUTPUT[0,0,0,0,0] = DESTINATION[*,*,*,*,0:K]
	    	6:  OUTPUT[0,0,0,0,0,0] = DESTINATION[*,*,*,*,*,0:K]
	    	7:  OUTPUT[0,0,0,0,0,0,0] = DESTINATION[*,*,*,*,*,*,0:K]
	    ENDCASE
	ENDIF
;
;  Add INSERT.
;
	CASE R_NDIM OF
		2:  OUTPUT[0,INDEX] = INSERT
		3:  OUTPUT[0,0,INDEX] = INSERT
		4:  OUTPUT[0,0,0,INDEX] = INSERT
		5:  OUTPUT[0,0,0,0,INDEX] = INSERT
		6:  OUTPUT[0,0,0,0,0,INDEX] = INSERT
		7:  OUTPUT[0,0,0,0,0,0,INDEX] = INSERT
	ENDCASE
;
;  Store the remainder of DESTINATION, if any, in the output array.
;
	IF INDEX NE LAST THEN BEGIN
	    K = INDEX + 1
	    CASE R_NDIM OF
	    	2:  OUTPUT[0,K] = DESTINATION[*,K:*]
	    	3:  OUTPUT[0,0,K] = DESTINATION[*,*,K:*]
	    	4:  OUTPUT[0,0,0,K] = DESTINATION[*,*,*,K:*]
	    	5:  OUTPUT[0,0,0,0,K] = DESTINATION[*,*,*,*,K:*]
	    	6:  OUTPUT[0,0,0,0,0,K] = DESTINATION[*,*,*,*,*,K:*]
	    	7:  OUTPUT[0,0,0,0,0,0,K] = DESTINATION[*,*,*,*,*,*,K:*]
	    ENDCASE
	ENDIF
;
;  Replace DESTINATION with OUTPUT, and return.
;
	DESTINATION = OUTPUT
	RETURN
	END
