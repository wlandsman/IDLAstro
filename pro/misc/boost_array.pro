	PRO BOOST_ARRAY, DESTINATION, APPEND
;+
; NAME:
;	BOOST_ARRAY
; PURPOSE:
;	Append one array onto a destination array
; EXPLANATION:
;	Add array APPEND to array DESTINATION, allowing the dimensions of
;	DESTINATION to adjust to accommodate it.  If both input arrays have the
;	same number of dimensions, then the output array will have one
;	additional dimension.  Otherwise, the last dimension of DESTINATION
;	will be incremented by one.
; CATEGORY:
;	Utility
; CALLING SEQUENCE:
;	BOOST_ARRAY, DESTINATION, APPEND
; INPUT:
;	DESTINATION	= Array to be expanded.
;	APPEND		= Array to append to DESTINATION.
; OUTPUTS:
;	DESTINATION	= Expanded output array.
; RESTRICTIONS:
;	DESTINATION and APPEND have to be either both of type string or both of
;	numerical types.
;
;	APPEND cannot have more dimensions than DESTINATION.
;
; MODIFICATION HISTOBY:
;	Written Aug'88 (DMZ, ARC)
;	Modified Sep'89 to handle byte arrays (DMZ)
;	Modifed to version 2, Paul Hick (ARC), Feb 1991
;	Removed restriction to 2D arrays, William Thompson (ARC), Feb 1992.
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
;
	ON_ERROR, 2			;On error, return to caller
;
;  Check the number of parameters.
;
	IF N_PARAMS() NE 2 THEN MESSAGE,	$
		'Syntax:  BOOST_ARRAY, DESTINATION, APPEND'
;
;  Make sure APPEND is defined.
;
	IF N_ELEMENTS(APPEND) EQ 0 THEN MESSAGE,	$
		'Array to be appended (APPEND) not defined'
;
;  If DESTINATION is not defined, then set it equal to APPEND.
;
	IF N_ELEMENTS(DESTINATION) EQ 0 THEN BEGIN
		DESTINATION = APPEND
		RETURN
	ENDIF
;
;  Get the array types and dimensions of DESTINATION and APPEND.
;
	SD = SIZE(DESTINATION)
	SA = SIZE(APPEND)
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
;  Calculate the number of dimensions in the output array.  If both arrays have
;  the same number of dimensions, then create a new array with an extra
;  dimension of two.  Otherwise, make sure that DESTINATION has more dimensions
;  than APPEND.
;
	IF D_NDIM EQ A_NDIM THEN BEGIN
		R_DIM = [D_DIM > A_DIM, 2]
	END ELSE IF D_NDIM LT A_NDIM THEN BEGIN
		MESSAGE,'APPEND has more dimensions than DESTINATION'
;
;  Otherwise, merge the dimensions of DESTINATION and APPEND, and add one to
;  the final dimension.
;
	END ELSE BEGIN
		R_DIM = D_DIM
		FOR I = 0,A_NDIM-1 DO R_DIM[I] = D_DIM[I] > A_DIM[I]
		R_DIM[D_NDIM-1] = R_DIM[D_NDIM-1] + 1
	ENDELSE
;
;  Create the output array with the correct number of elements, and the greater
;  of the types of DESTINATION and APPEND.
;
	OUTPUT = MAKE_ARRAY(DIMENSION=R_DIM, TYPE=(D_TYPE > A_TYPE))
;
;  Store DESTINATION in the output array.
;
	R_NDIM = N_ELEMENTS(R_DIM)
	CASE R_NDIM OF
		2:  OUTPUT[0,0] = DESTINATION
		3:  OUTPUT[0,0,0] = DESTINATION
		4:  OUTPUT[0,0,0,0] = DESTINATION
		5:  OUTPUT[0,0,0,0,0] = DESTINATION
		6:  OUTPUT[0,0,0,0,0,0] = DESTINATION
		7:  OUTPUT[0,0,0,0,0,0,0] = DESTINATION
	ENDCASE
;
;  Add APPEND at the end.
;
	LAST = R_DIM[R_NDIM-1] - 1
	CASE R_NDIM OF
		2:  OUTPUT[0,LAST] = APPEND
		3:  OUTPUT[0,0,LAST] = APPEND
		4:  OUTPUT[0,0,0,LAST] = APPEND
		5:  OUTPUT[0,0,0,0,LAST] = APPEND
		6:  OUTPUT[0,0,0,0,0,LAST] = APPEND
		7:  OUTPUT[0,0,0,0,0,0,LAST] = APPEND
	ENDCASE
;
;  Replace DESTINATION with OUTPUT, and return.
;
	DESTINATION = OUTPUT
	RETURN
	END
