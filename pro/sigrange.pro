	FUNCTION SIGRANGE,ARRAY,FRACTION=FRACTION,MISSING=MISSING,RANGE=RANGE
;+
; NAME: 
;	SIGRANGE()
; PURPOSE: 
;	Selects the most significant data range in an image.
; EXPLANATION: 
;	Selects out the most significant range in the data to be used in 
;	displaying images.  The histogram of ARRAY is used to select the most
;	significant range.      Useful for scaling an image display.
; CALLING SEQUENCE: 
;	OUTPUT = SIGRANGE( ARRAY )
; INPUTS: 
;	ARRAY	 = Array to take most significant range of.
; OPTIONAL INPUTS: 
;	None.
; OUTPUTS: 
;	The function returns an array where values above and below the
;	selected range are set equal to the maximum and minimum of the
;	range respectively.
; OPTIONAL INPUT KEYWORDS: 
;	FRACTION = Fraction of data to consider most significant.
;		   Defaults to 0.99
;	MISSING	 = Value used to flag missing points.  Data points with this
;		   value are not considered or changed.
; OPTIONAL OUTPUT KEYWORD
;	RANGE    = 2 element vector, giving the range (minimum and maxmimum) 
;		used
;
; NOTES:
;       If the image array contains more than 10,000 points then SIGRANGE() 
;       uses random indexing of a subset of the points to determine the range
;       (for speed).    Thus identical calls to SIGRANGE() might not yield
;       identical results (although they should be very close).     
; RESTRICTIONS: 
;	ARRAY must have more than two points.  Fraction must be greater than 0 
;	and less than 1.
;
;	SIGRANGE was originally part of the SERTS image display package.   
;	Other routines from this package are available at 
;
;	http://sohowww.nascom.nasa.gov/solarsoft/gen/idl/image/
;
;	Note that this version of SIGRANGE does not include the non-standard 
;	system variables used in the SERTS package.
; REVISION HISTORY: 
;	Version 1, William Thompson, GSFC, 12 May 1993.
;		Incorporated into CDS library.
;	Version 2, William Thompson, GSFC, 25 May 1993.
;		Changed call to HISTOGRAM to be compatible with OpenVMS/ALPHA
;       Version 3, CDP, RAL, Add RANGE keyword.  16-Apr-96
;	Version 4, William Thompson, GSFC, 17 April 1996
;		Corrected some problems when range is too high.
;	Version 5, 13-Jan-1998, William Thompson, GSFC
;		Use random numbers to improve statistics when only using a
;		fraction of the array.
;	Version 6, 06-Mar-1998, William Thompson, GSFC
;		Change default to 0.99
;-
;
	IF N_ELEMENTS(FRACTION) NE 1 THEN FRACTION = 0.99
	IF N_ELEMENTS(ARRAY) LE 2 THEN BEGIN
	    MESSAGE, /CONTINUE, 'Not enough points to form histogram'
	    RETURN, ARRAY
	END ELSE IF (FRACTION LE 0) OR (FRACTION GE 1) THEN BEGIN
	    MESSAGE, /CONTINUE, 'Fraction must be GT 0 and LT 1'
	    RETURN, ARRAY
	ENDIF
;
;  To speed up the process, work on a reduced version of ARRAY.
;
	IF N_ELEMENTS(ARRAY) LT 10000 THEN ATEMP0 = ARRAY ELSE BEGIN
	    NN = 1000 > (N_ELEMENTS(ARRAY) / 25) < 100000
	    ATEMP0 = ARRAY[N_ELEMENTS(ARRAY)*RANDOMU(SEED,NN)]
	ENDELSE
;
;  Get the total range of the data, excluding any missing points.
;
        IF N_ELEMENTS(MISSING) EQ 1 THEN BEGIN
                W = WHERE(ATEMP0 NE MISSING, COUNT)
                IF COUNT GT 0 THEN ATEMP0 = ATEMP0(W)
	ENDIF 
	N_TOTAL = N_ELEMENTS(ATEMP0)
	AMAX = 1.*MAX(ATEMP0)
	AMIN = 1.*MIN(ATEMP0)
	IF AMIN EQ AMAX THEN GOTO, EXIT_POINT
;
;  Set up some initial parameters for the reiteration.
;
	ATEMP = ATEMP0
	DELTA = 0
;
;  Form the histogram, and calculate an array expressing the fraction of points
;  that fall within or below the given bin.
;
FIND_RANGE:
	LAST_DELTA = DELTA
	X = AMIN  +  FINDGEN(1001) * (AMAX - AMIN) / 1000.
	H = HISTOGRAM(LONG((ATEMP-AMIN)*1000./(AMAX - AMIN)))
	FOR I = 1,N_ELEMENTS(H)-1 DO H[I] = H[I] + H[I-1]
	H = H / FLOAT(N_TOTAL)
;
;  Estimate the endpoints corresponding to the specified range, and calculate
;  the values at these endpoints.  Limit the array to be within these values.
;
	IMIN = (MIN( WHERE( H GT ((1. - FRACTION) / 2.) )) - 1) > 0
	IMAX =  MIN( WHERE( H GT ((1. + FRACTION) / 2.) ))
	IF IMAX LT 0 THEN IMAX = 1000
	AMIN = X[IMIN]
	AMAX = X[IMAX]
;
;  If the calculated range is zero, then use 2% of the full range of the data.
;
	IF AMAX EQ AMIN THEN BEGIN
		BMAX = MAX(ATEMP0, MIN=BMIN)
		AMAX = MAX(ATEMP0(WHERE(ATEMP0 LE (AMAX + 0.01*(BMAX-BMIN)))))
		AMIN = MIN(ATEMP0(WHERE(ATEMP0 GE (AMIN - 0.01*(BMAX-BMIN)))))
	ENDIF
;
;  If the range calculated has changed by more than 5% from the last iteration,
;  the reiterate.
;
	ATEMP = AMIN > ATEMP0 < AMAX
	DELTA = AMAX - AMIN
	RATIO = (DELTA - LAST_DELTA) / (DELTA + LAST_DELTA)
	IF ABS(RATIO) GT 0.05 THEN GOTO, FIND_RANGE
;
;  If a missing pixel flag value was passed, then reset those points to the
;  flag value.  Return the adjusted array.
;
EXIT_POINT:
	ATEMP = AMIN > ARRAY < AMAX
	IF N_ELEMENTS(MISSING) EQ 1 THEN BEGIN
		WW = WHERE(ARRAY EQ MISSING,N_MISSING)
		IF N_MISSING GT 0 THEN ATEMP[WW] = MISSING
	ENDIF
        RANGE = [AMIN,AMAX]
	RETURN, ATEMP
	END
