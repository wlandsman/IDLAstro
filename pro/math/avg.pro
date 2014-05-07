FUNCTION AVG,ARRAY,DIMENSION, NAN = NAN, DOUBLE = DOUBLE
;+
; NAME:
;       AVG
; PURPOSE:
;       Return the average value of an array, or 1 dimension of an array
; EXPLANATION:
;       Calculate the average value of an array, or calculate the average
;       value over one dimension of an array as a function of all the other
;       dimensions.
;
;       In 2009, a DIMENSION keyword was added to the IDL MEAN() function,
;       giving it the same capability as AVG().  Thus, the use of AVG() is now
;       **deprecated** in favor of the MEAN() function.    
; CALLING SEQUENCE:
;       RESULT = AVG( ARRAY, [ DIMENSION, /NAN, /DOUBLE ] )
;
; INPUTS:
;       ARRAY = Input array.  May be any type except string.
;
; OPTIONAL INPUT PARAMETERS:
;       DIMENSION = Optional dimension to do average over, integer scalar
;
; OPTIONAL KEYWORD INPUT:
;      /NAN - Set this keyword to cause the routine to check for occurrences of
;            the IEEE floating-point value NaN in the input data.  Elements with
;            the value NaN are treated as missing data.
;      /DOUBLE - By default, if the input Array is double-precision, complex, 
;                or double complex, the result is of the same type;  64 bit
;                integers are also returned as double.   Otherwise the result
;                the  result is floating-point.   Use of the /DOUBLE keyword 
;                forces a double precision output.   Note that internal 
;                computations are always done in double precision.
; OUTPUTS:
;       The average value of the array when called with one parameter.
;
;       If DIMENSION is passed, then the result is an array with all the
;       dimensions of the input array except for the dimension specified,
;       each element of which is the average of the corresponding vector
;       in the input array.
;
;       For example, if A is an array with dimensions of (3,4,5), then the
;       command B = AVG(A,1) is equivalent to
;
;                       B = FLTARR(3,5)
;                       FOR J = 0,4 DO BEGIN
;                               FOR I = 0,2 DO BEGIN
;                                       B[I,J] = TOTAL( A[I,*,J] ) / 4.
;                               ENDFOR
;                       ENDFOR
;
; RESTRICTIONS:
;       Dimension specified must be valid for the array passed; otherwise the
;       input array is returned as the output array.
; PROCEDURE:
;       AVG(ARRAY) = TOTAL(ARRAY, /DOUBLE)/N_ELEMENTS(ARRAY) when called with 
;       one parameter.
; MODIFICATION HISTORY:
;       William Thompson        Applied Research Corporation
;       July, 1986              8201 Corporate Drive
;                               Landover, MD  20785
;       Converted to Version 2      July, 1990
;       Replace SUM call with TOTAL    W. Landsman    May, 1992
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Added /NAN keyword   W. Landsman      July 2000
;       Accept a scalar input value    W. Landsman/jimm@berkeley   November 2000
;       Internal calculations always in double precision W. Landsman March 2002
;       Return NAN if all values in array are NAN  W. Landsman April 2002
;       Fixed coding bug if all values in array are NAN W. Landsman Jan 2004
;-
 ON_ERROR,2
 S = SIZE(ARRAY,/STR)
 IF S.N_ELEMENTS EQ 1 THEN RETURN, array[0]
 IF S.N_ELEMENTS EQ 0 THEN $
        MESSAGE,'Variable must be an array, name= ARRAY'
;
    IF N_PARAMS() EQ 1 THEN BEGIN
        IF KEYWORD_SET(NAN) THEN NPTS = TOTAL(FINITE(ARRAY) ) $
                            ELSE NPTS = N_ELEMENTS(ARRAY)
        IF NPTS EQ 0 THEN AVERAGE = !VALUES.F_NAN ELSE $
                          AVERAGE = TOTAL(ARRAY, NAN=NAN,/DOUBLE) / NPTS
    ENDIF ELSE BEGIN
        IF ((DIMENSION GE 0) AND (DIMENSION LT S.N_DIMENSIONS)) THEN BEGIN
                AVERAGE = TOTAL(ARRAY,DIMENSION+1,NAN=NAN,/DOUBLE) 
; Install a bug workaround since TOTAL(A,/NAN) returns 0 rather than NAN if 
; all A values are NAN. 
                IF KEYWORD_SET(NAN) THEN BEGIN
                     NPTS = TOTAL(FINITE(ARRAY),DIMENSION+1 ) 
                     BAD = WHERE(NPTS EQ 0, NBAD)
                     AVERAGE = AVERAGE/(NPTS>1)
                     IF NBAD GT 0 THEN AVERAGE[BAD] = !VALUES.D_NAN
                 ENDIF ELSE AVERAGE = AVERAGE/S.DIMENSIONS[DIMENSION]
                   
        END ELSE $
                MESSAGE,'*** Dimension out of range, name= ARRAY'
    ENDELSE

; Convert to floating point unless of type double, complex, or L64, or
; if /DOUBLE is set.

 IF ~KEYWORD_SET(DOUBLE) THEN BEGIN 
    CASE S.TYPE OF
     5: RETURN, AVERAGE
     6: RETURN, COMPLEXARR( FLOAT(AVERAGE), FLOAT(IMAGINARY(AVERAGE)) )
     9: RETURN, AVERAGE
    14: RETURN, AVERAGE
    15: RETURN, AVERAGE
    ELSE: RETURN, FLOAT(AVERAGE)
  ENDCASE
  ENDIF ELSE RETURN, AVERAGE
 END
