        FUNCTION WHERENAN, ARRAY, COUNT
;+
; NAME:
;      WHERENAN()
; PURPOSE:
;      Find the indices of all big-endian NaN values in an array.  OBSOLETE
; EXPLANATION: 
;      Find the positions of all values within an array that correspond to the
;      big-endian NaN (not-a-number) special values.    
;
;      THIS PROCEDURE ONLY IDENTIFIES BIG_ENDIAN NaN VALUES.  DO NOT USE IT 
;      TO IDENTIFY NaN VALUES IN GENERAL.   Instead, to identify NaN values on 
;      the host machine use the FINITE() function 
;
;      IDL>     result = where( finite(array,/NAN) )
;       
;      The main purpose of this routine is to catch  NaN special values 
;      written in big_endian format (e.g. FITS data) on a little endian 
;      machine prior to conversion with e.g. IEEE_TO_HOST.    It was needed
;      many years ago because VMS machines could not handle big-endian 
;      special values, but this routine is now kept only for backwards 
;      compatibility.
;
; CALLING SEQUENCE:
;      Result = WHERENAN( ARRAY [, COUNT ] )
;
; INPUT PARAMETERS:
;      ARRAY   = Array to test against the IEEE NaN special values.  Must be
;                of either floating point, double-precision, or complex type.
;
; OUTPUTS:
;      The result of the function is the indices of all values of ARRAY
;      corresponding to the IEEE NaN specification, similar to the IDL WHERE
;      function.
;
; OPTIONAL OUTPUT PARAMETERS:
;      COUNT   = Number of values found corresponding to IEEE NaN.
;
; SIDE EFFECTS:
;      If no NaN values are found, or if ARRAY is not of type float, double
;      precision, or complex, then -1 is returned, and COUNT is set to 0.
;
; RESTRICTIONS:
;      ARRAY must be of type float, double-precision, or complex.
;
; PROCEDURE:
;      The bit patterns of the numbers being tested are compared against the
;      IEEE NaN standard.
;
; MODIFICATION HISTORY:
;      William Thompson, Feb. 1992.
;      William Thompson, Oct. 1992, fixed bug regarding order of bytes on VAX
;              machines.
;      Converted to IDL V5.0   W. Landsman   September 1997
;-
;
        ON_ERROR,2
;
;  Check the number of parameters.
;
        IF N_PARAMS() LT 1 THEN MESSAGE,        $
                'Syntax:  Result = WHERENAN(ARRAY [,COUNT])'
;
;  Parse the input array based on the datatype.
;
        SZ = SIZE(ARRAY)
        CASE SZ[SZ[0]+1] OF
;
;  Single precision floating point.
;
                4:  BEGIN
                        LARRAY = LONG(ARRAY,0,N_ELEMENTS(ARRAY))
                        BYTEORDER,LARRAY,/NTOHL
                        E0 = '7F800000'X
                        E = LARRAY AND E0
                        F = LARRAY AND '7FFFFF'X
                        RESULT = WHERE((E EQ E0) AND (F NE 0), COUNT)
                        END
;
;  Double precision floating point.
;
                5:  BEGIN
                        LARRAY = LONG(ARRAY,0,2,N_ELEMENTS(ARRAY))
                        BYTEORDER,LARRAY,/NTOHL
                        E0 = '7FF00000'X
                        E = LARRAY[0,*] AND E0
                        F1 = LARRAY[0,*] AND 'FFFFF'X
                        RESULT = WHERE((E EQ E0) AND ((F1 NE 0) OR      $
                                (LARRAY[1,*] NE 0)), COUNT)
                        END
;
;  Single precision complex floating point.
;
                6:  BEGIN
                        LARRAY = LONG(ARRAY,0,2,N_ELEMENTS(ARRAY))
                        BYTEORDER,LARRAY,/NTOHL
                        E0 = '7F800000'X
                        E1 = LARRAY[0,*] AND E0
                        E2 = LARRAY[1,*] AND E0
                        F1 = LARRAY[0,*] AND '7FFFFF'X
                        F2 = LARRAY[1,*] AND '7FFFFF'X
                        RESULT = WHERE(((E1 EQ E0) AND (F1 NE 0)) OR    $
                                ((E2 EQ E0) AND (F2 NE 0)), COUNT)
                        END
                ELSE:  BEGIN
                        MESSAGE,'Data type must be floating point',/INFORMATIONAL
                        RESULT = -1
                        COUNT = 0
                        END
        ENDCASE
;
        RETURN, RESULT
        END
