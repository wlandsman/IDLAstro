;-------------------------------------------------------------
;+
; NAME:
;       YDN2MD
; PURPOSE:
;       Convert from year and day number of year to month and day of month.       
; CALLING SEQUENCE:
;       YDN2MD,yr,dy,m,d
; INPUTS:
;       yr = 4 digit year (like 1988), integer scalar
;       dy = day number in year (like 310), integer scalar or vector
;
; OUTPUTS:
;       m = month number (1-12, e.g. 11 = Nov)   
;       d = day of month (like 5).          
;       Note: On error returns m = d = -1.
;
; EXAMPLE:
;       Find the month/day of days 155 and 255 in the year 2001
;
;       IDL> ydn2md, 2001, [155,255], m, d
;         ==> m = [6,9]   & d = [4,12]        ; = June 4 and September 12 
;       
; MODIFICATION HISTORY:
;       Adapted from Johns Hopkins University/Applied Physics Laboratory
;       Update to use VALUE_LOCATE,   W. Landsman    January 2001  
;-
;-------------------------------------------------------------
 
	PRO YDN2MD,YR,DY,M,D, help=hlp
 
	IF (N_PARAMS() LT 4) or keyword_set(hlp) THEN BEGIN
	  PRINT,' Convert from year and day number of year to month '+$
	    'and day of month.'
	  PRINT,' ydn2md,yr,dy,m,d'
	  PRINT,'   yr = year (like 1988), scalar input'
	  PRINT,'   dy = day number in year (like 310), scalar or vector input'
	  PRINT,'   m = month number (like 11 = Nov).    out'
	  PRINT,'   d = day of month (like 5).           out'
	  PRINT,' Note: On error returns m = d = -1.'
	  RETURN
	ENDIF
 
	; Days before start of each month.
	YDAYS = [0,31,59,90,120,151,181,212,243,273,304,334,366] + 1
 
	LEAP =  (((YR MOD 4) EQ 0) AND ((YR MOD 100) NE 0)) OR $
                ((YR MOD 400) EQ 0) 
                    
        IF LEAP THEN YDAYS[2] = YDAYS[2:*] + 1
        M = VALUE_LOCATE(YDAYS, DY) + 1
	D = DY - YDAYS[M-1] + 1
        BAD = WHERE(M GT 12, NBAD)

        IF NBAD GT 0 THEN BEGIN
            M[BAD] = -1
            D[BAD] = -1
            MESSAGE,'Error in Day Number',/CON
        ENDIF
	RETURN
 
	END
