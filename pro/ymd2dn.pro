;-------------------------------------------------------------
;+
; NAME:
;       YMD2DN
; PURPOSE:
;       Convert from year and day number of year to month and day of month.
; CALLING SEQUENCE:
;       YMD2DN,yr,m,d,dy
; INPUTS:
;       yr = 4 digit year (like 1988), integer scalar
;       m = month number (1-12, e.g. 11 = Nov)
;       d = day of month (like 5).
;
; OUTPUTS:
;       dy = day number in year (like 310)
;
; MODIFICATION HISTORY:
;       Adapted from ydn2md, Ole Streicher, 2015
;-
;-------------------------------------------------------------

PRO YMD2DN,YR,M,D,DY, help=hlp
  IF (N_PARAMS() LT 4) or keyword_set(hlp) THEN BEGIN
     PRINT,' Convert from year, month and day of month to '+$
           'day of the year.'
     PRINT,' ymd2dn,yr,m,d,dy'
     PRINT,'   yr = year (like 1988), scalar input'
     PRINT,'   m = month number (like 11 = Nov).    input'
     PRINT,'   d = day of month (like 5).           input'
     PRINT,'   dy = day number in year (like 310), out'
     RETURN
  ENDIF

  ; Days before start of each month.
  YDAYS = [0,31,59,90,120,151,181,212,243,273,304,334,366]

  LEAP =  (((YR MOD 4) EQ 0) AND ((YR MOD 100) NE 0)) OR $
          ((YR MOD 400) EQ 0)

  DY = YDAYS[M-1] + (LEAP  AND M GT 2) + D
  RETURN
END
