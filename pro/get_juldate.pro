pro get_juldate,jd
;+
; NAME:
;    GET_JULDATE
; PURPOSE:
;     Return the current Julian Date
;
; EXPLANATION:
;     In V5.4, GET_JULDATE became completely obsolete with the introduction
;     of the /UTC keyword to SYSTIME().   So GET_JULDATE,jd is equivalent to
;     jd = SYSTIME(/JULIAN,/UTC).
;
; CALLING SEQUENCE:
;       GET_JULDATE,jd
;
; INPUTS:
;       None
;
; OUTPUTS:
;       jd = Current Julian Date, double precision scalar
;
; EXAMPLE:
;       Return the current hour, day, month and year as integers
;
;       IDL> GET_JULDATE, JD                  ;Get current Julian date
;       IDL> DAYCNV, JD, YR, MON, DAY, HOURS  ;Convert to hour,day month & year
;
; METHOD:
;       A call is made to SYSTIME(/JULIAN,/UTC).
;
; REVISION HISTORY:
;       Written Wayne Landsman                March, 1991
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Assume since V5.4 Use /UTC keyword to SYSTIME()  W. Landsman April 2006
;-
 compile_opt idl2
 if N_Params() LT 1 then begin
     Print,'Syntax - GET_JULDATE, JD'
     return
 endif

     jd = SYSTIME(/JULIAN,/UTC)
 return    
 end
