PRO JDCNV, YR, MN, DAY, HR, JULIAN
;+
; NAME:
;	JDCNV
; PURPOSE:
;	Converts Gregorian dates to Julian days   
;
; EXPLANATION:
;       For IDL versions V5.1 or greater, this procedure is superceded by
;       JULDAY() function in the standard IDL distribution.   Note, however,
;       that prior to V5.1 there wasa bug in JULDAY() that gave answers off
;       by 0.5 days. 
;        
; CALLING SEQUENCE:
;	JDCNV, YR, MN, DAY, HR, JULIAN
;
; INPUTS:
; 	YR = Year, integer scalar or vector
;	MN = Month  integer (1-12) scalar or vector
;	DAY = Day   integer 1-31) scalar or vector 
;	HR  = Hours and fractions of hours of universal time (U.T.), scalar
;              or vector
;		
; OUTPUTS:
;	JULIAN = Julian date (double precision) 
;
; EXAMPLE:
;	To find the Julian Date at 1978 January 1, 0h (U.T.)
;
;	IDL> JDCNV, 1978, 1, 1, 0., JULIAN
;
;	will give JULIAN = 2443509.5
; NOTES:
;	(1) JDCNV will accept vector arguments 
;	(2) JULDATE is an alternate procedure to perform the same function
;
; REVISON HISTORY:
;	Converted to IDL from Don Yeomans Comet Ephemeris Generator,
;	B. Pfarr, STX, 6/15/88
;	Converted to IDL V5.0   W. Landsman   September 1997
;       Added checks on valid month, day ranges W. Landsman July 2008
;-
 On_error,2
 compile_opt idl2

 if N_params() LT 5 then begin
	print,'Syntax -  JDCNV, yr, mn, day, hr, julian'  
	print,'   yr - Input Year (e.g. 1978), scalar or vector'
	print,'   mn - Input Month (1-12), scalar or vector'
	print,'   day - Input Day (1-31), scalar or vector'
	print,'   hr - Input Hour (0-24), scalar or vector'
	print,'   julian - output Julian date'
        return
 endif
 if max(mn) GT 12 then message,/con,  $
     'Warning - Month number outside of expected range [1-12] '
 if max(day) GT 31 then message,/con, $
     'Warning - Day number outside of expected range [1-31] '

 yr = long(yr) & mn = long(mn) &  day = long(day)	;Make sure integral
 L = (mn-14)/12		;In leap years, -1 for Jan, Feb, else 0
 julian = day - 32075l + 1461l*(yr+4800l+L)/4 + $
         367l*(mn - 2-L*12)/12 - 3*((yr+4900l+L)/100)/4
 julian = double(julian) + (HR/24.0D) - 0.5D

 return
 end
