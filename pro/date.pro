FUNCTION DATE,YEAR,DAY
;+
; NAME:
;	DATE
; PURPOSE:
;	Convert day-of-year to a DD-MMM-YYYY string
;
; CALLING SEQUENCE:
;	D_String = DATE(Year, day )
;
; INPUTS:
;	Year - Integer scalar specifying the year.   If the year contains only
;		two digits, then it is assumed to indicate the number of 
;		years after 1900. 
;
;	Day - Integer scalar giving number of days after Jan 0 of the 
;		specified year.    Can be larger than 366     
;
; OUTPUTS:
;	D_String - String giving date in format '13-MAR-1986'
;
; RESTRICTIONS:
;	Will not work for years before 100 AD 
; EXAMPLE:
;	IDL> print, date(1997,279)
;		'6-Oct-1997'
;
; MODIFICATION HISTORY:
;       D.M. fecit  24 October,1983
;	Work for years outside of the 19th century  W. Landsman  September 1997
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
 IF day LE 0 THEN BEGIN
	D_String = '%DATE-F-DAY.LE.ZERO'
 ENDIF ELSE BEGIN
	Last_Day = [31,59,90,120,151,181,212,243,273,304,334,365]
	LD = [0,INTARR(11)+1]
	Day_of_Year = Day
	Months = 'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'

; Every year that is exactly divisible by 4 is a leap year, except for years
; that exactly divisible by 100; these centurial years are leap years only if
; they are exactly divisible by 400.

	IF Year LT 100 THEN Yr = Year + 1900 ELSE Yr = Year
	Leap = (((Yr MOD 4) EQ 0) AND ((Yr MOD 100) NE 0)) $
		OR ((Yr MOD 400) EQ 0)
	N_Days = 365 + Leap

	WHILE Day_of_Year GT N_Days DO BEGIN
		Day_of_Year = Day_of_Year - N_Days
		Yr = Yr + 1
		Leap = (((Yr MOD 4) EQ 0) AND ((Yr MOD 100) NE 0)) $
			OR ((Yr MOD 400) EQ 0)
		N_Days = 365 + Leap
	END

	End_Date = '-' + STRTRIM(YR,2)

	IF Leap THEN Last_Day = Last_Day + LD
	Last_Month = Day_of_Year LE Last_Day
	Where_LD = WHERE(Last_Month, N_Month)

	IF N_Month EQ 12 THEN BEGIN
		D_String = STRTRIM(Day_of_Year,2) + '-JAN' + End_Date
	ENDIF ELSE BEGIN
		LAST_Month = Where_LD[0]
		Month = STRMID(Months,3*Last_Month,3)
		Day_of_Month = Day_of_Year - Last_Day[Last_Month-1]
		D_String = STRTRIM(Day_of_Month,2) + '-' + Month + End_Date
	END
 END

 RETURN,D_String
 END
