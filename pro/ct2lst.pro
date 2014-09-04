PRO CT2LST, lst, lng, tz, tme, day, mon, year
;+
; NAME:
;     CT2LST
; PURPOSE:
;     To convert from Local Civil Time to Local Mean Sidereal Time.
;
; CALLING SEQUENCE:
;     CT2LST, Lst, Lng, Tz, Time, [Day, Mon, Year] 
;                       or
;     CT2LST, Lst, Lng, dummy, JD
;
; INPUTS:
;     Lng  - The longitude in degrees (east of Greenwich) of the place for 
;            which the local sidereal time is desired, scalar.   The Greenwich 
;            mean sidereal time (GMST) can be found by setting Lng = 0.
;     Tz  - The time zone of the site in hours, positive East  of the Greenwich
;           meridian (ahead of GMT).  Use this parameter to easily account 
;           for Daylight Savings time (e.g. -4=EDT, -5 = EST/CDT), scalar
;           This parameter is not needed (and ignored) if Julian date is 
;           supplied.    ***Note that the sign of TZ was changed in July 2008
;           to match the standard definition.*** 
;     Time or JD  - If more than four parameters are specified, then this is 
;               the time of day of the specified date in decimal hours.  If 
;               exactly four parameters are specified, then this is the 
;               Julian date of time in question, scalar or vector
;
; OPTIONAL INPUTS:
;      Day -  The day of the month (1-31),integer scalar or vector
;      Mon -  The month, in numerical format (1-12), integer scalar or vector
;      Year - The 4 digit year (e.g. 2008), integer scalar or vector
;
; OUTPUTS:
;       Lst   The Local Sidereal Time for the date/time specified in hours.
;
; RESTRICTIONS:
;       If specified, the date should be in numerical form.  The year should
;       appear as yyyy.
;
; PROCEDURE:
;       The Julian date of the day and time is question is used to determine
;       the number of days to have passed since 0 Jan 2000.  This is used
;       in conjunction with the GST of that date to extrapolate to the current
;       GST; this is then used to get the LST.    See Astronomical Algorithms
;       by Jean Meeus, p. 84 (Eq. 11-4) for the constants used.
;
; EXAMPLE:
;       Find the Greenwich mean sidereal time (GMST) on 2008 Jul 30 at 15:53 pm
;       in Baltimore, Maryland (longitude=-76.72 degrees).   The timezone is 
;       EDT or tz=-4
;
;       IDL> CT2LST, lst, -76.72, -4,ten(15,53), 30, 07, 2008
;
;               ==> lst =  11.356505  hours  (= 11h 21m 23.418s)
;
;       The Web site  http://tycho.usno.navy.mil/sidereal.html contains more
;       info on sidereal time, as well as an interactive calculator.
; PROCEDURES USED:
;       jdcnv - Convert from year, month, day, hour to julian date
;
; MODIFICATION HISTORY:
;     Adapted from the FORTRAN program GETSD by Michael R. Greason, STX, 
;               27 October 1988.
;     Use IAU 1984 constants Wayne Landsman, HSTX, April 1995, results 
;               differ by about 0.1 seconds  
;     Longitudes measured *east* of Greenwich   W. Landsman    December 1998
;     Time zone now measure positive East of Greenwich W. Landsman July 2008
;     Remove debugging print statement  W. Landsman April 2009
;-
 On_error,2
 compile_opt idl2

 if N_params() LT 3 THEN BEGIN
        print,'Syntax - CT2LST, Lst, Lng, Tz, Time, Day, Mon, Year' 
        print,'                 or'
        print,'         CT2LST, Lst, Lng, Tz, JD'
        return
 endif
;                            If all parameters were given, then compute
;                            the Julian date; otherwise assume it is stored
;                            in Time.
;

 IF N_params() gt 4 THEN BEGIN
   time = tme - tz
   jdcnv, year, mon, day, time, jd 

 ENDIF ELSE jd = double(tme)
;
;                            Useful constants, see Meeus, p.84
;
 c = [280.46061837d0, 360.98564736629d0, 0.000387933d0, 38710000.0 ]
 jd2000 = 2451545.0D0
 t0 = jd - jd2000
 t = t0/36525
;
;                            Compute GST in seconds.
;
 theta = c[0] + (c[1] * t0) + t^2*(c[2] - t/ c[3] )
;
;                            Compute LST in hours.
;
 lst = ( theta + double(lng))/15.0d
 neg = where(lst lt 0.0D0, n)
 if n gt 0 then lst[neg] = 24.D0 + (lst[neg] mod 24)
 lst = lst mod 24.D0
;   
 RETURN
 END
