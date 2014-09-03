PRO ZENPOS, date, ra, dec
;+
; NAME:
;       ZENPOS
; PURPOSE:
;       Return the zenith RA and Dec in radians for a given Julian date.
;
; CALLING SEQUENCE:
;       ZENPOS, Date, Ra, Dec
;
; INPUT:
;       Date  The Julian date, in double precision, of the date and time
;               for which the zenith position is desired, scalar or vector.
;
; OUTPUTS:
;       Ra    The right ascension in RADIANS of the zenith.
;       Dec   The declination in RADIANS of the zenith.
;
; PROCEDURE:
;       The local sidereal time is computed; this is the RA of the zenith.
;       It and the observatories latitude (corresponding to the Dec.) are
;       converted to radians and returned as the zenith direction.
;
; PROMPTS:
;       ZENPOS will prompt for the following 3 parameters if they are not
;       defined in the common block SITE (see below)
;
;       LAT,LNG - north latitude and east longitude of the desired location 
;               in DEGREES
;       TZONE - Time Zone (in hours) of the desired location (e.g. 4 = EDT,
;               5 = EST)
;
; COMMON BLOCKS:
;       SITE - This common block should contain the three scalars LAT, LNG
;               and TZONE
;
; PROCEDURE CALLS:
;       CT2LST - Convert to Local Mean Sidereal Time
; MODIFICATION HISTORY:
;       Written by Michael R. Greason, STX, 14 October 1988.
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Update documentation, longitude now *east* of Greenwich W.L. July 2000
;-
 COMMON SITE, lat, lng, tzone

 if N_params() EQ 0 then begin
     print,'Syntax - zenpos, dte, ra, dec'
     print,'         dte = Julian Date, Ouput Ra and Dec in radians'
     return
 endif

 if N_elements(lat) eq 0 then read, $
       'Enter latitude and longitude (in degrees): ',lat,lng
 if N_elements(tzone) eq 0 then read, $
       'Enter time zone (in hours): ',tzone
;
;                            Define the needed conversion factors.
;
 d2rad = !DPI / 180.D0
 h2rad = !DPI / 12.D0
;
;                            Get the sidereal time corresponding to the 
;                            supplied date.
;
 ct2lst, lst, lng, tzone, date
;
;                            Compute the RA and Dec.
;
 ra = lst * h2rad
 dec = ra*0. + lat * d2rad
;
 RETURN
 END
