PRO ZENPOS, date, ra, dec, latitude=latitude, longitude=longitude, degrees = degrees
;+
; NAME:
;       ZENPOS
; PURPOSE:
;       Return the zenith RA and Dec for a given Julian date and observer longitude
;               and latitude
;
; CALLING SEQUENCE:
;       ZENPOS, Date, Ra, Dec, [ Latitude= , Longitude = , /Degrees ]
;
; INPUT:
;       Date  The Julian date, in double precision, of the date and time
;               for which the zenith position is desired, scalar or vector.
;
; OPTIONAL INPUT KEYWORDS:
;       LATITUDE  -  Latitude of the desired location (degrees)
;       LONGITUDE -  Longitude of the desired location (degrees)
;
; OUTPUTS:
;       RA - The right ascension of the zenith (in radians unless /Degrees is set)
;       Dec - The declination of the zenith (in radians unless /Degrees is set)
;
;       RA and Dec will have same number of elements as the input Julian date.
;
; PROCEDURE:
;       The local sidereal time is computed; this is the RA of the zenith.
;       It and the observatories latitude (corresponding to the Dec.) are
;       converted to radians and returned as the zenith direction.
;
; PROMPTS:
;       ZENPOS will prompt for the following 2 parameters if they are not
;       defined in the common block SITE (see below) or provided as
;       keywords.  Keywords override information in the common block.
;
;       LAT,LNG - north latitude and east longitude of the desired location 
;               in DEGREES
;
; COMMON BLOCKS:
;       SITE - This common block should contain the scalars LAT, LNG
;
; PROCEDURE CALLS:
;       CT2LST - Convert to Local Mean Sidereal Time
; MODIFICATION HISTORY:
;       Written by Michael R. Greason, STX, 14 October 1988.
;       Update documentation, longitude now *east* of Greenwich W.L. July 2000
;       Add keywords for non-interactive use  J. Sapp   July 2016
;       Remove unneeded time zone info, add /Degrees  W. Landsman Mar 2018
;-
 COMMON SITE, lat, lng

 if N_params() EQ 0 then begin
     print,'Syntax - zenpos, dte, ra, dec, [latitude = , longitude =, /Degrees]'
     print,'         dte = Julian Date
     print,'         Ouput Ra, Dec in radians (unless /Degrees is set)'
     return
 endif

 ; Override common block parameters with keywords
 if N_elements(latitude) ne 0 then lat = latitude
 if N_elements(longitude) ne 0 then lng = longitude

 if N_elements(lat) eq 0 then read, $
       'Enter latitude and longitude (in degrees): ',lat,lng
;
;                            Define the needed conversion factors.
;
 d2rad = !DPI / 180.D0
 h2rad = !DPI / 12.D0
;
;                Get the sidereal time corresponding to the supplied date.
;
 ct2lst, lst, lng, tzone, date  ;tzone dummy parameter since we give Julian date
;
;                            Compute the RA and Dec.
;
 if keyword_set(degrees) then begin
    ra = lst*15
    dec = ra*0.0d + lat
 endif else begin   
 	ra = lst * h2rad
 	dec = ra*0.0d + lat * d2rad
 endelse
;
 RETURN
 END
