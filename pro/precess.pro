pro precess, ra, dec, equinox1, equinox2, PRINT = print, FK4 = FK4, $
        RADIAN=radian
;+
; NAME:
;      PRECESS
; PURPOSE:
;      Precess coordinates from EQUINOX1 to EQUINOX2.  
; EXPLANATION:
;      For interactive display, one can use the procedure ASTRO which calls 
;      PRECESS or use the /PRINT keyword.   The default (RA,DEC) system is 
;      FK5 based on epoch J2000.0 but FK4 based on B1950.0 is available via 
;      the /FK4 keyword.
;
;      Use BPRECESS and JPRECESS to convert between FK4 and FK5 systems
; CALLING SEQUENCE:
;      PRECESS, ra, dec, [ equinox1, equinox2, /PRINT, /FK4, /RADIAN ]
;
; INPUT - OUTPUT:
;      RA - Input right ascension (scalar or vector) in DEGREES, unless the 
;              /RADIAN keyword is set
;      DEC - Input declination in DEGREES (scalar or vector), unless the 
;              /RADIAN keyword is set
;              
;      The input RA and DEC are modified by PRECESS to give the 
;      values after precession.
;
; OPTIONAL INPUTS:
;      EQUINOX1 - Original equinox of coordinates, numeric scalar.  If 
;               omitted, then PRECESS will query for EQUINOX1 and EQUINOX2.
;      EQUINOX2 - Equinox of precessed coordinates.
;
; OPTIONAL INPUT KEYWORDS:
;      /PRINT - If this keyword is set and non-zero, then the precessed
;               coordinates are displayed at the terminal.    Cannot be used
;               with the /RADIAN keyword
;      /FK4   - If this keyword is set and non-zero, the FK4 (B1950.0) system
;               will be used otherwise FK5 (J2000.0) will be used instead.
;      /RADIAN - If this keyword is set and non-zero, then the input and 
;               output RA and DEC vectors are in radians rather than degrees
;
; RESTRICTIONS:
;       Accuracy of precession decreases for declination values near 90 
;       degrees.  PRECESS should not be used more than 2.5 centuries from
;       2000 on the FK5 system (1950.0 on the FK4 system).
;
; EXAMPLES:
;       (1) The Pole Star has J2000.0 coordinates (2h, 31m, 46.3s, 
;               89d 15' 50.6"); compute its coordinates at J1985.0
;
;       IDL> precess, ten(2,31,46.3)*15, ten(89,15,50.6), 2000, 1985, /PRINT
;
;               ====> 2h 16m 22.73s, 89d 11' 47.3"
;
;       (2) Precess the B1950 coordinates of Eps Ind (RA = 21h 59m,33.053s,
;       DEC = (-56d, 59', 33.053") to equinox B1975.
;
;       IDL> ra = ten(21, 59, 33.053)*15
;       IDL> dec = ten(-56, 59, 33.053)
;       IDL> precess, ra, dec ,1950, 1975, /fk4
;
; PROCEDURE:
;       Algorithm from Computational Spherical Astronomy by Taff (1983), 
;       p. 24. (FK4). FK5 constants from "Astronomical Almanac Explanatory
;       Supplement 1992, page 104 Table 3.211.1.
;
; PROCEDURE CALLED:
;       Function PREMAT - computes precession matrix 
;
; REVISION HISTORY
;       Written, Wayne Landsman, STI Corporation  August 1986
;       Correct negative output RA values   February 1989
;       Added /PRINT keyword      W. Landsman   November, 1991
;       Provided FK5 (J2000.0)  I. Freedman   January 1994
;       Precession Matrix computation now in PREMAT   W. Landsman June 1994
;       Added /RADIAN keyword                         W. Landsman June 1997
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Correct negative output RA values when /RADIAN used    March 1999 
;       Work for arrays, not just vectors  W. Landsman    September 2003 
;-    
  On_error,2                                           ;Return to caller

  npar = N_params()
  deg_to_rad = !DPI/180.0D0

   if ( npar LT 2 ) then begin 

     print,'Syntax - PRECESS, ra, dec, [ equinox1, equinox2,' + $ 
                ' /PRINT, /FK4, /RADIAN ]'
     print,'         NOTE: RA and DEC must be in DEGREES unless /RADIAN is set'
     return 

  endif else if (npar LT 4) then $
      read,'Enter original and new equinox of coordinates: ',equinox1,equinox2 

  npts = min( [N_elements(ra), N_elements(dec)] )
  if npts EQ 0 then $  
       message,'ERROR - Input RA and DEC must be vectors or scalars'
  array  = size(ra,/N_dimen) GE 2
  if array then dimen = size(ra,/dimen)

  if ~keyword_set( RADIAN) then begin
          ra_rad = ra*deg_to_rad     ;Convert to double precision if not already
          dec_rad = dec*deg_to_rad 
  endif else begin
        ra_rad= double(ra) & dec_rad = double(dec)
  endelse

  a = cos( dec_rad )  

 CASE npts of                    ;Is RA a vector or scalar?

   1:    x = [a*cos(ra_rad), a*sin(ra_rad), sin(dec_rad)] ;input direction 

   else: begin          

         x = dblarr(npts,3)
         x[0,0] = reform(a*cos(ra_rad),npts,/over)
         x[0,1] = reform(a*sin(ra_rad),npts,/over)
         x[0,2] = reform(sin(dec_rad),npts,/over)
         x = transpose(x)
         end

   ENDCASE  

   sec_to_rad = deg_to_rad/3600.d0

; Use PREMAT function to get precession matrix from Equinox1 to Equinox2

  r = premat(equinox1, equinox2, FK4 = fk4)

  x2 = r#x      ;rotate to get output direction cosines

 if npts EQ 1 then begin                 ;Scalar

        ra_rad = atan(x2[1],x2[0])
        dec_rad = asin(x2[2])

 endif else begin                ;Vector     

        ra_rad = dblarr(npts) + atan(x2[1,*],x2[0,*])
        dec_rad = dblarr(npts) + asin(x2[2,*])

 endelse

  if ~keyword_set(RADIAN) then begin
        ra = ra_rad/deg_to_rad
        ra = ra + (ra LT 0.)*360.D            ;RA between 0 and 360 degrees
        dec = dec_rad/deg_to_rad
  endif else begin
        ra = ra_rad & dec = dec_rad
        ra = ra + (ra LT 0.)*2.0d*!DPI
  endelse

  if array then begin
       ra = reform(ra, dimen , /over)
       dec = reform(dec, dimen, /over)
  endif

  if keyword_set( PRINT ) then $
      print, 'Equinox (' + strtrim(equinox2,2) + '): ',adstring(ra,dec,1)

  return
  end
