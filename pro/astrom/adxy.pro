pro adxy, hdr, a, d, x, y, PRINT = print, ALT = alt        ;Ra, Dec to X,Y
;+
; NAME:
;       ADXY
; PURPOSE:
;       Use a FITS header to convert astronomical to pixel coordinates
; EXPLANATION:
;       Use an image header to compute X and Y positions, given the
;       RA and Dec (or longitude, latitude) in decimal degrees.  
;
; CALLING SEQUENCE:
;       ADXY, HDR               ;Prompt for Ra and DEC 
;       ADXY, hdr, a, d, x, y, [ /PRINT, ALT= ]
;
; INPUTS:
;       HDR - FITS Image header containing astrometry parameters
;
; OPTIONAL INPUTS:
;       A - Right ascension in decimal DEGREES, scalar or vector
;       D - Declination in decimal DEGREES, scalar or vector        
;
;       If A and D are not supplied, user will be prompted to supply
;       them in either decimal degrees or HR,MIN,SEC,DEG,MN,SC format.
;
; OPTIONAL OUTPUT:
;       X     - row position in pixels, same number of elements as A and D
;       Y     - column position in pixels
;
;       X and Y will be in standard IDL convention (first pixel is 0) and not
;       the FITS convention (first pixel is 1).      As in FITS an integral
;       value corresponds to the center of a pixel.
; OPTIONAL KEYWORD INPUT:
;       /PRINT - If this keyword is set and non-zero, then results are displayed
;               at the terminal.
;       ALT -  single character 'A' through 'Z' or ' ' specifying an alternate 
;             astrometry system present in the FITS header.    The default is
;             to use the primary astrometry or ALT = ' '.   If /ALT is set, 
;             then this is equivalent to ALT = 'A'.   See Section 3.3 of 
;             Greisen & Calabretta (2002, A&A, 395, 1061) for information about
;             alternate astrometry keywords.
;
; OPERATIONAL NOTES:
;       If less than 5 parameters are supplied, or if the /PRINT keyword is
;       set, then the X and Y positions are displayed at the terminal.
;
;       If the procedure is to be used repeatedly with the same header,
;       then it would be faster to use AD2XY.
;
; PROCEDURES CALLED:
;       AD2XY, ADSTRING(), EXTAST, GETOPT(), TEN()
;
; REVISION HISTORY:
;       W. Landsman                 HSTX          January, 1988
;       Use astrometry structure   W. Landsman   January, 1994  
;       Changed default ADSTRING format   W. Landsman    September, 1995
;       Check if latitude/longitude reversed in CTYPE keyword W. L. Feb. 2004
;       Added ALT keyword   W. Landsman   September 2004
;       Work for non-spherical coordinate transformation W. Landsman May 2005 
;       More informative error message if astrometry missing W.L. Feb 2008
;       Cosmetic updates W.L. July 2011
;       Use version 2 astrometry structure J. P. Leahy July 2013       
;-
 Compile_opt idl2
 On_error,2

 npar = N_params()

 if ( npar EQ 0 ) then begin
        print,'Syntax - ADXY, hdr, [a, d, x, y, /PRINT, ALT= ]'
        print,'If supplied, A and D must be in decimal DEGREES'
        return
 endif                                                                  
 
 extast, hdr, astr, noparams, ALT = alt   ;Extract astrometry from FITS header
  if ( noparams LT 0 ) then begin
        if alt EQ '' then $
        message,'ERROR - No astrometry info in supplied FITS header' $
	else  message, $
	'ERROR  - No alt=' + alt + ' astrometry info in supplied FITS header'
  endif	

 astr2 = TAG_EXIST(astr,'AXES') ; Version 2 structure
 
 if npar lt 3 then begin
   RD: print,'Coordinates must be entered in either decimal (2 parameter) ' 
   print,'  or sexagesimal (6 parameter) format'
   inp = ''
   read,'ADXY: Enter coordinates: ',inp
   radec = getopt(inp,'F')
   case N_elements(radec) of 
      2: begin 
         a = radec[0] & d = radec[1]
         end
      6: begin
         a = ten(radec[0:2]*15.) & d = ten(radec[3:5])
         end
   else: begin
         print,'ADXY: ERROR - Either 2 or 6 parameters must be entered'
         return
         end
   endcase 
 endif

 case strmid( astr.ctype[0], 5,3) of
 'GSS': gsssadxy, astr, a, d, x, y       ;HST Guide star astrometry
 else:  ad2xy, a, d, astr, x, y          ;All other cases
 endcase

 if (npar lt 5) || keyword_set( PRINT ) then begin
        npts = N_elements(a)
        tit = strmid(astr.ctype,0,4)
         spherical = strmid(astr.ctype[0],4,1) EQ '-'
	if spherical then begin
        fmt = '(2F9.4,A,2X,2F8.2)'
        str = adstring(a,d,1)
        tit = strmid(astr.ctype,0,4)
        tit = repchr(tit,'-',' ')
        flip = astr2 ? astr.reverse : $
        (tit[0] EQ 'DEC ') || (tit[0] EQ 'ELAT') || (tit[0] EQ 'GLAT') 
        if flip then tit = rotate(tit,2)
        print,'    ' + tit[0] + '    ' + tit[1] + '       ' + tit[0]  + $
              '         ' + tit[1]  + '        X       Y'
        for i = 0l, npts-1 do $
        print,FORMAT = fmt, a[i], d[i], str[i], x[i], y[i] 
        endif else begin
	 unit1 = strtrim( sxpar( hdr, 'CUNIT1'+alt,count = N_unit1),2)
	 if N_unit1 EQ 0 then unit1 = ''
	 unit2 = strtrim( sxpar( hdr, 'CUNIT2'+alt,count = N_unit2),2)
	 if N_unit2 EQ 0 then unit2 = ''
	 print,'   ' + tit[0] + '     ' + tit[1] + '         X       Y'
	 if (N_unit1 GT 0) || (N_unit2 GT 0) then $
	     print,unit1 ,unit2,f='(t5,a,t14,a)'
	     for i=0l, npts-1 do $
	 print, a[i], d[i], x[i], y[i], f='(2F9.4,2X,2F8.2)'
       endelse
  endif
 
 return
 end
