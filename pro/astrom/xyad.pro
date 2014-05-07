pro xyad, hdr, x, y, a, d, PRINT = print, GALACTIC = galactic, ALT = alt, $
         CELESTIAL = celestial, ECLIPTIC = ecliptic, PRECISION = precision
;+
; NAME:
;       XYAD
; PURPOSE:
;       Use a FITS header to convert pixel (X,Y) to world coordinates
; EXPLANATION: 
;       Use astrometry in a FITS image header to compute world
;       coordinates in decimal degrees from X and Y.    
;
;       If spherical coordinates (Calabretta & Greisen 2002, A&A, 395, 1077) are 
;       not present, then XYAD will still perform the transformation specified
;       by the CD, CRVAL, and CRPIX keywords.
; CALLING SEQUENCE:
;       XYAD, HDR               ;Prompt for X and Y positions
;       XYAD, HDR, X, Y, A, D, [ /PRINT, /Galactic, /Celestial, /Ecliptic, 
;                                ALT =, PRECISION=]
; INPUTS:
;       HDR - FITS Image header containing astrometry info
;
; OPTIONAL INPUTS:
;       X     - row position in pixels, scalar or vector
;       Y     - column position in pixels, scalar or vector
;
;       X and Y should be in IDL convention, (first pixel is (0,0) where
;       the integral value corresponds to the center of the pixel.)
;
; OPTIONAL OUTPUT:
;       A - Output longitude in decimal DEGREES (for spherical coordinates), 
;               same number of elements as X and Y.    For celestial 
;               coordinates, this is the Right ascension.
;       D - Output latitude in decimal DEGREES.   For celestial coordinates,
;               this is the declination.
; OPTIONAL KEYWORD INPUT:
;       ALT -  single character 'A' through 'Z' or ' ' specifying an alternate 
;             astrometry system present in the FITS header.    The default is
;             to use the primary astrometry or ALT = ' '.   If /ALT is set, 
;             then this is equivalent to ALT = 'A'.   See Section 3.3 of 
;             Greisen & Calabretta (2002, A&A, 395, 1061) for information about
;             alternate astrometry keywords.
;       PRECISION - Integer scalar (0-4) specifying the number of digits 
;             displayed after the decimal of declination.   The RA is
;             automatically one digit more.   See ADSTRING() for more info.
;             Default value is 1, and the keyword is ignored if results are not 
;             displayed at the terminal 
;       /PRINT - If this keyword is set and non-zero, then results are displayed
;               at the terminal.in both decimal and sexagesimal notation.
;
;       The default for XYAD is to return the coordinate system present in
;       in the FITS header.    However, the following mutually exclusive 
;       keywords can be used to convert to a particular coordinate system:
;
;       /CELESTIAL - Output is Right Ascension and declination
;       /ECLIPTIC - Output is Ecliptic longitude and latitude
;       /GALACTIC - Output is Galactic longitude and latitude
;                   Celestial & Ecliptic coords depend on the reference
;                   equinox, set to either B1950 (=FK4) or J2000 (=FK5,ICRS)
;                   according to the header or standard FITS WCS defaults.
;                   Note that astrometry at the sub-arcsec level requires
;                   fine distinctions that are not handled here.
; 
; OPERATIONAL NOTES:
;       If less than 5 parameters are supplied, or if the /PRINT keyword is
;       set, then the computed astronomical coordinates are displayed at the 
;       terminal.
;
;       If this procedure is to be used repeatedly with the same header,
;       then it would be faster to use XY2AD.
;
; EXAMPLE:
;       A FITS header, hdr, contains astrometric information in celestial
;       coordinates.   Find the RA and Dec corresponding to position X=23.3
;        Y = 100.2 on an image
;        IDL> xyad, hdr, 23.3, 100.2      ;Displays results at the terminal
;       To display the results in Galactic coordinates
;        IDL> xyad, hdr, 23.3, 100.2, /GALACTIC
;
; PROCEDURES CALLED
;       ADSTRING(), EULER, EXTAST, GSSSXYAD, REPCHR(),  XY2AD
;
; REVISION HISTORY:
;       W. Landsman                 STX          Jan, 1988
;       Use astrometry structure  W. Landsman    Jan, 1994
;       Recognize GSSS header  W. Landsman       June, 1994
;       Changed ADSTRING output format   W. Landsman    September 1995
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Use vector call to ADSTRING() W. Landsman February 2000
;       Added ALT input keyword  W. Landsman June 2003
;       Add precision keyword  W. Landsman February 2004
;       Fix display if 'RA','DEC' reversed in CTYPE  W. Landsman Feb. 2004
;       Handle display of NaN values W. Landsman May 2004
;       Work for non-spherical coordinate transformations W. Landsman Oct 2004
;       Fix output display units if ALT keyword used W. Landsman March 2005
;       More informative error message if no astrometry present W.L Nov 2007
;       Fix display when no equinox in header W.L. Dec 2007
;       Fix header display for noncelestial coords W.L. Jan 2008
;       Check for non-standard projections, set FK4 flag. J. P. Leahy Jul 2013
;-
 compile_opt idl2
 On_error,2

 npar = N_params()
 if ( npar EQ 0 ) then begin
        print,'Syntax -  XYAD, hdr, [x, y, a, d, /PRINT, Alt=, Precision=, '
        print,'                      /Galactic, /Celestial, /Ecliptic ]'
        print,'HDR - FITS header (string array) containing astrometry'
        print,'X,Y - Input X and Y positions (scalar or vector)'
        print,'A,D - Output RA and Dec in decimal degrees'
        return
 endif                                                         

  extast, hdr, astr, noparams, ALT = alt       ;Extract astrometry structure

  if ( noparams LT 0 ) then begin
        if alt EQ '' then $
        message,'ERROR - No astrometry info in supplied FITS header' $
	else  message, $
	'ERROR  - No alt=' + alt + ' astrometry info in supplied FITS header'
  endif	
 
  astr2 = TAG_EXIST(astr,'AXES')
  
  if ( npar lt 3 ) then read,'XYAD: Enter X and Y positions: ',x,y

  case strmid(astr.ctype[0],5,3)  of 
        'GSS': gsssxyad, astr, x, y, a, d
         else: xy2ad, x, y, astr, a, d
  endcase
  titname = strmid(astr.ctype,0,4)
  if (titname[0] EQ 'DEC-') or (titname[0] EQ 'ELAT') or $
          (titname[0] EQ 'GLAT') then titname = rotate(titname,2)

  eqnx = get_equinox(hdr,code)  
  IF astr2 THEN FK4 = STRMID(astr.RADECSYS,0,3) EQ 'FK4' ELSE $
     FK4 = eqnx EQ 1950

  if keyword_set(GALACTIC) then begin
      case titname[0] of 
      'RA--': euler, a,d, select=1, FK4=fk4
      'ELON': euler, a,d, select=5, FK4=fk4
      'GLON':
      else: MESSAGE, "doesn't know how to convert from "+titname
      endcase
      titname = ['GLON','GLAT']
  endif else if keyword_set(ECLIPTIC) then begin 
      case titname[0] of 
      'RA--': euler, a, d, select=3, FK4=fk4
      'ELON':
      'GLON': euler, a,d, select=6, FK4=fk4
      else: MESSAGE, "doesn't know how to convert from "+titname
      endcase
      titname = ['ELON','ELAT']
  endif else if keyword_set(CELESTIAL) then begin
      case titname[0] of 
      'RA--':
      'ELON': euler, a, d, select=4, FK4 = FK4
      'GLON': euler, a,d, select=2, FK4 = FK4
      else: MESSAGE, "doesn't know how to convert from "+titname
      endcase
      titname = ['RA--','DEC-']
  endif 

  if (npar lt 5) or keyword_set(PRINT) then begin
        g = where( finite(d) and finite(a), Ng)
	 tit1= titname[0]
	 t1 = strpos(tit1,'-')
	 if t1 gt 0 then tit1 = strmid(tit1,0,t1)
	 tit2= titname[1]
	 t1 = strpos(tit2,'-')
	 if t1 gt 0 then tit2 = strmid(tit2,0,t1)
        npts = N_elements(X)
        spherical = strmid(astr.ctype[0],4,1) EQ '-'
        fmt = '(2F8.2,2x,2F9.4,2x,A)'
        if spherical then begin

        tit = '    X       Y         ' + tit1 + '      ' + tit2 
	sexig = strmid(titname[0],0,4) EQ 'RA--'
	if sexig then begin 
  
 	eqnx = code NE -1 ? '_' + string(eqnx,f='(I4)') :  '    '
	tit = tit +  $
	   '        ' + tit1  + eqnx +  '      ' + tit2 + eqnx
        if N_elements(precision) EQ 0 then precision = 1
        str = replicate('    ---          ---    ', Npts)
        if Ng GT 0 then str[g] = adstring(a[g],d[g],precision)
	endif else str = replicate('', npts)
	print,tit
        for i=0l, npts-1 do $
        print,FORMAT=fmt, float(x[i]), float(y[i]), a[i], d[i], str[i]	
	
        endif else begin
            unit1 = strtrim( sxpar( hdr, 'CUNIT1'+alt,count = N_unit1),2)
            if N_unit1 EQ 0 then unit1 = ''
	    unit2 = strtrim( sxpar( hdr, 'CUNIT2'+alt,count = N_unit2),2)
            if N_unit2 EQ 0 then unit2 = ''
       print,'    X       Y         ' + titname[0] + '     ' + titname[1] 
       if (N_unit1 GT 0) or (N_unit2 GT 0) then $
       print,unit1 ,unit2,f='(t23,a,t33,a)' 	    
        for i=0l, npts-1 do $
       print,FORMAT=fmt, float(x[i]), float(y[i]), a[i], d[i]
        endelse 
   endif
   
   return
   end
