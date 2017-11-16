pro fits_xyad, filename_or_fcb, x, y, a, d, PRINT = print, GALACTIC = galactic, ALT = alt, $
         CELESTIAL = celestial, ECLIPTIC = ecliptic, PRECISION = precision, $
         exten_no= exten_no, extver= extver, extname = extname,extlevel=extlevel, $
         nodistort = nodistort, nosip = nosip
;+
; NAME:
;       FITS_XYAD
; PURPOSE:
;       Use a FITS file with astrometry to convert pixel (X,Y) to world coordinates
; EXPLANATION: 
;       Use astrometry in a FITS file to compute world coordinates in decimal degrees from
;       X and Y pixel coordinates.
;
;       This routine can be used with any FITS file containing WCS (world coordinate
;       system) information.    But it is especially useful for files using the
;		distortion lookup tables (https://fits.gsfc.nasa.gov/wcs/dcs_20040422.pdf)
;		which are stored in separate extensions in the FITS file.    An example of such 
;       files are the Wide Field Camera 3 (WFC3) _flt files from Hubble 
;
;       If spherical coordinates (Calabretta & Greisen 2002, A&A, 395, 1077) are 
;       not present, then FITS_XYAD will still perform the transformation specified
;       by the CD, CRVAL, and CRPIX keywords.
; CALLING SEQUENCE:
;       FITS_XYAD, filename_or_fcb              ;Prompt for X and Y positions
;       FITS_XYAD, filename_or_fcb, X, Y, A, D, [ /PRINT, /Galactic, /Celestial, /Ecliptic, 
;                                ALT =, PRECISION=]
; INPUTS:
;       FILENAME_or_FCB - either the FITS Control Block (FCB) structure returned by FITS_OPEN
;			or a scalar string giving the name of the FITS file.    If FITS_XYAD is to be
;			used repeatedly with the same file, then it is more efficient to first obtain a 
;			FCB structure with FITS_OPEN, and then pass this structure to FITS_XYAD.
;
; OPTIONAL INPUTS:
;       X     - row position in pixels, scalar or vector
;       Y     - column position in pixels, scalar or vector
;
;       X and Y should be in IDL convention, (first pixel is [0,0] where
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
;		EXTEN_NO - Extension number of FITS file containing the astrometric
;			information.   By default, the first extension is used, if present,
;			otherwise the primary header (EXTEN_NO=0) is used.
;       EXTNAME - string name of the extname to read
;       EXTVER - integer version number to read
;       EXTLEVEL - integer extension level to read
;       /NODISTORT - If set, then do not apply the distortion lookup tables
;       /NOSIP - If set, then do not apply the polynomial SIP distortions 
;       PRECISION - Integer scalar (0-4) specifying the number of digits 
;             displayed after the decimal of declination.   The RA is
;             automatically one digit more.   See ADSTRING() for more info.
;             Default value is 1, and the keyword is ignored if results are not 
;             displayed at the terminal 
;       /PRINT - If this keyword is set and non-zero, then results are displayed
;               at the terminal in both decimal and sexagesimal notation.
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
;       If this procedure is to be used repeatedly with the same file,
;       then it is quicker to call FITS_OPEN once and pass the file control block
;		FCB on each call to FITS_XYAD.
;
;       How to specify which extension in the FITS file to use
;               case 1: EXTEN_NO specified. EXTEN_NO will give the number of the
;                       extension to read.  The primary data unit is refered
;                       to as extension 0. If EXTEN_NO is specified, XTENSION,
;                       EXTNAME, EXTVER, and EXTLEVEL parameters are ignored.
;               case 2: if EXTEN_NO is not specified, the first extension
;                       with the specified XTENSION, EXTNAME, EXTVER, and
;                       EXTLEVEL will be read.  If any of the 4 parameters
;                       are not specified, they will not be used in the search.
;                       Setting EXTLEVEL=0, EXTVER=0, EXTNAME='', or
;                       XTENSION='' is the same as not supplying them.
;               case 3: if none of the keyword parameters, EXTEN_NO, XTENSION,
;                       EXTNAME, EXTVER, or EXTLEVEL are supplied.  FITS_XYAD
;                       will read the next extension in the file.  If the
;                       primary data unit (PDU), extension 0 has no astrometry, the
;                       first call to FITS_XYAD will read the first extension
;                       of the file.
;
; EXAMPLE:
;       A FITS file wfc3.fits contains astrometric information in celestial
;       coordinates.   Find the RA and Dec corresponding to position X=23.3
;        Y = 100.2 on an image
;		IDL> FITS_open, 'icau21u3q_flt.fits', fcb
;		IDL> FITS_xyad, fcb, 10, 10, aa, dd
;
; PROCEDURES CALLED
;       ADSTRING(), EULER, EXTAST, FITS_OPEN, GSSSXYAD, REPCHR(), XY2AD
;
; REVISION HISTORY:
;       Adapted from XYAD W. Landsman  October 2017 
;       Use both D2IMARR and WCSDVARR distortion tables  November  2017
;-    

 compile_opt idl2

 npar = N_params()
 if ( npar EQ 0 ) then begin
        print,'Syntax -  FITS_XYAD, fcb_or_filename, [x, y, a, d, /PRINT, Alt=, Precision=, '
        print,'                      /Galactic, /Celestial, /Ecliptic ]'
        print,'          extname=, exten_no=, extver=, extlevel=
        print,'fcb_or_filename - FITS control block (from FITS_OPEN) or FITS file name'
        print,'X,Y - Input X and Y positions (scalar or vector)'
        print,'A,D - Output RA and Dec in decimal degrees'
        return
 endif   
 
 Catch, theError
 IF theError NE 0 then begin
     Catch,/Cancel
     void = cgErrorMsg(/quiet)
     RETURN
     ENDIF                                                      

 fcbtype = size(filename_or_fcb,/tname)
 fcbsize = N_elements(filename_or_fcb)
 
 if (fcbsize NE 1) || (fcbtype NE 'STRUC') && (fcbtype NE 'STRING') then $
      message = 'Invalid Filename or FCB supplied'

 if fcbtype EQ 'STRING' then $
 	fits_open,filename_or_fcb,fcb else fcb = filename_or_fcb

 if N_elements(exten_no) EQ 0 then $  
 	if fcb.nextend GE 1 then exten_no = 1 else exten_no =0   
    
 fits_read, fcb, 0, hdr, /HEADER_ONLY, exten_no = exten_no, extname = extname, $
 	extver = extver, extlevel = extlevel  

;Extract astrometry structure     
  extast, hdr, astr, noparams, ALT = alt, has_CPDIS=has_CPDIS, has_D2IMDIS = has_D2IMDIS  

  if ( noparams LT 0 ) then begin
        if alt EQ '' then $
        message,'ERROR - No astrometry info in supplied FITS file' $
	else  message, $
	'ERROR  - No alt=' + alt + ' astrometry info in supplied FITS file'
  endif	
 

 
  astr2 = TAG_EXIST(astr,'AXES')
  
	if ( npar lt 3 ) then read,'XYAD: Enter X and Y positions: ',x,y
	xp = x
	yp = y

    if ~keyword_set(nodistort) then begin 
	if has_D2IMDIS then begin
	    extkey = sxpar(hdr,'D2IM1',dup=1)
	    e1 = (strsplit(extkey,' ',/ex))[1]
	    extkey = sxpar(hdr,'D2IM2',dup=1)
	    e2 = (strsplit(extkey,' ',/ex))[1]
	    
		fits_read,fcb, imdis1, hdis1, extname = 'D2IMARR',extver=e1,/no_abort,enum=enum1
		fits_read,fcb, imdis2, hdis2, extname = 'D2IMARR',extver=e2,/no_abort,enum=enum2
		if (enum1 GT 0) && (enum2 GT 0) then begin
		cdelt1 = sxpar(hdis1,'CDELT*')
		crval1 = sxpar(hdis1,'CRVAL*')
		crpix1 = sxpar(hdis1,'CRPIX*')
		xpos = (x+1-crval1[0])/cdelt1[0] + crpix1[0]
		ypos = (y+1-crval1[1])/cdelt1[1] + crpix1[1]
		xp1 =  interpolate(imdis1,xpos,ypos)
		
		cdelt2 = sxpar(hdis2,'CDELT*')
		crval2 = sxpar(hdis2,'CRVAL*')
		crpix2 = sxpar(hdis2,'CRPIX*')
		xpos = (x+1-crval2[0])/cdelt2[0] + crpix2[0]
		ypos = (y+1-crval2[1])/cdelt2[1] + crpix2[1]		

		yp1 = interpolate(imdis2,xpos,ypos)
		endif
		
	endif	
		
	if has_CPDIS then begin	
	    extkey = sxpar(hdr,'DP1',dup=1)
	    e1 = (strsplit(extkey,' ',/ex))[1]
	    extkey = sxpar(hdr,'DP2',dup=1)
	    e2 = (strsplit(extkey,' ',/ex))[1]
		fits_read,fcb, imdis1, hdis1, extname = 'WCSDVARR',extver=e1,/no_abort,enum=enum1
		fits_read,fcb, imdis2, hdis2, extname = 'WCSDVARR',extver=e2,/no_abort,enum=enum2
        if (enum1 GT 0) && (enum2 GT 0) then begin 
		cdelt1 = sxpar(hdis1,'CDELT*')
		crval1 = sxpar(hdis1,'CRVAL*')
		crpix1 = sxpar(hdis1,'CRPIX*')
		xpos = (x+1-crval1[0])/cdelt1[0] + crpix1[0]
		ypos = (y+1-crval1[1])/cdelt1[1] + crpix1[1]
		xp2 =  interpolate(imdis1,xpos,ypos)
		
		cdelt2 = sxpar(hdis2,'CDELT*')
		crval2 = sxpar(hdis2,'CRVAL*')
		crpix2 = sxpar(hdis2,'CRPIX*')
		xpos = (x+1-crval2[0])/cdelt2[0] + crpix2[0]
		ypos = (y+1-crval2[1])/cdelt2[1] + crpix2[1]		
		yp2 = interpolate(imdis2,xpos,ypos)
    
        xp += xp2 + xp1
        yp += yp2 + yp1

		endif
	endif    
  endif  

      
  if fcbtype EQ 'STRING' then fits_close,fcb
  if keyword_set(nosip) then astr.distort.name = ''
  case strmid(astr.ctype[0],5,3)  of 
        'GSS': gsssxyad, astr, xp, yp, a, d
         else: xy2ad, xp, yp, astr, a, d
  endcase
  titname = strmid(astr.ctype,0,4)
  if (titname[0] EQ 'DEC-') || (titname[0] EQ 'ELAT') || $
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

  if (npar lt 5) || keyword_set(PRINT) then begin
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
	tit += 	   '        ' + tit1  + eqnx +  '      ' + tit2 + eqnx
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
       if (N_unit1 GT 0) || (N_unit2 GT 0) then $
       print,unit1 ,unit2,f='(t23,a,t33,a)' 	    
        for i=0l, npts-1 do $
       print,FORMAT=fmt, float(x[i]), float(y[i]), a[i], d[i]
        endelse 
   endif
   
   return
   end
