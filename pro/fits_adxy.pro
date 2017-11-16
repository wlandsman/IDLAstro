pro FITS_adxy, filename_or_fcb, a, d, x, y, PRINT = print, ALT = alt, exten_no=exten_no, $
	extver= extver, extname = extname,extlevel=extlevel,nodistort=nodistort,nosip=nosip
;+
; NAME:
;       FITS_adxy
; PURPOSE:
;       Use a FITS file with astrometry to convert astronomical to pixel coordinates
; EXPLANATION:
;       Use astrometry in a FITS file to compute X and Y positions, given the
;       RA and Dec (or longitude, latitude) in decimal degrees.  
;
;       This routine can be used with any FITS file containing WCS (world coordinate
;       system) information.    But it is especially useful for files using the
;		proposed distortion lookup tables (https://fits.gsfc.nasa.gov/wcs/dcs_20040422.pdf)
;		which are stored in separate extensions in the FITS file.
; CALLING SEQUENCE:
;       FITS_adxy, HDR               ;Prompt for Ra and DEC 
;       FITS_adxy, hdr, a, d, x, y, [ EXTEN_NO=, /PRINT, ALT= ]
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
;       /PRINT - If this keyword is set and non-zero, then results are displayed
;               at the terminal.
;
; OPERATIONAL NOTES:
;       If less than 5 parameters are supplied, or if the /PRINT keyword is
;       set, then the X and Y positions are displayed at the terminal.
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
;                       of the file.;		
;
; PROCEDURES CALLED:
;       AD2XY, ADSTRING(), EXTAST, FITS_OPEN, FITS_READ, GETOPT(), TEN()
;
; REVISION HISTORY:
;		Adapted from adxy.pro  W. Landsman    October 2017
;       Use both D2IMARR and WCSDVARR distortion tables  November  2017
;-
 Compile_opt idl2

 npar = N_params()

 if ( npar EQ 0 ) then begin
        print,'Syntax - FITS_adxy, fcb_or_filename , [a, d, x, y, /PRINT, ALT= ]'
        print,'fcb_or_filename - FITS control block (from FITS_OPEN) or FITS file name'
        print,' If supplied, A and D must be in decimal DEGREES'
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
 	
 if N_elements(exten_no) EQ 0 && N_Elements(extname) EQ 0 then exten_no = 1
 	
 fits_read, fcb, 0, hdr, /HEADER_ONLY, exten_no = exten_no, extname = extname, $
 		extver = extver   
     
 if N_elements(exten_no) EQ 0 then $  
 	if fcb.nextend GE 1 then exten_no = 1 else exten_no =0   
;Extract astrometry from FITS header  
	extast, hdr, astr, noparams, ALT = alt, has_CPDIS=has_CPDIS, has_D2IMDIS = has_D2IMDIS   
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
   read,'FITS_adxy: Enter coordinates: ',inp
   radec = getopt(inp,'F')
   case N_elements(radec) of 
      2: begin 
         a = radec[0] & d = radec[1]
         end
      6: begin
         a = ten(radec[0:2]*15.) & d = ten(radec[3:5])
         end
   else: begin
         print,'FITS_adxy: ERROR - Either 2 or 6 parameters must be entered'
         return
         end
   endcase 
 endif
   if keyword_set(nosip) then astr.distort.name = ''
 

 case strmid( astr.ctype[0], 5,3) of
 'GSS': gsssFITS_adxy, astr, a, d, x, y       ;HST Guide star astrometry
 else:  ad2xy, a, d, astr, x, y          ;All other cases
 endcase
 
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
		xpos = (x-crval1[0])/cdelt1[0] + crpix1[0]
		ypos = (y-crval1[1])/cdelt1[1] + crpix1[1]
		
		cdelt2 = sxpar(hdis2,'CDELT*')
		crval2 = sxpar(hdis2,'CRVAL*')
		crpix2 = sxpar(hdis2,'CRPIX*')
		xpos = (x-crval2[0])/cdelt2[0] + crpix2[0]
		ypos = (y-crval2[1])/cdelt2[1] + crpix2[1]		

		xp1 = interpolate(imdis1,xpos,ypos)
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
		xpos = (x-crval1[0])/cdelt1[0] + crpix1[0]
		ypos = (y-crval1[1])/cdelt1[1] + crpix1[1]

		
		cdelt2 = sxpar(hdis2,'CDELT*')
		crval2 = sxpar(hdis2,'CRVAL*')
		crpix2 = sxpar(hdis2,'CRPIX*')
		xpos = (x-crval2[0])/cdelt2[0] + crpix2[0]
		ypos = (y-crval2[1])/cdelt2[1] + crpix2[1]		
 
        xp2 = interpolate(imdis1,xpos,ypos)
        yp2 = interpolate(imdis2,xpos,ypos)

		endif
	endif
	x = x - xp1 - xp2
	y = y - yp1 - yp2    
  endif  
  if fcbtype EQ 'STRING' then fits_close,fcb


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
