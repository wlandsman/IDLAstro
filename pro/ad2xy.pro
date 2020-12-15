pro ad2xy, a, d, astr, x, y
;+
; NAME:
;     AD2XY
; PURPOSE:
;     Compute X and Y from native coordinates and a FITS  astrometry structure
; EXPLANATION:
;     If a WCS projection (Calabretta & Greisen 2002, A&A, 395, 1077) is 
;     present, then the procedure WCSXY2SPH is used to compute native 
;     coordinates.   If distortion is present then this is corrected.  
;     In all cases, the inverse of the CD matrix is applied and offset 
;     from the reference pixel to obtain X and Y. 
;
;     AD2XY is generally meant to be used internal to other procedures.   For 
;     interactive purposes, use ADXY.
;
; CALLING SEQUENCE:
;     AD2XY, a ,d, astr, x, y   
;
; INPUTS:
;     A -     R.A. or longitude in DEGREES, scalar or vector.    
;     D -     Dec. or longitude in DEGREES, scalar or vector
;             If the input A and D are arrays with 2 or more dimensions,
;             they will be converted to a 1-D vectors.
;     ASTR - astrometry structure, output from EXTAST procedure containing:
;        .CD   -  2 x 2 array containing the astrometry parameters CD1_1 CD1_2
;               in DEGREES/PIXEL                                   CD2_1 CD2_2
;        .CDELT - 2 element vector giving increment at reference point in
;               DEGREES/PIXEL
;        .CRPIX - 2 element vector giving X and Y coordinates of reference pixel
;               (def = NAXIS/2) in FITS convention (first pixel is 1,1)
;        .CRVAL - 2 element vector giving coordinates of the reference pixel 
;               in DEGREES
;        .CTYPE - 2 element vector giving projection types 
;        .LONGPOLE - scalar longitude of north pole (default = 180) 
;        .PV2 - Vector of additional parameter (e.g. PV2_1, PV2_2) needed in 
;               some projections
;
;     Fields added for version 2:
;      .PV1 - Vector of projection parameters associated with longitude axis
;      .AXES  - 2 element integer vector giving the FITS-convention axis 
;               numbers associated with astrometry, in ascending order. 
;               Default [1,2].
;      .REVERSE - byte, true if first astrometry axis is Dec/latitude
;      .COORDSYS - 1 or 2 character code giving coordinate system, including
;                 'C' = RA/Dec, 'G' = Galactic, 'E' = Ecliptic, 'X' = unknown.
;      .RADECSYS - String giving RA/Dec system e.g. 'FK4', 'ICRS' etc.
;      .EQUINOX  - Double giving the epoch of the mean equator and equinox
;      .DATEOBS  - Text string giving (start) date/time of observations
;      .MJDOBS   - Modified julian date of start of observations.
;      .X0Y0     - Implied offset in intermediate world coordinates if user has
;                  specified a non-standard fiducial point via PV1 and also
;                  has set PV1_0a =/ 0 to indicate that the offset should be
;                  applied in order to place CRVAL at the IWC origin.
;                  Should be *added* to the IWC derived from application of
;                  CRPIX, CDELT, CD to the pixel coordinates.
;
;        .DISTORT - Optional substructure specifying distortion parameters
;
; OUTPUTS:
;     X     - row position in pixels, scalar or vector
;     Y     - column position in pixels, scalar or vector
;
;     X,Y will be in the standard IDL convention (first pixel is 0), and
;     *not* the FITS convention (first pixel is 1)
; NOTES:
;      AD2XY tests for presence of WCS coordinates by the presence of a dash 
;      in the 5th character position in the value of CTYPE (e.g 'DEC--SIN').
; COMMON BLOCKS:
;      BROYDEN_COMMON - Used when solving for a reverse distortion tranformation
;        (either SIP or TGV) by iterating on the forward transformation.
; PROCEDURES USED:
;       CGErrorMsg (from Coyote Library)
;       TAG_EXIST(), WCSSPH2XY
; REVISION HISTORY:
;     Converted to IDL by B. Boothman, SASC Tech, 4/21/86
;     Use astrometry structure,  W. Landsman      Jan. 1994   
;     Do computation correctly in degrees  W. Landsman       Dec. 1994
;     Only pass 2 CRVAL values to WCSSPH2XY   W. Landsman      June 1995
;     Don't subscript CTYPE      W. Landsman       August 1995        
;     Understand reversed X,Y (X-Dec, Y-RA) axes,   W. Landsman  October 1998
;     Consistent conversion between CROTA and CD matrix W. Landsman October 2000
;     No special case for tangent projection W. Landsman June 2003
;     Work for non-WCS coordinate transformations W. Landsman Oct 2004
;     Use CRVAL reference point for non-WCS transformation  W.L. March 2007
;     Use post V6.0 notation  W.L. July 2009
;     Allows use of Version 2 astrometry structure & optimised for
;     large input arrays. Wrap test for cylindrical coords. J. P. Leahy July 2013
;     Wrap test failed for 2d input arrays 
;                              T. Ellsworth-Bowers/W.Landsman July 2013
;     Tweaked to restore shape of arrays on exit JPL Aug 2013.
;     ..and make them scalars if input is scalar JPL Aug 2013
;     Iterate when forward SIP coefficients are supplied but not the reverse
;     coefficients.    Don't compute poles if not a cylindrical system
;              W. Landsman           Dec 2013
;     Evaluate TPV distortion (SCAMP) if present  W. Landsman  Jan 2014
;     Support IRAF TNX projection M. Sullivan U. of Southhamptom  Mar 2014
;     No longer check that CDELT[0] differs from 1 W. Landsman Apr 2015
;     Default projection is PIXEL not Tangent  W. Landsman Oct 2017
;     
;-

 compile_opt idl2
 common broyden_coeff, xcoeff, ycoeff
 

 if N_params() lT 4 then begin
        print,'Syntax -- AD2XY, a, d, astr, x, y'
        return
 endif

 Catch, theError
 IF theError NE 0 then begin
     Catch,/Cancel
     void = cgErrorMsg(/quiet)
     RETURN
     ENDIF

  if tag_exist(astr,'DISTORT') && ((astr.distort.name EQ 'TPV') || (astr.distort.name EQ 'TNX')) then $
  ctype = strmid(astr.ctype,0,4) + '-TAN' else ctype = astr.ctype 
 crval = astr.crval

 testing = 0B
 size_a = SIZE(a)
 ndima = size_a[0]

 astr2 = TAG_EXIST(astr,'AXES') ; version 2 astrometry structure
 IF astr2 THEN reverse = astr.reverse ELSE BEGIN
     coord = strmid(ctype,0,4)
     reverse = ((coord[0] EQ 'DEC-') && (coord[1] EQ 'RA--')) || $
               ((coord[0] EQ 'GLAT') && (coord[1] EQ 'GLON')) || $
               ((coord[0] EQ 'ELAT') && (coord[1] EQ 'ELON'))
 ENDELSE
 if reverse then crval = rotate(crval,2)        ;Invert CRVAL?
     
  spherical = strmid(astr.ctype[0],4,1) EQ '-'
  if spherical then begin
      IF astr2 THEN BEGIN
          cylin = WHERE(astr.projection EQ ['CYP','CAR','MER','CEA','HPX'],Ncyl)
          IF Ncyl GT 0 THEN BEGIN	
	  testing = 1
          size_d = SIZE(d)
          ndimd = size_d[0]
          IF ndima GT 1 THEN a = REFORM(a, size_a[ndima+2], /OVERWRITE)
          IF ndimd GT 1 THEN d = REFORM(d, size_d[ndimd+2], /OVERWRITE)
          a0 = [a, 0d0,180d0]  & d0 = [d, 0d0, 0d0] ; test points
          wcssph2xy, a0, d0, xsi, eta, CTYPE = ctype, PV1 = astr.pv1, $
              PV2 = astr.pv2, CRVAL = crval, CRXY = astr.x0y0 
	  ENDIF ELSE BEGIN
	  pv1 = astr.pv1
	  pv2 = astr.pv2
          if tag_exist(astr,'DISTORT') then $
	      if astr.distort.name EQ 'TPV' then begin 
	           pv1 = [0.0d,0,90.0d,180d,90d]    ;Tangent projection
	           pv2 = [0.0,0.0]
	      ENDIF   
          wcssph2xy, a, d, xsi, eta, CTYPE = ctype, PV1 = pv1, $
              PV2 = pv2, CRVAL = crval, CRXY = astr.x0y0 
	   ENDELSE
      ENDIF ELSE wcssph2xy, a, d, xsi, eta, CTYPE = ctype, PV2 = astr.pv2, $
        LONGPOLE = astr.longpole, CRVAL = crval, LATPOLE = astr.latpole
  endif else begin
        xsi = a - crval[0] & eta = d - crval[1]
  endelse	
  cd = astr.cd
  cdelt = astr.cdelt

  cd[0,0] *= cdelt[0] & cd[0,1] *= cdelt[0]
  cd[1,1] *= cdelt[1] & cd[1,0] *= cdelt[1]
     
 if reverse then begin
     temp = TEMPORARY(xsi) &  xsi = TEMPORARY(eta) & eta = TEMPORARY(temp)
 endif

   if tag_exist(astr,'DISTORT') && (astr.distort.name EQ 'TPV') then begin
            ctype = strmid(astr.ctype,0,4) + '-TAN'
            xcoeff = astr.pv1
	    ycoeff = astr.pv2
	    x0 = xcoeff[0]
	     y0 = ycoeff[0]  
	    for i=0, N_elements(xsi)-1 do begin	
	      	xcoeff[0] = x0 - xsi[i]
	     	ycoeff[0] = y0 - eta[i]    
	      	res = broyden([xsi[i],eta[i]], 'TPV_EVAL' )	     
	      	xsi[i] = res[0]
	      	eta[i] = res[1]
	      endfor
       ENDIF
   if tag_exist(astr,'DISTORT') && (astr.distort.name EQ 'TNX') then begin
            ctype = strmid(astr.ctype,0,4) + '-TAN'
            xcoeff = astr.distort.lngcor
	    ycoeff = astr.distort.latcor
            x0 = xcoeff.coeff[0]
            y0 = ycoeff.coeff[0]  
	    for i=0, N_elements(xsi)-1 do begin	
	      	xcoeff.coeff[0] = x0 - xsi[i]
	     	ycoeff.coeff[0] = y0 - eta[i]    
	      	res = broyden([xsi[i],eta[i]], 'TNX_EVAL' )	     
	      	xsi[i] = res[0]
	      	eta[i] = res[1]
	      endfor
       ENDIF

 crpix = astr.crpix - 1
 
 cdinv = invert(cd)
 x = ( cdinv[0,0]*xsi + cdinv[0,1]*eta  )
 y = ( cdinv[1,0]*TEMPORARY(xsi) + cdinv[1,1]*TEMPORARY(eta)  )

 if tag_exist(astr,'DISTORT') && ( astr.distort.name EQ 'SIP') then begin
           distort  = astr.distort
           ap = distort.ap
           bp = distort.bp
           na = ((size(ap,/dimen))[0])
; If reverse SIP coefficients are not supplied we iterate on the forward 
; coefficients (using BROYDEN).	   
           if na LE 1 then begin     
	      xcoeff = distort.a
	      ycoeff = distort.b	
	      x0 = xcoeff[0]
	      y0 = ycoeff[0]  
	      for i=0, N_elements(x)-1 do begin	
	      	xcoeff[0] = x0 - x[i]
	     	ycoeff[0] = y0 - y[i]    
	      	res = broyden([x[i],y[i]], 'SIP_EVAL' )	     
	      	x[i] = res[0]
	      	y[i] = res[1]
	      endfor
	   endif else begin   
           xdif1 = x
           ydif1 = y	   
           for i=0,na-1 do begin
               for j=0,na-1 do begin
                  if ap[i,j] NE 0.0 then xdif1 += x^i*y^j*ap[i,j]            
                  if bp[i,j] NE 0.0 then ydif1 += x^i*y^j*bp[i,j]
           endfor
           endfor

           x = xdif1
           y = ydif1
         ENDELSE
 ENDIF

 x += crpix[0] 
 y += crpix[1] 

; Check for wrapping in cylindrical projections: since the same phi
; appears at regular intervals in (x,y), depending on the location of
; the reference point on the pixel grid, some of the returned pixel 
; values may be offset by 360 degrees from the ones we want.
;
; The pixel grid may be rotated relative to intermediate world coords, 
; so the offset may have both x and y components in pixel space. 
;
; Doesn't try if native and astronomical poles are misaligned
; as this fix doesn't work in that case.

 IF testing THEN BEGIN
     npt = N_ELEMENTS(a)
     x0 = x[npt:npt+1] & y0 = y[npt:npt+1]
     x  = x[0:npt-1]   & y  = y[0:npt-1]
     
         crval = astr.crval
         IF astr.reverse THEN crval = REVERSE(crval)
         WCS_GETPOLE, crval, astr.pv1[3]-astr.pv1[1], astr.pv1[2], $
                      alpha_p, delta_p, $
                      LATPOLE = astr.pv1[4], AT_POLE = at_pole
         IF at_pole THEN BEGIN
             naxis = astr.naxis
             offmap = WHERE(x LT 0 OR y LT 0 OR $
                            x GT naxis[0] OR y GT naxis[1], noff)          
             IF offmap[0] NE -1 THEN BEGIN
                                 ; 360 degree shift
                 x360 = 2d0*(x0[1] - x0[0])
                 y360 = 2d0*(y0[1] - y0[0])
                 IF x360 LT 0 THEN BEGIN
                     x360 *= -1d0
                     y360 *= -1d0
                 ENDIF
                 xshift = x360 NE 0d0
                 yshift = y360 NE 0d0
                             ; Figure out which direction shift is
                 IF xshift THEN BEGIN
                     IF (MIN(x[offmap],/NAN) LT 0) THEN BEGIN
                         x[offmap] += x360
                         IF yshift THEN y[offmap] += y360
                     ENDIF ELSE IF MAX(x[offmap],/NAN) GT naxis[0] THEN BEGIN
                         x[offmap] -= x360
                         IF yshift THEN y[offmap] -= y360
                     ENDIF
                 ENDIF ELSE BEGIN
                     IF y360 LT 0 THEN BEGIN
                         x360 *= -1d0
                         y360 *= -1d0
                     ENDIF
                     IF (MIN(y[offmap],/NAN) LT 0) THEN BEGIN
                         IF xshift THEN x[offmap] += x360
                         y[offmap] += y360
                     ENDIF ELSE BEGIN
                         IF xshift THEN x[offmap] -= x360
                         y[offmap] -= y360
                     ENDELSE
                 ENDELSE
             ENDIF
         ENDIF
     ENDIF
 

 IF ndima GT 1 THEN BEGIN
    a = REFORM(a, size_a[1:ndima], /OVERWRITE)
    d = REFORM(d, size_a[1:ndima], /OVERWRITE)
    x = REFORM(x, size_a[1:ndima], /OVERWRITE)
    y = REFORM(y, size_a[1:ndima], /OVERWRITE)
 ENDIF ELSE if ndima EQ 0 THEN BEGIN
    a = a[0]
    d = d[0]
    x = x[0]
    y = y[0]
 ENDIF
 
 return
 end
