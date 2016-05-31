pro hastrom,oldim,oldhd,newim,newhd,refhd,MISSING=missing, INTERP = interp, $
               ERRMSG = errmsg,CUBIC = cubic, DEGREE = Degree, NGRID = Ngrid, $
	       SILENT = silent
;+
; NAME:
;       HASTROM
; PURPOSE:
;       Transformation of an image to align it with a reference image
; EXPLANATION:
;       A  transformation is applied (using POLY_2D) to an image so that   
;       its astrometry is identical with that in a reference header.  This
;       procedure can be used to align two images.
;
; CALLING SEQUENCE:
;       HASTROM, oldim, oldhd, newim, newhd, refhd, [MISSING =, INTERP = ]
;                            or
;       HASTROM, oldim, oldhd, refhd, [MISSING =, INTERP ={0,1,2}, NGRID =, 
;                                      CUBIC =, DEGREE = ]
;
; INPUTS:
;       OLDIM - Image array to be manipulated.  If only 3 parameters are
;               supplied then OLDIM and OLDHD will be modified to contain 
;               the output image array and header
;       OLDHD - FITS header array for OLDIM, containing astrometry parameters
;       REFHD - Reference header, containing astrometry parameters.  OLDIM
;               will be rotated, shifted, and compressed or expanded until
;               its astrometry matches that in REFHD.
; OUTPUTS:
;       NEWIM - Image array after transformation has been performed.
;               The dimensions of NEWIM will be identical to the NAXIS1 and 
;               NAXIS2 keywords specified in REFHD.  Regions on the reference 
;               image that do not exist in OLDIM can be assigned a value with
;               the MISSING keyword.
;       NEWHD - Updated FITS image header associated with NEWIM
;
; OPTIONAL INPUT KEYWORDS:
;       CUBIC - a scalar value between -1 and 0 specifying cubic interpolation
;               with the specified value as the cubic interpolation parameter.
;              (see poly_2d for info).    Setting CUBIC to a value greater 
;               than zero is equivalent to setting CUBIC = -1. 
;       DEGREE - Integer scalar specifying the degree of the transformation.
;               See the routine POLYWARP for more info.   Default = 
;               1 (linear transformation) unless polynomial ('SIP') distortion 
;               parameters are present in either the input or reference FITS
;               header.    In that case, the default degree is equal to the
;               degree of the distortion polynomial.     Currently, HASTROM 
;               will force a value of degree of less than 4 (see notes)
;       INTERP - Scalar, one of 0, 1, or 2 determining type of interpolation
;               0 nearest neighbor, 1 (default) bilinear interpolation, 
;               2 cubic interpolation.
;       MISSING - Set this keyword to a scalar value which will be assigned
;               to pixels in the output image which are out of range of the
;               supplied imput image.  If not supplied, then linear 
;               extrapolation is used.   See the IDL manual on POLY_2D.
;               ***NOTE: A bug was introduced into the POLY_2D function in IDL 
;               V5.5 (fixed in V6.1) such that the MISSING keyword
;               may not work properly with floating point data***
;       NGRID -  Integer scalar specifying the number of equally spaced grid 
;               points on each axis to use to specify the transformation.   
;               The value of NGRID must always be greater than DEGREE + 1.
;               The default is DEGREE + 2 which equals 3 (9 total points) for
;               DEGREE=1 (linear warping).
;       SILENT - If set, then some informational error messages are suppressed.
; OPTIONAL OUTPUT KEYWORD:
;       ERRMSG - If this keyword is supplied, then any error messages will be
;               returned to the user in this parameter rather than depending on
;               on the MESSAGE routine in IDL.   If no errors are encountered
;               then a null string is returned.               
; NOTES:
;       (1) The 3 parameter calling sequence is less demanding on virtual 
;               memory.
;       (2) The astrometry in OLDHD will be precessed to match the equinox
;                given in REFHD.
;       (3) If an ST Guidestar image is used for the reference header, then the
;                output header will be converted to standard astrometry. 
;       (4) We found (in May 2016) numerical instability in POLYWARP when 
;             Degree is set to a value of 5 or larger.    Therefore DEGREE will
;             be forced to a value of 4 or less (along with a warning).      Note 
;             that in POLYWARP a DEGREE of 5 actually includes 10th order terms 
;             like x^5*y^5
; EXAMPLE:
;       Suppose one has an image array, IM, and an associated FITS header H.
;       One desires to warp the image array so that it is aligned with another
;       image with a FITS header, HREF.    Both headers contain astrometry info.
;       Set pixel values to 0 where there is no overlap between the input and
;       reference image, and use linear interpolation (default)
;
;       IDL> hastrom, IM, H, HREF, MISSING = 0
;
; PROCEDURES USED:
;       ad2xy, check_FITS, extast, get_EQUINOX(), gsssextast, hprecess,
;       putast, sxaddpar, sxaddhist, sxpar(), xy2ad, zparcheck
;
; REVISION HISTORY:
;       Written  W. Landsman, STX Co.              Feb, 1989
;       Updated to CHECK_FITS                      Dec, 1991
;       New astrometry keywords                    Mar, 1994
;       Recognize GSSS header   W. Landsman        June, 1994
;       Added CUBIC keyword     W. Landsman        March, 1997
;       Accept INTERP=0, Convert output GSS header to standard astrometry
;                               W. Landsman        June 1998
;       Remove calls to obsolete !ERR system variable   March 2000
;       Added ERRMSG output keyword  W. Landsman    April 2000
;       Need to re-extract astrometry after precession  W. Landsman Nov. 2000
;       Check for distortion parameters in headers, add more FITS HISTORY
;       information                        W. Landsman   February 2005
;       Use different coefficient for nearest neighbor to avoid half-pixel
;       shift with POLY_2D      W. Landsman   Aug 2006
;       Return ERRMSG if no overlap between images  W. Landsman  Nov 2007
;       Use V6.0 notation  W. Landsman  Jan 2012
;       Test for Degree > 4 usage in Polywarp  W. Landsman   May 2016
;       Ensure all grid point computations are Double  W. Landsman May 2016
;       
;-
 compile_opt idl2
 On_error,2                              ;Return to caller
 npar = N_params()

 if (npar LT 3) or (npar EQ 4) then begin        ;3 parameter calling sequence?
        print,'Syntax:  HASTROM, oldim, oldhd, refhd'
        print,'     or  HASTROM, oldim, oldhd, newim, newhd, refhd'
        print,'                 [ MISSING=, DEGREE=, INTERP=, NGRID=, CUBIC = ]'
        return
 endif  

 if ( npar EQ 3 ) then begin
        zparcheck, 'HASTROM', newim, 3, 7, 1, 'Reference FITS header'
        refhd = newim
 endif else  $
        zparcheck, 'HASTROM', refhd, 5, 7, 1, 'Reference FITS header'

 radeg = 180.D/!DPI                      ;Double precision !RADEG

save_err = arg_present(errmsg)     ;Does user want error msgs returned?

;                                    Check for valid 2-D image & header
 check_FITS, oldim, oldhd, dimen, /NOTYPE, ERRMSG = errmsg
  if errmsg NE '' then begin
        if ~save_err then message,'ERROR - ' + errmsg,/CON
        return
  endif

  if N_elements(dimen) NE 2 then begin 
        errmsg =  'ERROR - Input image array must be 2-dimensional'
        if ~save_err then message,'ERROR - ' + errmsg,/CON
        return
 endif

 xsize_old = dimen[0]  &  ysize_old = dimen[1]

 xsize_ref = sxpar( refhd, 'NAXIS1' )                ;Get output image size
 ysize_ref = sxpar( refhd, 'NAXIS2' ) 
 if (xsize_ref LT 1) || (ysize_ref LT 1) then begin 
       errmsg = 'ERROR - Reference header must be for a 2-dimensional image'
       if ~save_err then message,'ERROR - ' + errmsg,/CON
       return
 endif
     

; Extract CD, CRPIX and CRVAL value from image header and reference header

 newhd = oldhd
 extast, newhd, astr_old, par_old    
 if ( par_old LT 0 ) then begin   
       errmsg = 'ERROR - Input FITS Header does not contain astrometry'
       if ~save_err then message,'ERROR - ' + errmsg,/CON
       return
 endif
 extast, refhd, astr_ref, par_ref    
 if ( par_old LT 0 ) || ( par_ref LT 0 ) then begin  
       errmsg = 'ERROR -Reference FITS Header does not contain astrometry'
       if ~save_err then message,'ERROR - ' + errmsg,/CON
       return
 endif


;   Precess the header if necessary

 refeq = get_equinox( refhd, code)
 if code EQ -1 then message, NoPrint = Silent, $
   'WARNING - Equinox not specified in reference header',/CON else begin
   oldeq = get_equinox( oldhd, code)
   if code EQ -1 then message, NoPrint = Silent, $
      'WARNING - Equinox not specified in original header',/CON else $
   if oldeq NE refeq then begin      ;Precess header and re-extract structure
           hprecess, newhd, refeq
           extast, newhd, astr_old, par_old
   endif    
 endelse

; Make a grid of points in the reference image to be used for the transformation

 if ~keyword_set( DEGREE ) then degree = 1
    if tag_exist(astr_old,'DISTORT') then begin
       distort = astr_old.distort
       if distort.name EQ 'SIP' then begin
          na = ((size(distort.ap,/dimen))[0])
          degree = degree > (na -1 )     
        endif
     endif

    if tag_exist(astr_ref,'DISTORT') then begin
       distort = astr_ref.distort
       if distort.name EQ 'SIP' then begin
          na = ((size(distort.a,/dimen))[0])
          degree = degree > (na -1 )     
        endif
     endif
      
 if ~keyword_set(NGRID) then ngrid = (degree + 2)
 if ~keyword_set(CUBIC) then begin 
        cubic = 0
        if N_elements(INTERP) EQ 0 then Interp = 1
 endif

 nxdif = round( xsize_ref / (ngrid-1) ) + 1
 nydif = round( ysize_ref / (ngrid-1) ) + 1

 xref = dblarr(ngrid,ngrid) & yref = xref
 xrow = [ lindgen(ngrid-1)*nxdif, xsize_ref-1. ]
 yrow = [ lindgen(ngrid-1)*nydif, ysize_ref-1. ]

 for i=0,ngrid-1 do xref[0,i] =   xrow     ;Four corners of image
 for i=0,ngrid-1 do yref[0,i] = replicate( yrow[i], ngrid)

; Find the position of the reference points in the supplied image

 case strmid(astr_ref.ctype[0],5,3) of
       'GSS': gsssxyad, astr_ref, xref, yref, ra, dec
        else: xy2ad, xref, yref, astr_ref, ra, dec
 endcase

 case strmid(astr_old.ctype[0],5,3) of
        'GSS': gsssadxy, astr_old, ra, dec, x, y
        else: ad2xy, ra, dec, astr_old, x, y
 endcase

 if ( max(x) LT 0 ) || ( min(x) GT xsize_old ) || $
    ( max(y) LT 0 ) || ( min(y) GT ysize_old ) then begin
      errmsg = 'No overlap found between original and reference images'
      if ~save_err then begin 
         message,'ERROR - ' + errmsg,/CON
         message,'Be sure you have the right headers and the right equinoxes',/CON
      endif	 
      return
 endif
 
 if degree GT 4 then message,/INF, $
    'Warning - POLYWARP Polynomial degree set to 4'

  if interp EQ 0 $ ;Get coefficients
    then polywarp, x+.5, y+.5, xref, yref, degree<4, kx, ky, status = status $
    else polywarp, x, y, xref, yref, degree<4, kx, ky ,status=status
    case status of 
    0: 
    1: message,NoPrint=Silent,/INF,'Warning: Singular matrix in version in PolyWarp'
    2: message,NoPrint=Silent,/INF,'Warning: Small Pivot element in Polywarp'
    3: message,'Invalid Status value returned from Polywarp'
    endcase
  
 
 if N_elements(missing) NE 1 then begin        ;Do the warping

 if npar EQ 3 then $
    oldim = poly_2d( temporary(oldim), kx, ky, Interp, xsize_ref, ysize_ref, $
                      CUBIC = cubic) else $
    newim = poly_2d( oldim, kx, ky, Interp, xsize_ref, ysize_ref, CUBIC = cubic)

 endif else begin

 if npar EQ 3 then $
    oldim = poly_2d( temporary(oldim), kx, ky, Interp, xsize_ref, ysize_ref, $
         MISSING=missing, CUBIC = cubic) $
 else $
    newim = poly_2d( oldim, kx, ky, Interp, xsize_ref, ysize_ref, $
          MISSING=missing, CUBIC = cubic)

 endelse
 
 sxaddpar, newhd, 'NAXIS1', xsize_ref
 sxaddpar, newhd, 'NAXIS2', ysize_ref

 if strmid(astr_ref.ctype[0],5,3) EQ 'GSS' then begin
        refhdnew = refhd
        gsss_stdast,refhdnew
        extast,refhdnew,astr_ref
 endif
 putast, newhd, astr_ref

 label = 'HASTROM: ' + strmid(systime(),4,20)
 image = sxpar( refhd, 'IMAGE', Count = N_image)
 if N_image EQ 1 THEN sxaddhist,label+' Reference Image - ' + image,newhd
 sxaddhist,label+ ' Original Image Size X: ' + strtrim(xsize_old,2) + $
                   ' Y: '  + strtrim(ysize_old,2), newhd
 sxaddhist,'HASTROM: Polynomial Degree used for image warping: ' + $
            strtrim(degree<4,2), newhd
 if cubic NE 0 then sterp = 'CUBIC = ' + strtrim(cubic,2) else $
     sterp = (['Nearest Neighbor','Linear','Cubic'])[interp]
 sxaddhist,'HASTROM: ' + sterp + ' interpolation',newhd
 sxaddhist,'HASTROM: Number of grid points ' + strtrim(ngrid*ngrid,2), newhd

; Update BSCALE and BZERO factors in header if necessary.   This is only an
; approximate correction for nonlinear warping.

 bscale = sxpar( newhd, 'BSCALE', Count = N_Bscale)
 if (N_bscale GT 0 ) && ( bscale NE 1. ) then begin
    getrot, astr_old, rot, cdelt_old, SILENT = silent 
    getrot, astr_ref, rot, cdelt_ref, SILENT = silent
    pix_ratio = ( cdelt_old[0]*cdelt_old[1]) / (cdelt_ref[0]*cdelt_ref[1] )
    sxaddpar, newhd, 'BSCALE', bscale/pix_ratio
    bzero = sxpar( newhd,'BZERO' )
    if bzero NE 0. then sxaddpar, newhd, 'BZERO', bzero/pix_ratio
 endif

 if npar LT 4 then oldhd = newhd

 return
 end
