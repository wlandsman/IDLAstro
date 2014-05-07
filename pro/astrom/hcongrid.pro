pro hcongrid, oldim, oldhd, newim, newhd, newx, newy, HALF_HALF = half_half, $
              INTERP=interp, OUTSIZE = outsize, CUBIC = cubic, ERRMSG = errmsg,$
	      ALT = alt
;+
; NAME:
;       HCONGRID
; PURPOSE:
;       CONGRID an image and update astrometry in a FITS header
; EXPLANATION:
;       Expand or contract an image using CONGRID and update the 
;       associated FITS header array.
;
; CALLING SEQUENCE:
;       HCONGRID, oldhd                       ;Update FITS header only
;       HCONGRID, oldim, oldhd, [ newim, newhd, newx, newy, /HALF_HALF
;                                 CUBIC = , INTERP=, OUTSIZE=, ERRMSG=, ALT= ]
;
; INPUTS:
;       OLDIM - the original image array
;       OLDHD - the original image FITS header, string array
;
; OPTIONAL INPUTS:
;       NEWX - size of the new image in the X direction
;       NEWY - size of the new image in the Y direction
;               The OUTSIZE keyword can be used instead of the 
;               NEWX, NEWY parameters
;
; OPTIONAL OUTPUTS:
;       NEWIM - the image after expansion or contraction with CONGRID
;       NEWHD - header for newim containing updated astrometry info
;               If output parameters are not supplied, the program
;               will modify the input parameters OLDIM and OLDHD
;               to contain the new array and updated header.
;
; OPTIONAL KEYWORD INPUTS:
;      ALT - Single character 'A' through 'Z' or ' ' specifying which astrometry
;          system to modify in the FITS header.    The default is to use the
;          primary astrometry of ALT = ' '.    See Greisen and Calabretta (2002)
;          for information about alternate astrometry keywords.

;       CUBIC - If set and non-zero, then cubic interpolation is used.   Valid
;               ranges are  -1 <= Cubic < 0.   Setting /CUBIC is equivalent to
;               CUBIC = -1 and also equivalent to INTERP = 2.   See INTERPOLATE
;               for more info.    Setting CUBIC = -0.5 is recommended.
;       ERRMSG - If this keyword is supplied, then any error mesasges will be
;               returned to the user in this parameter rather than depending on
;               on the MESSAGE routine in IDL.   If no errors are encountered
;               then a null string is returned.               
;       /HALF_HALF - Due to edge effects, the default behaviour of CONGRID is 
;           to introduce a slight shift in the image center.  Craig Markwardt
;           (http://cow.physics.wisc.edu/~craigm/idl/misc.html) has written
;           a modified version of CONGRID called CMCONGRID that when used with
;           the /HALF_HALF keyword eliminates any shift.   The use of the 
;           /HALF keyword emulates CMCONGRID and eliminates any shift in the
;           image centroid. 
;       INTERP   - 0 for nearest neighbor, 1 for bilinear interpolation
;               (default), 2 for cubic (=-1) interpolation.   
;       OUTSIZE - Two element integer vector which can be used instead of the
;               NEWX and NEWY parameters to specify the output image dimensions
; OPTIONAL KEYWORD OUTPUT:
;       ERRMSG - If this keyword is supplied, then any error mesasges will be
;               returned to the user in this parameter rather than depending on
;               on the MESSAGE routine in IDL.   If no errors are encountered
;               then a null string is returned.               
; PROCEDURE:
;       Expansion or contraction is done using the CONGRID function, unless
;       HALF_HALF is set. 
;
;       The parameters BSCALE, NAXIS1, NAXIS2, CRPIX1, and CRPIX2 and
;       the CD (or CDELT) parameters are updated for the new header.
;
; NOTES:
;       A FITS header can be supplied as the first parameter without having
;       to supply an image array.   The astrometry in the FITS header will be
;       updated to be appropriate to the specified image size.
;
;       If the FITS header contains astrometry from a ST Guide Star image,
;       then the astrometry will be converted to an approximately equivalent
;       tangent projection before applying CONGRID.
; EXAMPLE:
;       Congrid an 512 x 512 image array IM and FITS header H to size 300 x 300
;       using cubic interpolation.   Use the HALF_HALF keyword to avoid 
;       a shift of the image centroid
;
;       IDL> hcongrid, IM ,H, OUT = [300, 300], CUBIC = -0.5, /HALF
;
;       The variables IM and H will be modified to the new image size.
;
; PROCEDURES CALLED:
;       CHECK_FITS, CONGRID(), EXTAST, GSSS_STDAST, SXADDHIST, 
;       SXADDPAR, SXPAR(), ZPARCHECK
; MODIFICATION HISTORY:
;       Written, Aug. 1986 W. Landsman, STI Corp.
;       Added interp keywords, J. Isensee, July, 1990
;       Add cubic interpolation W. Landsman HSTX   January 1994
;       Recognize a GSSS FITS header   W. Landsman   June 1994
;       Fix case where header but not image supplied  W. Landsman  May 1995
;       Remove call to SINCE_VERSION()   W. Landsman   March 1996
;       Assume since IDL V3.5, add CUBIC keyword      W. Landsman   March 1997
;       Update BSCALE even if no astrometry present   W. Landsman   May 1997
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Added HALF_HALF keyword  W. Landsman  February 2000
;       Added ERRMSG keyword, use double precision formatting W.L.  April 2000
;       Recognize PC00n00m astrometry format  W. Landsman   December 2001
;       Now works when both /INTERP and /HALF are set W. Landsman January 2002
;       Fix output astrometry for non-equal plate scales for PC matrix or
;       CROTA2 keyword, added ALT keyword.   W. Landsman May 2005
;       Update distortion parameters if present  W. Landsman January 2008
;       Don't update BSCALE/BZERO for unsigned integer W.Landsman Mar 2008
;       Write CRPIX as Double precision if necessary W. Landsman Oct 2012
;- 
 On_error,2
 compile_opt idl2
 Npar = N_params()      ;Check # of parameters

 if Npar EQ 0  then begin 
     print,'    Syntax - HCONGRID, oldim, oldhd,[ newim, newhd, newx, newy'
     print,'           ALT=, CUBIC = , INTERP =, /HALF, OUTSIZE = , ERRMSG=]'
     return
 endif

 save_err = arg_present(errmsg)
 if Npar EQ 1 then begin 

        zparcheck, 'HCONGRID', oldim, 1, 7, 1, 'Image header'
        oldhd = oldim
        xsize = sxpar( oldhd,'NAXIS1')
        ysize = sxpar( oldhd,'NAXIS2')

 endif else begin
;   Check for valid 2-D image & header
  check_FITS, oldim, oldhd, dimen, /NOTYPE,ERRMSG = errmsg

  if errmsg NE '' then begin
        if ~save_err then message,'ERROR - ' + errmsg,/CON
        return
  endif
  if N_elements(dimen) NE 2 then begin 
      errmsg =  'Input image array must be 2-dimensional'
      if ~save_err then message,'ERROR - ' + errmsg,/CON
      return
  endif 
  xsize = dimen[0]  &  ysize = dimen[1]
 endelse
    tname = size(oldim,/tname)

 if keyword_set(CUBIC) then interp = 2
 if N_elements(interp) EQ 0 then interp = 1

 case interp of 
 0:   type = ' Nearest Neighbor Approximation'
 1:   type = ' Bilinear Interpolation'
 2:   type = ' Cubic Interpolation'
 else: begin 
       errmsg = 'Illegal value of INTERP keyword, must be 0, 1, or 2'
       if ~save_err then  message,'ERROR - ' + errmsg,/CON
       return
       end
 endcase

 if npar LT 6 then begin
    if ( N_elements(OUTSIZE) NE 2 ) then begin
      message, /INF, $
        'Original array size is '+ strn( xsize ) + ' by ' + strn(ysize)
      read,'Enter size of new image in the X direction: ',newx
      read,'Enter size of new image in the Y direction: ',newy
   endif else begin
      newx = outsize[0]
      newy = outsize[1]
   endelse 
 endif
 
 if ( xsize EQ newx ) && ( ysize EQ newy ) then begin 
       message,'Output image size equals input image size',/INF
       return
 endif

 xratio = float(newx)/xsize
 yratio = float(newy)/ysize
 lambda = yratio/xratio         ;Measures change in aspect ratio.


 if ( npar GT 1 ) then begin

 if keyword_set(half_half) then begin
   srx = (findgen(newx) + 0.5)/xratio - 0.5
   sry = (findgen(newy) + 0.5)/yratio - 0.5
   if interp GT 0 then begin
      if ( npar GT 2 ) then $
        newim = interpolate(oldim, srx,sry,/GRID, CUBIC = cubic) else $
        oldim = interpolate(oldim, srx,sry,/GRID, CUBIC = cubic)
   endif else begin
        xr = float(xsize)/newx  & yr = float(ysize)/newy
        if (npar GT 2) then $
        newim = POLY_2D(oldim, [[xr/2.,0],[xr,0]], $
                            [ [xr/2.,yr],[0,0] ],0,newx,newy) else $
        oldim = POLY_2D(oldim, [[yr/2.,0],[yr,0] ], $
                            [[ yr/2.,yr],[0,0] ],0,newx,newy) 
   endelse
 endif else begin
 
 if ( npar GT 2 ) then $
      newim = congrid( oldim, newx, newy, INTERP = interp, CUBIC = cubic) else $
      oldim = congrid( temporary(oldim), newx, newy, $
                CUBIC = cubic, INTERP=interp )
 endelse

 endif

 newhd = oldhd
 sxaddpar, newhd, 'NAXIS1', fix(newx)
 sxaddpar, newhd, 'NAXIS2', fix(newy)
 label = 'HCONGRID:' + strmid(systime(),4,20)
 history =   ' Original Image Size Was '+ strn(xsize) + ' by ' + strn(ysize)
 sxaddhist, label + history, newhd
 if npar GT 1 then sxaddhist, label+type, newhd

; Update astrometry info if it exists

 extast, newhd ,astr, noparams, ALT = alt
 if noparams GE 0 then begin
 if strmid(astr.ctype[0],5,3) EQ 'GSS' then begin
        gsss_stdast, newhd
        extast, newhd, astr, noparams
 endif

 pix_ratio = xratio*yratio      ;Ratio of pixel areas
 
  crpix = astr.crpix - 1.0
  
 if keyword_set(half_half) then begin
     sxaddpar, newhd, 'CRPIX1' + alt, $
                      (crpix[0]+0.5)*xratio + 0.5
     sxaddpar, newhd, 'CRPIX2' + alt,  $
                       (crpix[1]+0.5)*yratio + 0.5
 endif else begin 
     sxaddpar, newhd, 'CRPIX1' + alt , crpix[0]*xratio + 1.0
     sxaddpar, newhd, 'CRPIX2' + alt , crpix[1]*yratio + 1.0
 endelse 


 if tag_exist(astr,'DISTORT') then begin
         distort = astr.distort
	 message,'Updating SIP distortion parameters',/INF
         update_distort,distort, [1./xratio,0],[1./yratio,0]
	 astr.distort= distort
	 add_distort, newhd, astr
   endif	 



 if (noparams NE 2) then begin 

    cdelt = astr.cdelt
    sxaddpar, newhd, 'CDELT1' + alt , CDELT[0]/xratio
    sxaddpar, newhd, 'CDELT2' + alt , CDELT[1]/yratio
; Adjust the PC matrix if non-equal plate scales.   See equation 187 in 
; Calabretta & Greisen (2002)
    if lambda NE 1.0 then begin
        cd = astr.cd
	if noparams EQ 1 then begin
;Can no longer  use the simple CROTA2 convention, change to PC keywords
	 sxaddpar,newhd,'PC1_1'+alt, cd[0,0] 
	 sxaddpar, newhd,'PC2_2'+alt, cd[1,1]
         sxdelpar, newhd, ['CROTA2','CROTA1']
        endif	
        sxaddpar, newhd, 'PC1_2'+alt, cd[0,1]/lambda
        sxaddpar, newhd, 'PC2_1'+alt, cd[1,0]*lambda
   endif	


 endif else begin

    cd = astr.cd
    sxaddpar, newhd, 'CD1_1' + alt, cd[0,0]/xratio
    sxaddpar, newhd, 'CD1_2' + alt, cd[0,1]/yratio
    sxaddpar, newhd, 'CD2_1' + alt, cd[1,0]/xratio
    sxaddpar, newhd, 'CD2_2' + alt , cd[1,1]/yratio

 endelse
 endif 

; Adjust BZERO and BSCALE for new pixel size, unless these values are used
; to define unsigned integer data types.  

 bscale = sxpar( oldhd, 'BSCALE')
 bzero = sxpar( oldhd, 'BZERO')
 unsgn = (tname EQ 'UINT') || (tname EQ 'ULONG') 

 if ~unsgn then begin 
 if (bscale NE 0) && (bscale NE 1) then $
    sxaddpar, newhd, 'BSCALE', bscale/pix_ratio, 'Calibration Factor'
 if (bzero NE 0) then sxaddpar, newhd, 'BZERO', bzero/pix_ratio, $
       ' Additive Constant for Calibration'
 endif 

 if npar EQ 2 then oldhd = newhd else $
       if npar EQ 1 then oldim = newhd


 return
 end
