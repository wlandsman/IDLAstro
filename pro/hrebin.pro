 pro hrebin, oldim, oldhd, newim, newhd, newx, newy, TOTAL = total, $
            SAMPLE=sample, OUTSIZE = outsize, ERRMSG = errmsg, ALT=alt
;+
; NAME:
;    HREBIN
; PURPOSE:
;    Expand or contract a FITS image using (F)REBIN and update the header 
; EXPLANATION:
;    If the output size is an exact multiple of the input size then REBIN is 
;    used, else FREBIN is used.   User can either overwrite the input array,
;    or write to new variables.     By default, the counts/pixel is preserved,
;    though one can preserve the total counts or surface flux by setting /TOTAL
;
; CALLING SEQUENCE:
;    HREBIN, oldhd        ;Special calling sequence to just update header
;    HREBIN, oldim, oldhd, [ newim, newhd, newx, newy, OUTSIZE = ,/SAMPLE, 
;                            ERRMSG =  ]
;
; INPUTS:
;    OLDIM - the original image array
;    OLDHD - the original image FITS header, string array
;
; OPTIONAL INPUTS:
;    NEWX - size of the new image in the X direction, integer scalar
;    NEWY - size of the new image in the Y direction, integer scalar
;            HREBIN will prompt for NEWX and NEWY if not supplied
;
; OPTIONAL OUTPUTS:
;    NEWIM - the image after expansion or contraction with REBIN
;    NEWHD - header for newim containing updated astrometry info
;            If output parameters are not supplied, the program will modify
;            the input parameters OLDIM and OLDHD to contain the new array and 
;            updated header.
;
; OPTIONAL INPUT KEYWORDS:
;    ALT - Single character 'A' through 'Z' or ' ' specifying which astrometry
;          system to modify in the FITS header.    The default is to use the
;          primary astrometry of ALT = ' '.    See Greisen and Calabretta (2002)
;          for information about alternate astrometry keywords.
;
;    OUTSIZE - Two element integer vector which can be used instead of the
;             NEWX and NEWY parameters to specify the output image dimensions
;
;    /SAMPLE - Expansion or contraction is done using REBIN which uses 
;              bilinear interpolation when magnifying and boxaveraging when 
;              minifying.   If the SAMPLE keyword is supplied and non-zero, 
;              then nearest neighbor sampling is used in both cases.   Keyword
;              has no effect when output size is not a multiple of input size.
;
;    /TOTAL - If set then the output image will have the same total number of counts
;             as the input image.     Because HREBIN also updates the astrometry,
;             use of the TOTAL keyword also preserves counts per surface area, e.g.
;             counts/(arc sec)@    
;
; OPTIONAL KEYWORD OUTPUT:
;       ERRMSG - If this keyword is supplied, then any error mesasges will be
;               returned to the user in this parameter rather than depending on
;               on the MESSAGE routine in IDL.   If no errors are encountered
;               then a null string is returned.               
; PROCEDURE:
;     The parameters BSCALE, NAXIS1, NAXIS2, CRPIX1, and CRPIX2 and the CD 
;     (or CDELT) parameters are updated for the new FITS header.
;
; EXAMPLE:
;     Compress a 2048 x 2048 image array IM, with FITS header HDR, to a 
;     724 x 724 array.   Overwrite the input variables with the compressed 
;     image and header.
;
;     IDL> hrebin, im, hdr, OUT = [724, 724]
;
; PROCEDURES USED:
;     CHECK_FITS, EXTAST, FREBIN, GSSS_STDAST, STRN(), SXPAR(), SXADDHIST, 
;     SXADDPAR, ZPARCHECK
;
; MODIFICATION HISTORY:
;     Written, December 1990  W. Landsman, ST System Corp.
;     Update CD1_1 keywords   W. Landsman   November 1992
;     Check for a GSSS header   W. Landsman  June 1994
;     Update BSCALE even if no astrometry present   W. Landsman  May 1997
;     Converted to IDL V5.0   W. Landsman   September 1997
;     Use FREBIN to accept sizes that are not a integer multiple of the original
;         size    W. Landsman     August 1998
;     Correct for "edge" effects when expanding with REBIN W. Landsman Apr. 1999
;     Fixed initialization of header only call broken in Apr 98 change May. 1999
;     Remove reference to obsolete !ERR  W. Landsman   February 2000
;     Use double precision formatting for CD matrix W. Landsman April 2000
;     Recognize PC00n00m astrometry format   W. Landsman   December 2001
;     Correct astrometry for integral contraction W. Landsman  April 2002
;     Fix output astrometry for non-equal plate scales for PC matrix or
;     CROTA2 keyword, added ALT keyword.   W. Landsman May 2005
;     Update distortion parameters if present  W. Landsman August 2007
;     Don't update BSCALE/BZERO for unsigned integer W.Landsman Mar 2008
;     Use post-V6.0 notation   W. Landsman  Nov 2011
;     Write CRPIX values as double precision if necessary W. Landsman Oct. 2012
;     Always call FREBIN, added TOTAL keyword W. Landsman Nov 2015
;- 
 On_error,2
 compile_opt idl2

 npar = N_params()      ;Check # of parameters
 if (npar EQ 3) || (npar EQ 5) || (npar EQ 0) then begin
     print,'Syntax - HREBIN, oldim, oldhd,[ newim, newhd, OUTSIZE=, ' + $
                           '/SAMPLE, ERRMSG= ]'
     return
 endif

 if ~keyword_set(SAMPLE) then sample = 0
 save_err = arg_present(errmsg)      ;Does user want to return error messages?

; If only 1 parameter is supplied, then assume it is a FITS header

 if ( npar EQ 1 ) then begin           

        zparcheck, 'HREBIN', oldim, 1, 7, 1, 'Image header'
        oldhd = oldim
        xsize = sxpar( oldhd,'NAXIS1' )
        ysize = sxpar( oldhd,'NAXIS2' )

 endif else begin 

     check_FITS, oldim, oldhd, dimen, /NOTYPE, ERRMSG = errmsg
     if errmsg NE '' then begin
        if ~save_err then message,'ERROR - ' + errmsg,/CON
        return
     endif
     if N_elements(dimen) NE 2 then begin 
           errmsg = 'Input image array must be 2-dimensional'
           if ~save_err then message,'ERROR - ' + errmsg,/CON
           return
     endif
      xsize = dimen[0]  &  ysize = dimen[1]
 endelse
 tname = size(oldim,/tname)
 
 if ( npar LT 6 ) then begin

    if ( N_elements(OUTSIZE) NE 2 ) then begin
    tit = !MSG_PREFIX + 'HREBIN: '
    print, tit, 'Original array size is '+ strn(xsize) + ' by ' + strn(ysize)
    read, tit + 'Enter size of new image in the X direction: ',newx
    read, tit + 'Enter size of new image in the Y direction: ',newy
  endif else begin
     newx = outsize[0]
     newy = outsize[1]
   endelse 
 
 endif

;  Modified Nov 2015 to always call FREBIN.     FREBIN() will call the IDL REBIN()
;  function if we are changing dimensions by an exact multiple.

 if npar GT 1 then begin
 
   if npar GT 2 then newim = frebin( oldim, newx, newy,total=total) $
                else oldim = frebin( oldim, newx, newy,total=total) 
   endif


 if ( sample GT 0 ) then type = ' Nearest Neighbor Approximation' else begin
          if ( newx LT xsize ) then type = ' Box Averaging' else $
                                    type = ' Bilinear Interpolation'
 endelse

 newhd = oldhd
 sxaddpar, newhd, 'NAXIS1', fix(newx)
 sxaddpar, newhd, 'NAXIS2', fix(newy)
 label = 'HREBIN: '+ strmid( systime(),4,20 )
 sxaddpar,newhd,'history',label + ' Original Image Size Was '+ $
         strn(xsize) +' by ' +  strn(ysize) 
 if ( npar GT 1 ) then sxaddpar,newhd,'history',label+type
 
 xratio = float(newx) / xsize   ;Expansion or contraction in X
 yratio = float(newy) / ysize   ;Expansion or contraction in Y
 lambda = yratio/xratio         ;Measures change in aspect ratio.
 pix_ratio = xratio*yratio      ;Ratio of pixel areas


; Update astrometry info if it exists

 extast, newhd, astr, noparams, ALT = alt
 if noparams GE 0 then begin

 if strmid(astr.ctype[0],5,3) EQ 'GSS' then begin
        gsss_stdast, newhd
        extast, newhd, astr, noparams
 endif


; Correct the position of the reference pixel.   Note that CRPIX values are
; given in FORTRAN (first pixel is (1,1)) convention

 crpix = astr.crpix

; When expanding with REBIN with bilinear interpolation (SAMPLE = 0), edge
; effects are introduced, which require a different calculation of the updated
; CRPIX1 and CRPIX2 values.

exact = (~(xsize mod newx) || ~(newx mod xsize)) &&  $
        (~(ysize mod newy) || ~(newy mod ysize)) 
 if (exact) && (~keyword_set(SAMPLE)) && (xratio GT 1) then $
      crpix1 = (crpix[0]-1.0)*xratio + 1.0                  else $
      crpix1 = (crpix[0]-0.5)*xratio + 0.5

 if (exact) && (~keyword_set(SAMPLE)) && (yratio GT 1) then $
      crpix2 = (crpix[1]-1.0)*yratio + 1.0                  else $
      crpix2 = (crpix[1]-0.5)*yratio + 0.5

 if N_elements(alt) EQ 0 then alt = ''
 sxaddpar, newhd, 'CRPIX1' + alt, crpix1
 sxaddpar, newhd, 'CRPIX2' + alt, crpix2
 
  if tag_exist(astr,'DISTORT') then begin
         distort = astr.distort
	 message,'Updating SIP distortion parameters',/INF
         update_distort,distort, [1./xratio,0],[1./yratio,0]
	 astr.distort= distort
	 add_distort, newhd, astr
   endif	 



; Scale either the CDELT parameters or the CD1_1 parameters.

 if (noparams NE 2) then begin 

    cdelt = astr.cdelt
    sxaddpar, newhd, 'CDELT1' + alt, CDELT[0]/xratio
    sxaddpar, newhd, 'CDELT2' + alt, CDELT[1]/yratio
; Adjust the PC matrix if aspect ratio has changed.   See equation 187 in 
; Calabretta & Greisen (2002)
    if lambda NE 1.0 then begin
        cd = astr.cd
	if noparams EQ 1 then begin
;Can no longer use the simple CROTA2 convention, change to PC keywords
	 sxaddpar,newhd,'PC1_1'+alt, cd[0,0]
	 sxaddpar, newhd,'PC2_2'+alt, cd[1,1]
     sxdelpar, newhd, ['CROTA2','CROTA1']
        endif	
        sxaddpar, newhd, 'PC1_2'+alt, cd[0,1]/lambda
        sxaddpar, newhd, 'PC2_1'+alt, cd[1,0]*lambda
   endif	

 endif else begin     ;CDn_m Matrix format

    cd = astr.cd
    sxaddpar, newhd, 'CD1_1'+alt, cd[0,0]/xratio
    sxaddpar, newhd, 'CD1_2'+alt, cd[0,1]/yratio
    sxaddpar, newhd, 'CD2_1'+alt, cd[1,0]/xratio
    sxaddpar, newhd, 'CD2_2'+alt, cd[1,1]/yratio

 endelse
 endif

; Adjust BZERO and BSCALE for new pixel size, unless these values are used
; to define unsigned integer data types.  

 if ~keyword_set(TOTAL) then begin
 bscale = sxpar( oldhd, 'BSCALE')
 bzero = sxpar( oldhd, 'BZERO')
 unsgn = (tname EQ 'UINT') || (tname EQ 'ULONG') 

 if ~unsgn then begin 
 if (bscale NE 0) && (bscale NE 1) then $
    sxaddpar, newhd, 'BSCALE', bscale/pix_ratio, 'Calibration Factor'
 if (bzero NE 0) then sxaddpar, newhd, 'BZERO', bzero/pix_ratio, $
       ' Additive Constant for Calibration'
 endif 
 endif
 
  pixelsiz = sxpar( oldhd,'PIXELSIZ' , Count = N_pixelsiz)
 if N_pixelsiz GT 0 then sxaddpar, newhd, 'PIXELSIZ', pixelsiz/xratio

 if npar EQ 2 then oldhd = newhd else $
    if npar EQ 1 then oldim = newhd

 return
 end
