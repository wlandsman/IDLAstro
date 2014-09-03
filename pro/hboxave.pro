pro hboxave, oldim, oldhd, newim, newhd, box, ERRMSG = errmsg   ;Boxaverage and update header
;+
; NAME:
;       HBOXAVE
; PURPOSE:
;       Box average an image array and update the FITS header array
; EXPLANATION:
;       The function BOXAVE() is used.  This procedure is recommended for 
;       integer images when photometric precision is desired, because it 
;       performs intermediate steps using REAL*4 arithmetic.   Otherwise, the 
;       procedure HREBIN is much faster.
;
; CALLING SEQUENCE:
;       HBOXAVE, Oldim, Oldhd, Newim, Hewhd, box
;               or
;       HBOXAVE, Oldim, Oldhd, box
;
; INPUTS:
;       Oldim - the original image array
;       Oldhd - the original image FITS header, string array
;
; OPTIONAL INPUTS:
;       box - the box size to be used, integer scalar.  If omitted, then
;               HBOXAVE will prompt for this parameter.
;
; OPTIONAL OUTPUTS:
;       Newim - the image after boxaveraging
;       Newhd - header for newim containing updated astrometry info
;               If output parameters are not supplied, the program
;               will modify the input parameters OLDIM and OLDHD
;               to contain the new array and updated header.
; OPTIONAL KEYWORD OUTPUT:
;       ERRMSG - If this keyword is supplied, then any error mesasges will be
;               returned to the user in this parameter rather than depending on
;               on the MESSAGE routine in IDL.   If no errors are encountered
;               then a null string is returned.               
;
; PROCEDURE:
;       The parameters BSCALE, NAXIS1, NAXIS2, CRPIX1, and CRPIX2 and
;       the CD (or CDELT) parameters are updated for the new FITS header.
;
; EXAMPLE:
;       Compress the image in a FITS file 'image.fits' by a factor of 4 and 
;       update the astrometry in the FITS header
;
;       IDL> im = readfits('image.fits',hdr)    ;Read FITS file into IDL arrays
;       IDL> hboxave, im, hdr, 4                ;Boxaverage by 4
;       IDL> writefits,'image.fits',im,hdr      ;Write a new FITS file
;
; CALLED PROCEDURES:
;       CHECK_FITS - Check that the FITS header is appropriate to the image
;       BOXAVE() - Performs box averaging of an image
;       SXPAR(), SXADDPAR - Read and write FITS keyword values
;
; MODIFICATION HISTORY:
;       Written, Aug. 1986 W. Landsman, STI Corp.
;       IDLV2 changes, sxaddpar format keyword added, J. Isensee, July,1990
;       Fix 0.5 pixel offset in new CRPIX computation W. Landsman, Dec, 1991
;       Update BSCALE even if no astrometry present   W. Landsman, May 1997
;       Added ERRMSG keyword, Use double formatting   W. Landsman   April 2000
;       Recognize PC matrix astrometry format    W. Landsman   December 2001
;       Use V6.0 notation  W. Landsman  October 2012
;- 
 On_error,2                               ;Return to caller on error

 npar = N_params()

 if ( npar LT 2 ) then begin            ;Check # of parameters
     print,'Syntax: HBOXAVE, oldim, oldhd, [ newim, newhd, box, ERRMSG = ]'
     print,'    or  HBOXAVE, oldim, oldhd, [ box, ERRMSG =  ]'
     return
 endif

 save_err = arg_present(errmsg)      ;Does user want to return error messages?
;                                    Check for valid 2-D image & header
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
 if npar EQ 3 then begin

    box = newim

 endif else if (npar NE 5) then begin    ;prompt for box size

   print,'Boxaverage an image and update header'
   print,'Original array size is '+ strn(xsize) + ' by ' + strn(ysize)
   read, 'Enter width of box to be used in box average: ',box

 endif

 box = fix(box)                      ;Check for integer type 
 if N_elements(box) NE 1 then begin
    box = 0
    read, 'Enter width of box to be used in box average: ',box
 endif

 newx = xsize/float(box)
 newy = ysize/float(box)

 if (newx*box NE xsize) || (newy*box NE ysize) then $
    message,'ERROR - Box size does not evenly divide image size'

 if npar GT 3 then newim = boxave( oldim, box) else $
                   oldim = boxave( oldim, box)

 newhd = oldhd
 sxaddpar, newhd, 'NAXIS1', fix(newx)
 sxaddpar, newhd, 'NAXIS2', fix(newy)
 label = 'HBOXAVE:' + strmid( systime(), 4, 20)
 sxaddpar, newhd, 'HISTORY', label + ' Original Image Size Was ' + $
    strn(xsize) + ' by ' +  strn(ysize) 
 sxaddpar, newhd, 'HISTORY',label+' Box Width: '+ strn(box)+' Pixels'

; Update astrometry info if it exists

 extast, oldhd, astr, noparams
 if noparams GE 0 then begin

 pix_ratio = box*box     ;Ratio of old to new pixel areas

 crpix = (astr.crpix - 0.5)/box + 0.5
 sxaddpar, newhd, 'CRPIX1', crpix[0]
 sxaddpar, newhd, 'CRPIX2', crpix[1]

 if (noparams NE 2) then begin 

    cdelt = astr.cdelt
    sxaddpar, newhd, 'CDELT1', CDELT[0]*box
    sxaddpar, newhd, 'CDELT2', CDELT[1]*box

 endif else begin       ;CDn_m Matrix

    cd = astr.cd
    sxaddpar, newhd, 'CD1_1', cd[0,0]*box
    sxaddpar, newhd, 'CD1_2', cd[0,1]*box
    sxaddpar, newhd, 'CD2_1', cd[1,0]*box
    sxaddpar, newhd, 'CD2_2', cd[1,1]*box

 endelse 
 endif
 
 bscale = sxpar( oldhd, 'BSCALE')
 if ( bscale NE 0 )  && ( bscale NE 1) then $
      sxaddpar, newhd, 'BSCALE', bscale*pix_ratio, ' CALIBRATION FACTOR'

 bzero = sxpar( oldhd, 'BZERO')
 if ( bzero NE 0) then sxaddpar, newhd, 'BZERO', bzero*pix_ratio, $
        ' ADDITIVE CONST FOR CALIB'

 if npar LT 4 then oldhd = newhd
 return
 end
