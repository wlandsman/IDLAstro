pro hrot, oldim, oldhd, newim, newhd, angle, xc, yc, int, MISSING=missing, $
                INTERP = interp, CUBIC = cubic, PIVOT = pivot,ERRMSG= errmsg
;+
; NAME:
;       HROT
; PURPOSE:
;       Rotate an image and create new FITS header with updated astrometry.
; EXPLANATION: 
;       Cubic, bilinear or nearest neighbor interpolation can be used.
;
; CALLING SEQUENCE:
;       HROT, oldim, oldhd, [ newim, newhd, angle, xc, yc, int, 
;                       MISSING =, INTERP =, CUBIC = , /PIVOT]
; INPUTS:
;       OLDIM - the original image array                             
;       OLDHD - the original FITS image header, string array
;
; OPTIONAL INPUTS:
;       NEWIM - If NEWIM is set to -1, then the old image and header will
;               be updated
;       ANGLE - Rotation angle, degrees clockwise, scalar
;       XC    - X Center of rotation (-1 for center of image)
;       YC    - Y Center of rotation (-1 for center of image)
;       INT   - 0 for nearest neighbor, 1 for bilinear interpolation
;               2 for cubic interpolation.  
;
; OPTIONAL OUTPUTS:
;       NEWIM - the rotated image, with the same dimensions as Oldim 
;       NEWHD - header for newim containing updated astrometry info
;               If output parameters are not supplied, the program
;               will modify the input parameters OLDIM and OLDHD
;               to contain the rotated image and updated header.
;
; OPTIONAL INPUT KEYWORD:
;       MISSING - Set this keyword to a scalar value which will be assigned
;               to pixels in the output image which do not correspond to 
;               existing input images (e.g if one rotates off-center). 
;               If not supplied then linear extrapolation is used.
;               ***NOTE: A bug was introduced into the POLY_2D function in IDL
;               V5.5 (fixed in V6.1) such that the MISSING keyword
;               may not work properly with floating point data***
;
;       INTERP - scalar set to either 0 (nearest neighbor interpolation),
;               1 (bilinear interpolation), or 2 (cubic interpolation).    
;               The interpolation type can be specified by either the INTERP 
;               keyword or the int parameter
;             
;       CUBIC - If set and non-zero then cubic interpolation is used (see ROT),
;               which is equivalent to setting INT = 2.   In IDL V5.0 and later,
;                this keyword can also be set to a value between -1 and 0.
;
;       /PIVOT - Setting this keyword causes the image to pivot around the point
;		XC, YC, so that this point maps into the same point in the
;		output image.  If this keyword is set to 0 or omitted, then the
;		point XC, YC in the input image is mapped into the center of
;		the output image.
;
; OPTIONAL OUTPUT KEYWORD:
;       ERRMSG - If this keyword is supplied, then any error mesasges will be
;               returned to the user in this parameter rather than depending on
;               on the MESSAGE routine in IDL.   If no errors are encountered
;               then a null string is returned.               
; EXAMPLE:
;       Rotate an image non-interactively 30 degrees clockwise.  Use
;       bilinear interpolation, and set missing values to 0.
;
;       IDL>  HROT, im_old, h_old, im_new, h_new, 30, -1, -1, 1, MIS = 0
;
;       As above but update the input image and header and pivot about (100,120)
;
;       IDL>  HROT, im_old, h_old, -1, -1, 30, 100, 120, 1, MIS = 0, /PIVOT
; RESTRICTIONS:
;       Unlike the ROT procedure, HROT cannot be used to magnify or
;       or demagnify an image. Use HCONGRID or HREBIN instead.
;
; PROCEDURE:
;       The image array is rotated using the ROT procedure.
;       The CD (or CROTA) and CRPIX parameters, if present in the FITS header,
;       are updated for the new rotation.
;       History records are also added to the header
;
; PROCEDURES USED:
;       CHECK_FITS, EXTAST, GETOPT(), GETROT, ROT(), STRN(), SXADDPAR
;
; MODIFICATION HISTORY:
;       Written, Aug. 1986 W. Landsman, ST Systems Corp.
;       Added MISSING keyword, W. Landsman March, 1991
;       Added cubic interpolation, use astrometry structure   Feb 1994
;       Removed call to SINCE_VERSION()  W. Landsman  March 1996
;       Assume at least V3.5, add CUBIC parameter       W. Landsman  March 1997
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Fix for CROTA2 defined and CDELT1 NE CDELT2, W. Landsman  November 1998
;       Fix documentation  to specify clockwise rotation W. Landsman Dec. 1999
;       Added /PIVOT keyword    W. Landsman  January 2000
;       Added ERRMSG, Use double precision formatting, W. Landsman April 2000
;       Consistent conversion between CROTA and CD matrix W. Landsman Oct 2000
;       Work for both CD001001 and CDELT defined  W. Landsman   March 2001
;       Recognize PC matrix astrometry  W. Landsman December 2001
;       Update astrometry correctly when /PIVOT applied W. Landsman March 2002
;       Update CROTA2 astrometry correctly, approximate GSSS W.L. June 2003
;       Work with CD1_1, PC1_1 and CROTA keywords W. L. July 2003 
;       Work with angle as a 1 element vector  W.L.  May 2006
;- 
 On_error,2
 compile_opt idl2
 npar = N_params()

 if (npar LT 2) or (npar EQ 3) then begin       ;Check # of parameters
  print,'Syntax: HROT, oldim, oldhd, [ newim, newhd, angle, xc, yc, int,'
  print,'                   CUBIC =, INTERP = , MISSING = ,/PIVOT, ERRMSG= ]'
  print, 'Oldim and Oldhd will be updated if only 2 parameters supplied '
     return
 endif 

 cdr = !DPI/180.0D              ;Change degrees to radians
;                               Check that input header matches input image
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

  xsize = dimen[0]  &  ysize = dimen[1]

  xc_new = (xsize - 1)/2.
  yc_new = (ysize - 1)/2.
 if npar LT 8 then begin
  if npar EQ 2 then print,'Program will modify old image and header'
  print,'Original array size is '+ strn(xsize) + ' by ' + strn(ysize)
  read,'Angle of rotation (degrees clockwise): ',angle
  ans = '' 
  read,'Enter center (x,y) of rotation ( [RETURN] for center of image): ',ans
  center = getopt(ans,'F',2)
  if N_elements(center) EQ 1 then begin
      xc = -1 & yc = -1
  endif else begin
      xc = center[0] & yc = center[1]
  endelse
 endif

 if keyword_set( INTERP ) then int = interp
 if keyword_set( CUBIC ) then int = 2
 if N_elements(int) NE 1 then $
   read,'Enter 0 for nearest neighbor, 1 for bilinear, 2 for cubic interpolation: ',int

 case int of 
 0: type = ' Nearest Neighbor Approximation'
 1: type = ' Bilinear Interpolation' 
 2: type = ' Cubic Interpolation'
 else: message,'Illegal value of Interp parameter: must be 0,1, or 2'
 endcase

 if xc LT 0 then xc = xc_new
 if yc LT 0 then yc = yc_new

 if N_elements(newim) EQ 1 then $
   if newim EQ -1 then npar = 2                                      

 newhd = oldhd
 if N_elements(cubic) EQ 0 then cubic = (int EQ 2) 
 angle = angle[0]

  if N_elements(MISSING) NE 1 then begin

        if npar EQ 2 then begin 
               oldim = rot( oldim, angle, 1, xc,yc, $
                            CUBIC = cubic, INTERP = int, PIVOT = pivot)
        endif else begin
               newim = rot( oldim, angle, 1, xc,yc, $
                            CUBIC = cubic, INTERP = int, PIVOT = pivot)
        endelse
 
 endif else begin

        if npar EQ 2 then begin
           oldim = rot( oldim,angle,1,xc,yc, $
                CUBIC = cubic, MISSING = missing, INTERP = int, PIVOT = pivot) 
        endif else begin
           newim = rot( oldim, angle, 1, xc, yc, $
                CUBIC = cubic, MISSING = missing, INTERP = int, PIVOT = pivot)
        endelse
  endelse

 label = 'HROT:' + strmid(systime(),4,20)
 sxaddpar, newhd, 'HISTORY', label + $ 
   ' Rotated by' + string(float(angle), FORM = '(f7.2)') + ' Degrees'
 sxaddpar,newhd,'history',label+type 

; Update astrometry info if it exists

 extast, oldhd, astr, noparams
 if strmid(astr.ctype[0],5,3) EQ 'GSS' then begin
        gsss_stdast, newhd
        extast, newhd, astr, noparams
 endif


 if noparams GE 0 then begin    ;Astrometry parameters exist in header?
    crpix = astr.crpix
    cd = astr.cd
    cdelt = astr.cdelt
 
    theta = angle*cdr
    rot_mat = [ [ cos(theta), sin(theta)], $   ;Rotation matrix
                [-sin(theta), cos(theta)] ] 
    
    ncrpix =  transpose(rot_mat)#(crpix-1-[xc,yc]) + 1
    if ~keyword_set(PIVOT) then ncrpix = [xc_new,yc_new] + ncrpix $
                              else ncrpix = [xc,yc] + ncrpix
    sxaddpar, newhd, 'CRPIX1', ncrpix[0]
    sxaddpar, newhd, 'CRPIX2', ncrpix[1]
 
    newcd = cd # rot_mat

    if noparams EQ 3 then begin     ;Transformation matrix format

        sxaddpar, newhd, 'PC1_1', newcd[0,0] 
        sxaddpar, newhd, 'PC1_2', newcd[0,1] 
        sxaddpar, newhd, 'PC2_1', newcd[1,0]
        sxaddpar, newhd, 'PC2_2', newcd[1,1]
 
                                  
    endif else if noparams EQ 2 then begin

        sxaddpar, newhd, 'CD1_1', newcd[0,0]
        sxaddpar, newhd, 'CD1_2', newcd[0,1]
        sxaddpar, newhd, 'CD2_1', newcd[1,0]
        sxaddpar, newhd, 'CD2_2', newcd[1,1]

     endif else begin  
; Just need to update the CROTA keywords 
        crota  = atan( -newcd[1,0],newcd[1,1] )*180.0/!DPI
        sxaddpar, newhd,'CROTA1', crota
        sxaddpar, newhd,'CROTA2', crota
    
     endelse

 endif 

 if npar eq 2 then oldhd = newhd                ;update old image and header
 
 return
 end
