pro hrotate, oldim, oldhd, newim, newhd, direction,ERRMSG = errmsg     
;+
; NAME:
;     HROTATE
; PURPOSE:
;     Apply the IDL ROTATE function and update astrometry in a FITS header
; EXPLANATION:
;     Apply the intrinsic IDL ROTATE function to an image and update 
;     astrometry in the associated FITS header.
;
; CALLING SEQUENCE:
;     HROTATE, oldim, oldhd, newim, newhd, direction
;               or
;     HROTATE, oldim, oldhd, direction 
;                       
; INPUTS:
;     OLDIM - the original image array                             
;     OLDHD - the original FITS image header, string array
;     DIRECTION - Scalar integer (0-7) specifying rotation direction, 
;               exactly as specified by the IDL ROTATE function.
;
;        Direction  Transpose?  Rot. CCW  X1  Y1 
;       ---------------------------------------- 
;       0          No          None     X0  Y0    (no change)
;       1          No          90      -Y0  X0 
;       2          No          180     -X0 -Y0 
;       3          No          270      Y0 -X0 
;       4          Yes         None     Y0  X0 
;       5          Yes         90      -X0  Y0                   
;       6          Yes         180     -Y0 -X0 
;       7          Yes         270      X0 -Y0 
;
; OPTIONAL OUTPUTS:
;     NEWIM - the rotated image, with the same dimensions as Oldim 
;     NEWHD - header for newim containing updated astrometry info
;               If output parameters are not supplied, the program
;               will modify the input parameters OLDIM and OLDHD
;               to contain the rotated image and updated header.
;
; OPTIONAL KEYWORD OUTPUT:
;     ERRMSG - If this keyword is supplied, then any error mesasges will be
;               returned to the user in this parameter rather than depending on
;               on the MESSAGE routine in IDL.   If no errors are encountered
;               then a null string is returned.               
; EXAMPLE:
;     Rotate an image exactly 90 degrees counterclockwise and update the
;     FITS image array and header. 
;
;       IDL>  HROT, im, h, im_new, h_new, 1
;
; PROCEDURE:
;      The image array is rotated using the ROTATE function.
;      The CD (or CROTA) and CRPIX parameters, if present in the FITS header,
;      are updated for the new rotation.
;      History records are also added to the header
;
; RESTRICTIONS: 
;     Does not work Guide Star Survey (GSS) astrometry.    Use GSSS_STDAST to
;     first convert 
; PROCEDURES USED:
;     CHECK_FITS(), SXADDPAR, EXTAST
;
; MODIFICATION HISTORY:
;     Written,  Mar 1997    W. Landsman,  Hughes STX
;     Work for non-square images   W. Landsman   June 1998 Raytheon STX
;     Fix for different plate scales, and CROTA2 defined, November 1998  
;     Added ERRMSG, Use double precision formatting, W. Landsman April 2000
;     Consistent conversion between CROTA and CD matrix W. Landsman Oct 2000
;     Correct update when CROTA keyword present W. Landsman  June 2003
;     Update CDELT for AIPS-style astrometry headers M. Perrin/WL Jul 2003
;     Convert GSS astrometry to WCS W. Landsman  November 2004
;     Work even if no astrometry present, just update NAXIS* WL June 2011
;- 
 On_error,2
 npar = N_params()

 if (npar NE 3) and (npar NE 5) then begin      ;Check # of parameters
  print,'Syntax - HROTATE, oldim, oldhd, newim, newhd, direction'
  print,'                            or '
  print,'         HROTATE, oldim, oldhd, direction, {ERRMSG = ]'
  return
 endif 

 if npar EQ 3 then direction = newim
 if N_elements(direction) NE 1 then message, $
        'ERROR - Direction parameter must be an integer scalar (0-7)'
 dirpar = direction mod 8
 if dirpar LT 0 then dirpar = dirpar + 8

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

  if N_elements(dimen) NE 2 then message, $
     'ERROR - Input image array must be 2-dimensional'
  xsize = dimen[0]  &  ysize = dimen[1]
  xc = (xsize-1)/2.
  yc = (ysize-1)/2.
 
 newhd = oldhd

 if npar EQ 5 then newim = rotate(oldim, direction ) else $
                   oldim = rotate(oldim, direction )
 
 case dirpar of
   0: return
   1: rot_mat = [ [0, 1],[-1, 0] ] 
   2: rot_mat = [ [-1,0],[ 0,-1] ]
   3: rot_mat = [ [0,-1], [1, 0] ]
   4: rot_mat = [ [0, 1], [-1,0] ]
   5: rot_mat = [ [-1,0], [0, -1] ]
   6: rot_mat = [ [0,-1], [1, 0] ]
   7: rot_mat = [ [1, 0], [0, 1] ]
   else: message,$
        'ERROR - Illegal value of direction parameter, must be between 0 and 7'
   endcase

 if (xsize NE ysize) && (rot_mat[0,0] EQ 0) then begin
        sxaddpar, newhd, 'NAXIS1', ysize
        sxaddpar, newhd, 'NAXIS2', xsize
 endif

 label = 'HROTATE: ' + strmid(systime(),4,20)
 sxaddhist, label + ' Image = ROTATE(Image,' + strtrim(direction,2) + ')',newhd

; Update astrometry info if it exists.   If GSS astrometry is present, then
; convert it to standard WCS astrometry

 extast, oldhd, astr, noparams

 if noparams GE 0 then begin    ;Astrometry parameters exist in header?

 if strmid(astr.ctype[0],5,3) EQ 'GSS' then begin
        gsss_stdast, newhd
        extast, newhd, astr, noparams
 endif

; For non-square images, check if  X and Y axes have been flipped

    crpix = astr.crpix
    cd = astr.cd
    cdelt = astr.cdelt
    if cdelt[0] NE 1.0 then begin
         cd[0,0] = cd[0,0]*cdelt[0] & cd[0,1] = cd[0,1]*cdelt[0]
         cd[1,1] = cd[1,1]*cdelt[1] & cd[1,0] = cd[1,0]*cdelt[1]
     endif
       
    ncrpix =  [xc,yc] + rot_mat#(crpix-1 -[xc,yc]) + 1

    newcd =  cd # transpose(rot_mat)


    if (dirpar EQ 4) || (dirpar EQ 6) then begin
        ncrpix[0] = xsize - ( ncrpix[0] - 1)
        newcd[*,0] = -newcd[*,0]
    endif 

    if (dirpar EQ 5) || (dirpar EQ 7) then begin
        ncrpix[1] = ysize - (ncrpix[1] -1 )
        newcd[*,1] = -newcd[*,1]
    endif 

 
  if (xsize NE ysize) && (rot_mat[0,0] EQ 0) then begin
        ncrpix[0] = ncrpix[0] - xc + yc
        ncrpix[1] = ncrpix[1] - yc + xc
 endif


    sxaddpar, newhd, 'CRPIX1', ncrpix[0]
    sxaddpar, newhd, 'CRPIX2', ncrpix[1]

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

     endif else begin ; noparams = 1. CROTA+CDELT type
        crota  = atan(-newcd[1,0], newcd[1,1] )*180.0/!DPI

        if dirpar GE 4 then sxaddpar, newhd, 'CDELT1', -cdelt[0]

        sxaddpar, newhd,'CROTA1', crota
        sxaddpar, newhd,'CROTA2', crota
   endelse
      
   
 endif 

 if npar EQ 3 then oldhd = newhd                ;update old image and header
 
 return
 end
