PRO XMEDSKY, Image, Bkg, CLIP=clip, Nsig = nsig
;+
; NAME:
;       XMEDSKY
;
; PURPOSE:
;       Subtract sky from an image as a 1-D function of X
; EXPLANATION:
;       This procedure is designed to remove the sky from slitless spectra.
;       The sky is assumed to vary with wavelength (along a row) but not with
;       position (along a column).    The sky is computed as the 
;       column-by-column median of pixels within 3 sigma of the image global 
;       median.   This procedure is called by the cosmic ray rejection routine
;       CR_REJECT
;
; CALLING SEQUENCE:
;       XMEDSKY, Image, Bkg, [ CLIP=[x0, x1, y0, y1], NSIG= ]
;
; INPUTS:
;       Image:  Input image for which sky vector is to be computed.
;       
; INPUT KEYWORD PARAMETERS:
;       CLIP:   [x0, x1, y0, y1]: region of image to be used for all
;               statistical computations.    Default is to use the entire
;               image.   For STIS 1024 x 512 slitless spectra, the suggested
;               value is CLIP = [32,1023,12,499]
;       NSIG:   Positive scalar giving the number of sigma a pixel must be above
;               the global median to be reject.   Default is 3 sigma.
; OUTPUT PARAMETER:
;       Bkg:    Vector of sky values.
;;
; MODIFICATION HISTORY:
;       Written by:     R. S. Hill, Hughes STX, 20 Oct. 1997
;       Converted to V5.0, use STDDEV()   W. Landsman   June 1998
;       Check for valid WHERE, added NSIG keyword  W. Landsman   December 2000 
;       Assume since V5.1 so always use STDDEV  W. Landsman Feb 2004 
;       Assume since V5.6 use DIMEN keyword to MEDIAN W. Landsman Jan 2008  
;-
 compile_opt idl2
 if N_params() LT 2 then begin
        print,'Syntax - Xmedsky, Image, Bkg, [CLIP = ]'
        return
 endif
 if N_elements(nsig) EQ 0 then nsig=3
 sz = size(image)
 nbkg = sz[1]
 if N_elements(clip) LT 1 then clip = [0,sz[1]-1,0,sz[2]-1 ]

  bkg = median( image, dimen=2)

 tmpimg=image
 FOR i=0,sz[2]-1 DO tmpimg[0,i] = image[*,i] - bkg

; Now get the global median and standard deviation

 totmed = median(tmpimg[clip[0]:clip[1],clip[2]:clip[3]])
 totsdv = stddev(tmpimg[clip[0]:clip[1],clip[2]:clip[3]]) 

; Create a mask array showing where pixels are more than 3 (or Nsig) sigma
; from the global median.

 mask = byte(0*image+1)
 watt = where(abs(temporary(tmpimg)-totmed) GT (nsig*totsdv), cwatt)
 if cwatt GT 0 then mask[watt] = 0

; Now recompute column by column median using only unmasked pixels within the
; clipped region.

 FOR i=0,nbkg-1 DO BEGIN
   wmi = where(mask[i,clip[2]:clip[3]], cwmi)
   if cwmi GT 0 THEN $
       bkg[i]=median( image[i,clip[2] + wmi ] )
 ENDFOR

 return
 END

