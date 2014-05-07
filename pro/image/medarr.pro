PRO medarr, inarr, outarr, mask, output_mask
;+
; NAME:
;       MEDARR
; PURPOSE:
;       Compute the median at each pixel across a set of 2-d images
; EXPLANATION:
;       Each pixel in the output array contains  the median of the 
;       corresponding pixels in the input arrays.   Useful, for example to 
;       combine a stack of CCD images, while removing cosmic ray hits.
;
;       This routine has been mostly obsolete since V5.6 with the introduction
;       of the DIMENSION keyword to the intrinsic MEDIAN() function.   However,
;       it is  still useful for integer images if bad pixels need to be flagged
;       in a mask parameter.  (For floating point images, it is much 
;       faster to set invalid pixels to NaN values.)
; CALLING SEQUENCE:
;       MEDARR, inarr, outarr, [ mask, output_mask ]
; INPUTS:
;       inarr  -- A three dimensional array [Nx,Ny, N] containing the input 
;                images.    Each image is size Nx by Ny, and there are N
;                images.    
;
; OPTIONAL INPUT:
;       mask   -- Same structure as inarr, byte array with 1b where
;                 pixels are to be included, 0b where they are to be
;                 excluded.    For floating point images, it is much faster to 
;                 set masked pixels in inarr equal to !VALUES.F_NAN (see below),
;                 rather than use the mask parameter.
;                
; OUTPUTS:
;       outarr -- The output array.  It will have dimensions equal to the
;                 first two dimensions of the input array.
;
; OPTIONAL OUPUT:
;       output_mask -- Same structure as outarr, byte array with 1b where
;                      pixels are valid, 0b where all the input pixels
;                      have been masked out.
; RESTRICTIONS:
;        This procedure is *SLOW* when using the Mask parameter because it has
;        to loop over  each pixel of the image.  
;
; EXAMPLE:
;       Suppose one wants to combine three floating point 1024 x 1024 bias 
;       frames which have been read into the IDL variables im1,im2,im3
;
;       IDL> bigim = fltarr(1024,1024,3)        ;Create big array to hold images
;       IDL> bigim[0,0,0] = im1 & bigim[0,0,1] = im2 & bigim[0,0,2] = im2  
;       IDL> medarr, bigim, avgbias
;
;       The variable avgbias will be the desired 1024x 1024 float image.
; PROCEDURE:
;       If the MASK parameter is not set, then MEDARR is just a wrapper for 
;       MEDIAN(/EVEN, dimension = 3).    If the MASK parameter is set,
;       a scalar median function over the third dimension is looped over 
;       each pixel of the first two dimensions.   The /EVEN keyword is used
;       with MEDIAN (which averages the two middle values), since this avoids 
;       biasing the output for an even number of images.
;
;       Any values set to NAN (not a number) are ignored when computing the
;       median.    If all values for a pixel location are NAN, then the median
;       is also returned as NAN.
;
; MODIFICATION HISTORY:
;       Written by Michael R. Greason, STX, 12 June 1990.
;       Don't use MEDIAN function for even number of images.
;          W. Landsman Sep 1996
;       Mask added.  RS Hill, HSTX, 13 Mar. 1997
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Use /EVEN keyword to MEDIAN    W. Landsman  September 1997
;       Rearranged code for faster execution   W. Landsman January 1998
;       Faster execution for odd number of images   W. Landsman July 2000
;       V5.4 fix for change in SIZE() definition of undefined variable 
;                W. Landsman/E. Young   May 2001
;       Use MEDIAN(/DIMEN) for V5.6 or later   W. Landsman   November 2002
;       Use keyword_set() instead of ARG_present() to test for presence of mask
;           parameter  D. Hanish/W. Landsman   June 2003
;       Assume since V5.6  W. Landsman  Feb 2004
; 
;-
 On_error,2
;                       Check parameters.

 if N_params() LT 2 then begin                  ; # parameters.
        print, "Syntax -  MEDARR, inputarr, outputarr [, maskarr, output_mask]"
        return
 endif
 
 s = size(inarr)
 if s[0] NE 3 then $                    ; Input array size.
        message, "Input array must have 3 dimensions"
 if (N_elements(mask) EQ 0) then begin
        outarr = median(inarr,dimension=3,/even)
        return
 endif

;                       Create the output array.
 ncol = s[1]
 nrow = s[2]
 narr = s[3]
 type = s[s[0] + 1]
 outarr = make_array( dimen = [ncol,nrow], /NOZERO, TYPE = type )
 if arg_present(output_mask) then $
     output_mask = make_array (dimen = [ncol,nrow], VALUE = 1b)

;                       Combine the input arrays into the output array.

  sm = size(mask)
  if N_elements(mask) LT 4 then $ 
	 message,'Input mask not valid... must have 3 dimensions'
  if array_equal(sm[0:3], s[0:3] )  then $	 
     mask_given = 1b  $
     else message,'Mask not valid... must be same shape as input cube.'

 for j = 0l, (nrow-1) do begin    
        for i = 0l, (ncol-1) do begin
                good_pixels = 1b   
                       wmask = where(mask[i,j,*],cwm)
                       if cwm gt 0 then begin
                          marr = inarr[i,j,wmask] 
                       endif else begin
                          good_pixels = 0b
                          output_mask[i,j] = 0b
                       endelse
  
                if good_pixels then outarr[i,j] = median(marr,/EVEN)
          
        endfor
 endfor
 
 return
 end
