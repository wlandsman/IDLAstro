function frebin,image,nsout,nlout,total=total
;+
; NAME:
;   FREBIN
;
; PURPOSE:
;   Shrink or expand the size of an array an arbitrary amount using interpolation
;
; EXPLANATION: 
;   FREBIN is an alternative to CONGRID or REBIN.    Like CONGRID it
;   allows expansion or contraction by an arbitrary amount. ( REBIN requires 
;   integral factors of the original image size.)    Like REBIN it conserves 
;   flux by ensuring that each input pixel is equally represented in the output
;   array.       
;
; CALLING SEQUENCE:
;   result = FREBIN( image, nsout, nlout, [ /TOTAL] )
;
; INPUTS:
;    image - input image, 1-d or 2-d numeric array
;    nsout - number of samples in the output image, numeric scalar
;
; OPTIONAL INPUT:
;    nlout - number of lines in the output image, numeric scalar
;            If not supplied, then set equal to 1
;
; OPTIONAL KEYWORD INPUTS:
;   /total - if set, the output pixels will be the sum of pixels within
;          the appropriate box of the input image.  Otherwise they will
;          be the average.    Use of the /TOTAL keyword conserves total counts.
; 
; OUTPUTS:
;    The resized image is returned as the function result.    If the input
;    image is of type DOUBLE or FLOAT then the resized image is of the same
;    type.     If the input image is BYTE, INTEGER or LONG then the output
;    image is usually of type FLOAT.   The one exception is expansion by
;    integral amount (pixel duplication), when the output image is the same
;    type as the input image.  
;     
; EXAMPLE:
;     Suppose one has an 800 x 800 image array, im, that must be expanded to
;     a size 850 x 900 while conserving the total counts:
;
;     IDL> im1 = frebin(im,850,900,/total) 
;
;     im1 will be a 850 x 900 array, and total(im1) = total(im)
; NOTES:
;    If the input image sizes are a multiple of the output image sizes
;    then FREBIN is equivalent to the IDL REBIN function for compression,
;    and simple pixel duplication on expansion.
;
;    If the number of output pixels are not integers, the output image
;    size will be truncated to an integer.  The platescale, however, will
;    reflect the non-integer number of pixels.  For example, if you want to
;    bin a 100 x 100 integer image such that each output pixel is 3.1
;    input pixels in each direction use:
;           n = 100/3.1   ; 32.2581
;          image_out = frebin(image,n,n)
;
;     The output image will be 32 x 32 and a small portion at the trailing
;     edges of the input image will be ignored.
; 
; PROCEDURE CALLS:
;    None.
; HISTORY:
;    Adapted from May 1998 STIS  version, written D. Lindler, ACC
;    Added /NOZERO, use INTERPOLATE instead of CONGRID, June 98 W. Landsman  
;    Fixed for nsout non-integral but a multiple of image size  Aug 98 D.Lindler
;    DJL, Oct 20, 1998, Modified to work for floating point image sizes when
;		expanding the image. 
;    Improve speed by addressing arrays in memory order W.Landsman Dec/Jan 2001
;-
;----------------------------------------------------------------------------
      On_error,2
      compile_opt idl2

      if N_params() LT 1 then begin
           print,'Syntax = newimage = FREBIN(image, nsout, nlout, [/TOTAL])'  
           return,-1
       endif

       if n_elements(nlout) eq 0 then nlout=1
;
; determine size of input image
;
	ns = n_elements(image[*,0])
	nl = n_elements(image)/ns
;
; determine if we can use the standard rebin function
;
        dtype = size(image,/TNAME)
	if dtype EQ 'DOUBLE' then begin
		sbox = ns/double(nsout) 
		lbox = nl/double(nlout)
	   end else begin
		sbox = ns/float(nsout) 
		lbox = nl/float(nlout)
	end	

; Contraction by an integral amount 

	if (nsout eq long(nsout)) && (nlout eq long(nlout)) then begin
	if ((ns mod nsout) EQ 0) && ((nl mod nlout) EQ 0) then $
                if (dtype EQ 'DOUBLE') || (dtype EQ 'FLOAT') then begin
 		   if keyword_set(total) then $
		   return,rebin(image,nsout,nlout)*sbox*lbox else $
		   return,rebin(image,nsout,nlout) 
                endif else begin 
 		   if keyword_set(total) then $
		   return,rebin(float(image),nsout,nlout)*sbox*lbox else $
		   return,rebin(float(image),nsout,nlout)
                endelse 


; Expansion by an integral amount
	if ((nsout mod ns) EQ 0) && ((nlout mod nl) EQ 0) then begin
                xindex = long(lindgen(nsout)/(nsout/ns))
                if nl EQ 1 then begin
 		if keyword_set(total) then $
		return,interpolate(image,xindex)*sbox else $        
		return,interpolate(image,xindex)  
                endif
                yindex = long(lindgen(nlout)/(nlout/nl))
 		if keyword_set(total) then $
		return,interpolate(image,xindex,yindex,/grid)*sbox*lbox else $
		return,interpolate(image,xindex,yindex,/grid)  
	endif
   endif
	    ns1 = ns-1
	    nl1 = nl-1

; Do 1-d case separately

  if nl EQ 1 then begin
           if dtype eq 'DOUBLE' then result = dblarr(nsout,/NOZERO) $
			        else result = fltarr(nsout,/NOZERO)
	    for i=0L,nsout-1 do begin
	    	    rstart = i*sbox	       ;starting position for each box
	    	    istart = long(rstart)
	    	    rstop = rstart + sbox      ;ending position for each box
	    	    istop = long(rstop)<ns1
	    	    frac1 = rstart-istart
	    	    frac2 = 1.0 - (rstop-istop)
;
; add pixel values from istart to istop and  subtract fraction pixel 
; from istart to rstart and fraction pixel from rstop to istop
;
	   	     result[i] = total(image[istart:istop]) $
	   			- frac1 * image[istart]  $
	   			- frac2 * image[istop] 
	    endfor
 	    if keyword_set(total) then return,result $
	    			  else return,temporary(result)/(sbox*lbox)
 endif 

; Now do 2-d case
; First, bin in second dimension
;
	    if dtype eq 'DOUBLE' then temp = dblarr(ns,nlout, /NOZERO) $
			         else temp = fltarr(ns,nlout, /NOZERO)

; loop on output image lines
;
	    for i=0L,nlout-1 do begin
	    	    rstart = i*lbox		;starting position for each box
	    	    istart = long(rstart)
	    	    rstop = rstart + lbox	;ending position for each box
	    	    istop = long(rstop)<nl1
	    	    frac1 = rstart-istart
	    	    frac2 = 1.0 - (rstop-istop)
;
; add pixel values from istart to istop and  subtract fraction pixel 
; from istart to rstart and fraction pixel from rstop to istop
;

                     if istart EQ istop then $
	   	       temp[0,i] = (1.0 - frac1 - frac2)*image[*,istart] $
                       else $
	   	       temp[0,i] = total(image[*,istart:istop],2) $
	   			- frac1 * image[*,istart]  $
	   			- frac2 * image[*,istop] 
	    endfor
           temp = transpose(temp)
;
; bin in first dimension
;
	    if dtype eq 'DOUBLE' then result = dblarr(nlout,nsout,/NOZERO) $
			         else result = fltarr(nlout,nsout,/NOZERO)

;
; loop on output image samples
;
	    for i=0L,nsout-1 do begin
	    	    rstart = i*sbox	       ;starting position for each box
	    	    istart = long(rstart)
	    	    rstop = rstart + sbox      ;ending position for each box
	    	    istop = long(rstop)<ns1
	    	    frac1 = rstart-istart
	    	    frac2 = 1.0 - (rstop-istop)
;
; add pixel values from istart to istop and  subtract fraction pixel 
; from istart to rstart and fraction pixel from rstop to istop
;

		    if istart eq istop then $
                        result[0,i] = (1.-frac1-frac2)*temp[*,istart] else $
		    	result[0,i] = total(temp[*,istart:istop],2)   $
		    		- frac1 * temp[*,istart]  $
		    		- frac2 * temp[*,istop]
	    end

;            
	    if keyword_set(total) then $
                        return, transpose(result) $
	    	   else return, transpose(result)/(sbox*lbox)
	    			  
end
