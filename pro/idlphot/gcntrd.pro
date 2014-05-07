pro gcntrd,img,x,y,xcen,ycen,fwhm, maxgood = maxgood, keepcenter=keepcenter, $
                SILENT = silent, DEBUG = debug

;+
;  NAME: 
;       GCNTRD
;  PURPOSE:
;       Compute the stellar centroid by Gaussian fits to marginal X,Y, sums 
; EXPLANATION:
;       GCNTRD uses the DAOPHOT "FIND" centroid algorithm by fitting Gaussians
;       to the marginal X,Y distributions.     User can specify bad pixels 
;       (either by using the MAXGOOD keyword or setting them to NaN) to be
;       ignored in the fit.    Pixel values are weighted toward the center to
;       avoid contamination by neighboring stars. 
;
;  CALLING SEQUENCE: 
;       GCNTRD, img, x, y, xcen, ycen, [ fwhm , /SILENT, /DEBUG, MAXGOOD = ,
;                            /KEEPCENTER ]
;
;  INPUTS:     
;       IMG - Two dimensional image array
;       X,Y - Scalar or vector integers giving approximate stellar center
;
;  OPTIONAL INPUT:
;       FWHM - floating scalar; Centroid is computed using a box of half
;               width equal to 1.5 sigma = 0.637* FWHM.  GCNTRD will prompt
;               for FWHM if not supplied
;
;  OUTPUTS:   
;       XCEN - the computed X centroid position, same number of points as X
;       YCEN - computed Y centroid position, same number of points as Y
;
;       Values for XCEN and YCEN will not be computed if the computed
;       centroid falls outside of the box, or if there are too many bad pixels,
;       or if the best-fit Gaussian has a negative height.   If the centroid 
;       cannot be computed, then a  message is displayed (unless /SILENT is 
;       set) and XCEN and YCEN are set to -1.
;
;  OPTIONAL OUTPUT KEYWORDS:
;       MAXGOOD=  Only pixels with values less than MAXGOOD are used to in
;               Gaussian fits to determine the centroid.    For non-integer
;               data, one can also flag bad pixels using NaN values.
;       /SILENT - Normally GCNTRD prints an error message if it is unable
;               to compute the centroid.   Set /SILENT to suppress this.
;       /DEBUG - If this keyword is set, then GCNTRD will display the subarray
;               it is using to compute the centroid.
;       /KeepCenter  By default, GCNTRD first convolves a small region around 
;              the supplied position with a lowered Gaussian filter, and then 
;              finds the maximum pixel in a box centered on the input X,Y 
;              coordinates, and then extracts a new box about this maximum 
;              pixel.   Set the /KeepCenter keyword  to skip the convolution 
;              and finding the maximum pixel, and instead use a box 
;              centered on the input X,Y coordinates.                          
;  PROCEDURE: 
;       Unless /KEEPCENTER is set, a small area around the initial X,Y is 
;       convolved with a Gaussian kernel, and the maximum pixel is found.
;       This pixel is used as the  center of a square, within 
;       which the centroid is computed as the Gaussian least-squares fit
;       to the  marginal sums in the X and Y directions. 
;
;  EXAMPLE:
;       Find the centroid of a star in an image im, with approximate center
;       631, 48.    Assume that bad (saturated) pixels have a value of 4096 or
;       or higher, and that the approximate FWHM is 3 pixels.
;
;       IDL> GCNTRD, IM, 631, 48, XCEN, YCEN, 3, MAXGOOD = 4096       
;  MODIFICATION HISTORY:
;       Written June 2004, W. Landsman  following algorithm used by P. Stetson 
;             in DAOPHOT2.
;       Modified centroid computation (as in IRAF/DAOFIND) to allow shifts of
;      more than 1 pixel from initial guess.    March 2008
;      First perform Gaussian convolution prior to finding maximum pixel 
;      to smooth out noise  W. Landsman  Jan 2009
;-      
 On_error,2 
 compile_opt idl2

 if N_params() LT 5 then begin
        print,'Syntax: GCNTRD, img, x, y, xcen, ycen, [ fwhm, ' 
        print,'              /KEEPCENTER, /SILENT, /DEBUG, MAXGOOD= ]'
        PRINT,'img - Input image array'
        PRINT,'x,y - Input scalar integers giving approximate X,Y position'
        PRINT,'xcen,ycen - Output scalars giving centroided X,Y position'
        return
 endif else if N_elements(fwhm) NE 1 then $
      read,'Enter approximate FWHM of image in pixels: ',fwhm


 sz_image = size(img)
 if sz_image[0] NE 2 then message, $
   'ERROR - Image array (first parameter) must be 2 dimensional'

 xsize = sz_image[1]
 ysize = sz_image[2]
 dtype = sz_image[3]
 npts = N_elements(x) 
 maxbox = 13
 radius = 0.637*FWHM > 2.001             ;Radius is 1.5 sigma
 radsq = radius^2
 sigsq = ( fwhm/2.35482 )^2
 nhalf = fix(radius) < (maxbox-1)/2   	;
 nbox = 2*nhalf +1 	;# of pixels in side of convolution box 

 xcen = x*0. - 1 & ycen = y*0 - 1.
 ix = round(x)          ;Central X pixel        
 iy = round(y)          ;Central Y pixel

;Create the Gaussian convolution kernel in variable "g"
 mask = bytarr( nbox, nbox )   ;Mask identifies valid pixels in convolution box 
  g = fltarr( nbox, nbox )      
 row2 = (findgen(Nbox)-nhalf)^2
 g[0,nhalf] = row2
  for i = 1, nhalf do begin
	temp = row2 + i^2
	g[0,nhalf-i] = temp         
        g[0,nhalf+i] = temp
 endfor
 mask = fix(g LE radsq)
 good = where( mask, pixels)  ;Value of c are now equal to distance to center
   g = exp(-0.5*g/sigsq)	;Make g into a Gaussian kernel

; In fitting Gaussians to the marginal sums, pixels will arbitrarily be 
; assigned weights ranging from unity at the corners of the box to 
; NHALF^2 at the center (e.g. if NBOX = 5 or 7, the weights will be
;
;                                 1   2   3   4   3   2   1
;      1   2   3   2   1          2   4   6   8   6   4   2
;      2   4   6   4   2          3   6   9  12   9   6   3
;      3   6   9   6   3          4   8  12  16  12   8   4
;      2   4   6   4   2          3   6   9  12   9   6   3
;      1   2   3   2   1          2   4   6   8   6   4   2
;                                 1   2   3   4   3   2   1
;
; respectively).  This is done to desensitize the derived parameters to 
; possible neighboring, brighter stars.


 x_wt = fltarr(nbox,nbox)
 wt = nhalf - abs(findgen(nbox)-nhalf ) + 1
 for i=0,nbox-1 do x_wt[0,i] = wt
 y_wt = transpose(x_wt) 
 pos = strtrim(x,2) + ' ' + strtrim(y,2)

if ~keyword_set(Keepcenter) then begin 
; Precompute convolution kernel
 c = g*mask          ;Convolution kernel now in c      
 sumc = total(c)
 sumcsq = total(c^2) - sumc^2/pixels
 sumc = sumc/pixels
 c[good] = (c[good] - sumc)/sumcsq
endif

 for i = 0,npts-1 do begin        ;Loop over number of points to centroid

 if ~keyword_set(keepcenter) then begin
 if ( (ix[i] LT nhalf) || ((ix[i] + nhalf) GT xsize-1) || $
      (iy[i] LT nhalf) || ((iy[i] + nhalf) GT ysize-1) ) then begin
     if ~keyword_set(SILENT) then message,/INF, $
           'Position '+ pos[i] + ' too near edge of image'
     goto, DONE
 endif
 x1 = (ix[i]-nbox) > 0 
 x2 = (ix[i] + nbox) < (xsize-1)
 y1 = (iy[i]-nbox)  > 0
 y2 = (iy[i] + nbox) < (ysize-1)  
 h = img[x1:x2, y1:y2]
 h = convol(float(h), c)
 h= h[ nbox-nhalf: nbox + nhalf, nbox -nhalf: nbox + nhalf]
 d= img[ix[i]-nhalf: ix[i]+nhalf, iy[i]-nhalf:iy[i]+nhalf]

 if N_elements(maxgood) GT 0 then begin
     ig = where(d lt maxgood, Ng)
     mx = max(d[ig],/nan)
 endif
 mx = max( h,/nan)     ;Maximum pixel value in BIGBOX

 mx_pos = where(h EQ mx, Nmax) ;How many pixels have maximum value?
 idx = mx_pos mod nbox          ;X coordinate of Max pixel
 idy = mx_pos / nbox          ;Y coordinate of Max pixel
 if NMax GT 1 then begin        ;More than 1 pixel at maximum?
     idx = round(total(idx)/Nmax)
     idy = round(total(idy)/Nmax)
 endif else begin
     idx = idx[0]
     idy = idy[0]
 endelse
  xmax = ix[i] - (nhalf) + idx    ;X coordinate in original image array
  ymax = iy[i] - (nhalf) + idy    ;Y coordinate in original image array
  endif else begin
    xmax = ix[i]
    ymax = iy[i]
 endelse

; ---------------------------------------------------------------------
; check *new* center location for range
; added by Hogg

 if ( (xmax LT nhalf) || ((xmax + nhalf) GT xsize-1) || $
      (ymax LT nhalf) || ((ymax + nhalf) GT ysize-1) ) then begin
     if ~keyword_set(SILENT) then message,/INF, $
           'Position '+ pos[i] + ' moved too near edge of image'
     xcen[i] = -1   & ycen[i] = -1
     goto, DONE
 endif
; ---------------------------------------------------------------------

;  Extract  subimage centered on maximum pixel 

 d = img[xmax-nhalf : xmax+nhalf, ymax-nhalf : ymax+nhalf]
 

 if keyword_set(DEBUG) then begin
       message,'Subarray used to compute centroid:',/inf
       imlist,img,xmax,ymax,dx = nbox,dy=nbox
 endif  

 if N_elements(maxgood) GT 0 then $ 
           mask = (d lt maxgood) else $
   if (dtype eq 4) || (dtype EQ 5) then mask = finite(d) else $ 
           mask = replicate(1b, nbox, nbox)
  maskx = total(mask,2) GT 0
  masky = total(mask,1) GT 0

; At least 3 points are needed in the partial sum to compute the Gaussian

  if (total(maskx) LT 3) || (total(masky) LT 3) then begin
  if ~keyword_set(SILENT) then message,/INF, $
	'Position '+ pos[i] + ' has insufficient good points'
	 goto, DONE
  endif
  
  ywt = y_wt*mask
  xwt = x_wt*mask
  wt1 = wt*maskx
  wt2 = wt*masky
  
; Centroid computation:   The centroid computation was modified in Mar 2008 and
; now differs from DAOPHOT which multiplies the correction dx by 1/(1+abs(dx)). 
; The DAOPHOT method is more robust (e.g. two different sources will not merge)
; especially in a package where the centroid will be subsequently be 
; redetermined using PSF fitting.   However, it is less accurate, and introduces
; biases in the centroid histogram.   The change here is the same made in the 
; IRAF DAOFIND routine (see 
; http://iraf.net/article.php?story=7211&query=daofind )

 sd = total(d*ywt,2,/nan)
 sg = total(g*ywt,2)
 sumg = total(wt1*sg)
 sumgsq = total(wt1*sg*sg)
 
 sumgd = total(wt1*sg*sd)
 sumgx = total(wt1*sg)
 sumd = total(wt1*sd)
 p = total(wt1)
 xvec = nhalf - findgen(nbox) 
 dgdx = sg*xvec
 sdgdxs = total(wt1*dgdx^2)
 sdgdx = total(wt1*dgdx) 
 sddgdx = total(wt1*sd*dgdx)
 sgdgdx = total(wt1*sg*dgdx)

 hx = (sumgd - sumg*sumd/p) / (sumgsq - sumg^2/p)

; HX is the height of the best-fitting marginal Gaussian.   If this is not
; positive then the centroid does not make sense 

  if (hx LE 0) then begin
  if ~keyword_set(SILENT) then message,/INF, $
	'Position '+ pos[i] + ' cannot be fit by a Gaussian'
	 xcen[i] = -1	& ycen[i] = -1
	 goto, DONE
  endif

 skylvl = (sumd - hx*sumg)/p
 dx = (sgdgdx - (sddgdx-sdgdx*(hx*sumg + skylvl*p)))/(hx*sdgdxs/sigsq)
  if (abs(dx) GE nhalf) then begin
  if ~keyword_set(SILENT) then message,/INF, $
	'Position '+ pos[i] + ' is too far from initial guess'
	 goto, DONE
  endif


 
 xcen[i] = xmax + dx    ;X centroid in original array


;Now repeat computation for Y centroid

 sd = total(d*xwt,1,/nan)
 sg = total(g*xwt,1)
 sumg = total(wt2*sg)
 sumgsq = total(wt2*sg*sg)
 
 sumgd = total(wt2*sg*sd)
 sumd = total(wt2*sd)
 p = total(wt2)

 yvec = nhalf - findgen(nbox) 
 dgdy = sg*yvec
 sdgdys = total(wt2*dgdy^2)
 sdgdy = total(wt2*dgdy) 
 sddgdy = total(wt2*sd*dgdy)
 sgdgdy = total(wt2*sg*dgdy)

 hy = (sumgd - sumg*sumd/p) / (sumgsq - sumg^2/p)

  if (hy LE 0) then begin
  if ~keyword_set(SILENT) then message,/INF, $
	'Position '+ pos[i] + ' cannot be fit by a Gaussian'
	 goto, DONE
  endif

 skylvl = (sumd - hy*sumg)/p
 dy = (sgdgdy - (sddgdy-sdgdy*(hy*sumg + skylvl*p)))/(hy*sdgdys/sigsq)
  if (abs(dy) GE nhalf) then begin
  if ~keyword_set(SILENT) then message,/INF, $
	'Position '+ pos[i] + ' is too far from initial guess'
	 goto, DONE
  endif
 ycen[i] = ymax + dy    ;Y centroid in original array
DONE:

 endfor

return
end
